;; -*- lisp -*-

;; This file is part of hyperluminal-DB.
;; Copyright (c) 2013 Massimiliano Ghilardi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


(in-package :hyperluminal-mem)


(defmacro multiple-value-bind-chain2* ((var1 var2 &rest more-vars)
                                       (func arg1 arg2 &rest more-args) &body body)
  "Warning: this macro expands multiple references to FUNC, ARG1 and MORE-ARGS"
  (if more-vars
      (with-gensym tmp
        `(multiple-value-bind (,var1 ,tmp) (,func ,arg1 ,arg2 ,@more-args)
           (multiple-value-bind-chain2* (,var2 ,@more-vars) (,func ,arg1 ,tmp ,@more-args)
             ,@body)))
      `(multiple-value-bind (,var1 ,var2) (,func ,arg1 ,arg2 ,@more-args)
         ,@body)))


(defmacro with-mread* ((var1 var2 &rest more-vars)
                                    (ptr index end-index) &body body)
  "syntactic sugar for multiple calls to mread. Last name in MORE-VARS
will be bound to the new value of INDEX"
  (if more-vars
      (with-gensyms (ptr-var idx-var end-var)
        `(let* ((,ptr-var ,ptr)
                (,idx-var ,index)
                (,end-var ,end-index))
           (multiple-value-bind-chain2* (,var1 ,var2 ,@more-vars)
               (mread ,ptr-var ,idx-var ,end-var)
             ,@body)))
      `(multiple-value-bind (,var1 ,var2) (mread ,ptr ,index ,end-index)
         ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    dispatchers for object types                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric msize-object (object index)
  (:documentation
   "Compute and return the number of memory words needed to serialize OBJECT,
not including its header"))

(defgeneric mwrite-object (object ptr index end-index)
  (:documentation
   "Serialize OBJECT by writing it into the memory starting at (+ PTR INDEX).
Assumes OBJECT header was already written.
The available memory ends immediately before (+ PTR END-INDEX)."))

(defgeneric mread-object (type ptr index end-index &key)
  (:documentation
   "Deserialize an object of type TYPE by reading it from the memory starting at (+ PTR INDEX).
Assumes OBJECT header was already read.
The available memory ends immediately before (+ PTR END-INDEX)."))



  




(defmacro %msize* (index value &rest more-values)
  "Warning: this macro expands VALUE *before* INDEX"
  (if more-values
      (with-gensym new-index
        `(let1 ,new-index (msize ,value ,index)
           (%msize* ,new-index ,@more-values)))
      `(msize ,value ,index)))


(defmacro msize* (index value &rest more-values)
  (with-gensym old-index
    `(let1 ,old-index ,index
       (%msize* ,old-index ,value ,@more-values))))
  

(defmacro %mwrite* (ptr index end-index value &rest more-values)
  "Warning: this macro expands multiple references to PTR and END-INDEX"
  (if more-values
      (with-gensyms (new-index)
        `(let1 ,new-index (mwrite ,ptr ,index ,end-index ,value)
           (%mwrite* ,ptr ,new-index ,end-index ,@more-values)))
      `(mwrite ,ptr ,index ,end-index ,value)))


(defmacro mwrite* (ptr index end-index value &rest more-values)
  (if more-values
      (with-gensyms (ptr-var idx-var end-var)
        `(let* ((,ptr-var ,ptr)
                (,idx-var ,index)
                (,end-var ,end-index))
           (%mwrite* ,ptr-var ,idx-var ,end-var ,value ,@more-values)))
      `(mwrite ,ptr ,index ,end-index ,value)))








(defun msize-obj (object &optional (index 0))
  "Compute and return the number of memory words needed to serialize OBJECT,
including its header"
  (declare (type mem-size index))

  (incf index +mem-box/header-words+)

  (let1 index (msize (type-of object) index)
    (the (values mem-size &optional)
      (msize-object object index))))


(defun mwrite-obj (object ptr index end-index)
  "Serialize OBJECT by writing it into the memory starting at (+ PTR INDEX).
Also serializes OBJECT header.
The available memory ends immediately before (+ PTR END-INDEX)."
  (declare (type mem-size index end-index))

  ;; write OBJECT payload
  (let* ((index0 (mem-size+ index +mem-box/header-words+))
         (index1 (mwrite ptr index0 end-index (type-of object)))
         (index2 (mwrite-object object ptr index1 end-index))
         (actual-words (mem-size- index2 index)))
         
    (when (> index2 end-index)
      (error "HYPERLUMINAL-DB internal error!
wrote ~S word~P at address ~S + ~S,
but only ~S words were available at that location.
Either this is a bug in hyperluminal-db, or some object
was concurrently modified while being written"
             actual-words actual-words ptr index (mem-size- end-index index)))

    ;; write OBJECT header
    (mwrite-box/header ptr index +mem-obj/first+ (round-up-size actual-words))
    index2))


(defun mread-obj (ptr index end-index)
  "Deserialize an object of type TYPE by reading it from the memory starting at (+ PTR INDEX).
Also deserializes OBJECT header.
The available memory ends immediately before (+ PTR END-INDEX)."
  (declare (type mem-size index end-index))
  
  ;; skip BOX header
  (incf index +mem-box/header-words+)

  ;; read OBJECT type
  (multiple-value-bind (type new-index) (mread ptr index end-index)
    ;; TODO validate type against a set of trusted types
    (check-type type symbol)

    (the (values t mem-size &optional)
      (mread-object type ptr new-index end-index))))

    
(defun null-getter (obj)
  (declare (ignore obj))
  nil)

(defun null-setter (value obj)
  (declare (ignore obj))
  value)


(defstruct accessor
  (name    nil    :type symbol)
  (getter  #'null-getter :type function)
  (setter  #'null-setter :type function))


(declaim (inline msize-accessor mwrite-accessor mread-accessor))
         
(defun msize-accessor (index object accessor)
  (declare (type mem-size index)
           (type accessor accessor))
  (msize (funcall (accessor-getter accessor) object) index))


(defun mwrite-accessor (ptr index end-index object accessor)
  (declare (type mem-size index end-index)
           (type accessor accessor))
  (mwrite ptr index end-index (funcall (accessor-getter accessor) object)))


(defun mread-accessor (ptr index end-index object accessor)
  (declare (type mem-size index end-index)
           (type accessor accessor))
  (multiple-value-bind (value new-index)
      (mread ptr index end-index)
    (funcall (accessor-setter accessor) value object)
    new-index))



(defun msize-accessors (index object accessors)
  (declare (type mem-size index)
           (type simple-vector accessors))
  (loop for accessor across accessors do
       (setf index (msize-accessor index object accessor)))
  index)


(defun mwrite-accessors (ptr index end-index object accessors)
  (declare (type mem-size index end-index)
           (type simple-vector accessors))
  (loop for accessor across accessors do
       (setf index (mwrite-accessor ptr index end-index object accessor)))
  index)


(defun mread-accessors (ptr index end-index object accessors)
  (declare (type mem-size index end-index)
           (type simple-vector accessors))
  (loop for accessor across accessors do
       (setf index (mread-accessor ptr index end-index object accessor)))
  index)


(defun check-accessors (accessors)
  (declare (type simple-vector accessors))
  (loop for accessor across accessors do
       (check-type accessor accessor))
  t)


(defun check-accessor-names (accessor-names)
  (declare (type simple-vector accessor-names))
  (loop for accessor-name across accessor-names do
       (check-type accessor-name symbol))
  t)


(defun accessor-name-to-keyword (struct-name accessor-name)
  (let* ((prefix (concatenate 'string (symbol-name struct-name) "-"))
         (prefix-len (length prefix))
         (name (symbol-name accessor-name))
         (name-len   (length name)))

    (intern
     (if (and (> name-len prefix-len)
              (string= prefix (the string (subseq name 0 prefix-len))))
         (subseq name prefix-len)
         name)
     :keyword)))


(defun check-decl-mserializable-struct-args (accessor-names accessor-names-p mfunc mfunc-name)
  
  (when (eq (null accessor-names-p) (null mfunc))
    (error "error while invoking ~S~S~S:~%  exactly one of ~S or ~S~S must be specified"
           'decl- mfunc-name '-struct 'accessor-names mfunc-name '-struct))
       
  (check-accessor-names accessor-names))


(defun make-make-struct (struct-name)
  (declare (type symbol struct-name))
  (let ((pkg (symbol-package struct-name))
        (name (symbol-name struct-name)))
    (intern
     (concatenate 'string (symbol-name 'make-) name)
     pkg)))
    



(defmacro decl-msize-struct (struct-name &key (accessor-names #() accessor-names-p)
                                           msize-struct)
  "shortcut for (defmethod msize-object (...))"
  (declare (type simple-vector accessor-names)
           (type symbol msize-struct))

  (check-decl-mserializable-struct-args accessor-names accessor-names-p
                                        msize-struct 'msize)
  
  (with-gensyms (obj index)
    `(defmethod msize-object ((,obj ,struct-name) ,index)
       (declare (type mem-size ,index))
     
       ,(if accessor-names-p
            `(msize* ,index ,@(loop for accessor-name across accessor-names
                                 collect `(,accessor-name ,obj)))

            `(,msize-struct ,obj ,index)))))




(defmacro decl-mwrite-struct (struct-name &key (accessor-names #() accessor-names-p)
                                            mwrite-struct)
  "shortcut for (defmethod mwrite-object (...))"
  (declare (type simple-vector accessor-names)
           (type symbol mwrite-struct))

  (check-decl-mserializable-struct-args accessor-names accessor-names-p
                                        mwrite-struct 'mwrite)
    
  (with-gensyms (obj ptr index end-index)
    `(defmethod mwrite-object ((,obj ,struct-name) ,ptr ,index ,end-index)
       (declare (type mem-size ,index ,end-index))
     
       ,(if accessor-names-p
            `(mwrite* ,ptr ,index ,end-index
                      ,@(loop for accessor-name across accessor-names
                           collect `(,accessor-name ,obj)))

            `(,mwrite-struct ,obj ,index)))))



(defmacro decl-mread-struct (struct-name &key (accessor-names #() accessor-names-p)
                                           mread-struct
                                           (make-struct `,(make-make-struct struct-name)))
  "shortcut for (defmethod mread-object (...))"
  (declare (type simple-vector accessor-names)
           (type symbol mread-struct))

  (check-decl-mserializable-struct-args accessor-names accessor-names-p
                                        mread-struct 'mread)
    
  (with-gensyms (type ptr index end-index new-index)
    `(defmethod mread-object ((,type (eql ',struct-name)) ,ptr ,index ,end-index &key)
       (declare (type mem-size ,index ,end-index))
     
       ,(if accessor-names-p
            (let ((vars (loop for accessor-name across accessor-names
                           collect (gensym (symbol-name accessor-name)))))

              `(with-mread* (,@vars ,new-index) (,ptr ,index ,end-index)
                 (values
                  (,make-struct ,@(loop for accessor-name across accessor-names
                                     for var in vars
                                     collect (accessor-name-to-keyword struct-name accessor-name)
                                     collect var))
                  ,new-index)))

            `(,mread-struct ,type ,ptr ,index ,end-index)))))



(defmacro decl-mserializable-struct (struct-name
                                     &key (accessor-names #() accessor-names-p)
                                       msize-struct mwrite-struct mread-struct
                                       (make-struct `,(make-make-struct struct-name)))
  "shortcut for (decl-msize-struct ...) (decl-mwrite-struct ...) (decl-mread-struct ...)"
  `(progn
     ,@(when msize-struct
             `((decl-msize-struct ,struct-name
                                  ,@(when accessor-names-p `(:accessor-names ,accessor-names))
                                  :msize-struct ,msize-struct)))
     
     ,@(when mwrite-struct
             `((decl-mwrite-struct ,struct-name
                                  ,@(when accessor-names-p `(:accessor-names ,accessor-names))
                                  :mwrite-struct ,mwrite-struct)))

     ,@(when mread-struct
             `((decl-mread-struct ,struct-name
                                  ,@(when accessor-names-p `(:accessor-names ,accessor-names))
                                 :mread-struct ,mread-struct
                                 :make-struct ,make-struct)))))
