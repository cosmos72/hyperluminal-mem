;; -*- lisp -*-

;; This file is part of Hyperluminal-mem.
;; Copyright (c) 2013-2015 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package :hyperluminal-mem)



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

(defgeneric mread-object (type ptr index end-index &key &allow-other-keys)
  (:documentation
   "Deserialize an object of type TYPE by reading it from the memory starting at (+ PTR INDEX).
Assumes OBJECT header was already read.
The available memory ends immediately before (+ PTR END-INDEX)."))



  








;; (declaim (ftype ...)) for msize-obj, mwrite-obj and mread-obj is in box.lisp

(defun msize-obj (index object)
  "Compute and return the number of memory words needed to serialize OBJECT,
including its header"
  (declare (type mem-size index))

  (incf index +mem-box/header-words+)

  (let1 index (msize index (type-of object))
    (the (values mem-size &optional)
      (msize-object object index))))


(defun mwrite-obj (ptr index end-index object)
  "Serialize OBJECT by writing it into the memory starting at (+ PTR INDEX).
Also serializes OBJECT header.
The available memory ends immediately before (+ PTR END-INDEX)."
  (declare (type maddress ptr)
           (mem-size index end-index))

  ;; write OBJECT payload
  (let* ((index0 (mem-size+ index +mem-box/header-words+))
         (index1 (mwrite ptr index0 end-index (type-of object)))
         (index2 (mwrite-object object ptr index1 end-index))
         (actual-words (mem-size- index2 index)))
         
    (when (> index2 end-index)
      (error "HYPERLUMINAL-MEM internal error!
wrote ~S word~P at address ~S + ~S,
but only ~S words were available at that location.
Either this is a bug in hyperluminal-mem, or some object
was concurrently modified while being written"
             actual-words actual-words ptr index (mem-size- end-index index)))

    ;; write OBJECT header
    (mwrite-box/header ptr index +mem-obj+ (round-up-size actual-words))
    index2))


(defun mread-obj (ptr index end-index)
  "Deserialize an object of type TYPE by reading it from the memory starting at (+ PTR INDEX).
Also deserializes OBJECT header.
The available memory ends immediately before (+ PTR END-INDEX)."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  ;; skip BOX header
  (incf index +mem-box/header-words+)

  ;; read OBJECT type
  (multiple-value-bind (type new-index) (mread ptr index end-index)
    ;; TODO validate type against a set of trusted types
    (check-type type symbol)

    (the (values t mem-size &optional)
      (mread-object type ptr new-index end-index))))

    







(defun check-reader-names (reader-names)
  (declare (type list reader-names))
  (dolist (reader-name reader-names t)
    (check-type reader-name symbol)))


(defun reader-name-to-keyword (struct-name reader-name)
  (let* ((prefix (concatenate 'string (symbol-name struct-name) "-"))
         (prefix-len (length prefix))
         (name (symbol-name reader-name))
         (name-len   (length name)))

    (intern
     (if (and (> name-len prefix-len)
              (string= prefix (the string (subseq name 0 prefix-len))))
         (subseq name prefix-len)
         name)
     :keyword)))


(defun check-decl-mserializable-struct-args (reader-names reader-names-p mfunc mfunc-name)
  
  (when (eq (null reader-names-p) (null mfunc))
    (error "error while invoking ~S~S~S:~%  exactly one of ~S or ~S~S must be specified"
           'decl- mfunc-name '-struct 'reader-names mfunc-name '-struct))
       
  (check-reader-names reader-names))


(defun make-make-struct (struct-name)
  (declare (type symbol struct-name))
  (let ((pkg (symbol-package struct-name))
        (name (symbol-name struct-name)))
    (intern
     (concatenate 'string (symbol-name 'make-) name)
     pkg)))
    



(defmacro decl-msize-struct (struct-name &key (reader-names nil reader-names-p)
                                           msize-struct)
  "shortcut for (defmethod msize-object (...))"
  (declare (type list reader-names)
           (type symbol msize-struct))

  (check-decl-mserializable-struct-args reader-names reader-names-p
                                        msize-struct 'msize)
  
  (with-gensyms (obj index)
    `(defmethod msize-object ((,obj ,struct-name) ,index)
       (declare (type mem-size ,index))
     
       ,(if reader-names-p
            `(msize* ,index ,@(loop for reader-name in reader-names
                                 collect `(,reader-name ,obj)))

            `(,msize-struct ,obj ,index)))))




(defmacro decl-mwrite-struct (struct-name &key (reader-names nil reader-names-p)
                                            mwrite-struct)
  "shortcut for (defmethod mwrite-object (...))"
  (declare (type list reader-names)
           (type symbol mwrite-struct))

  (check-decl-mserializable-struct-args reader-names reader-names-p
                                        mwrite-struct 'mwrite)
    
  (with-gensyms (obj ptr index end-index)
    `(defmethod mwrite-object ((,obj ,struct-name) ,ptr ,index ,end-index)
       (declare (type mem-size ,index ,end-index))
     
       ,(if reader-names-p
            `(mwrite* ,ptr ,index ,end-index
                      ,@(loop for reader-name in reader-names
                           collect `(,reader-name ,obj)))

            `(,mwrite-struct ,obj ,index)))))



(defmacro decl-mread-struct (struct-name &key (reader-names nil reader-names-p)
                                           mread-struct
                                           (make-struct `,(make-make-struct struct-name)))
  "shortcut for (defmethod mread-object (...))"
  (declare (type list reader-names)
           (type symbol mread-struct))

  (check-decl-mserializable-struct-args reader-names reader-names-p
                                        mread-struct 'mread)
    
  (with-gensyms (type ptr index end-index new-index)
    `(defmethod mread-object ((,type (eql ',struct-name)) ,ptr ,index ,end-index &key)
       (declare (type mem-size ,index ,end-index))
     
       ,(if reader-names-p
            (let ((vars (loop for reader-name in reader-names
                           collect (gensym (symbol-name reader-name))))
                  (keywords (if (listp make-struct) (rest make-struct) nil))
                  (maker    (if (listp make-struct) (first make-struct) make-struct)))
              
              `(with-mread* (,@vars ,new-index) (,ptr ,index ,end-index)
                 (values
                  (,maker ,@(loop for reader-name in reader-names
                               for var in vars
                               for ks = keywords then (rest ks)
                               collect (or (first ks)
                                           (reader-name-to-keyword struct-name reader-name))
                               collect var))
                  ,new-index)))

            `(,mread-struct ,type ,ptr ,index ,end-index)))))



(defmacro decl-mserializable-struct (struct-name
                                     &key (reader-names nil reader-names-p)
                                       msize-struct mwrite-struct mread-struct
                                       (make-struct `,(make-make-struct struct-name)))
  "shortcut for (decl-msize-struct ...) (decl-mwrite-struct ...) (decl-mread-struct ...)"
  `(progn
     ,@(when msize-struct
             `((decl-msize-struct ,struct-name
                                  ,@(when reader-names-p `(:reader-names ,reader-names))
                                  :msize-struct ,msize-struct)))
     
     ,@(when mwrite-struct
             `((decl-mwrite-struct ,struct-name
                                   ,@(when reader-names-p `(:reader-names ,reader-names))
                                   :mwrite-struct ,mwrite-struct)))

     ,@(when mread-struct
             `((decl-mread-struct ,struct-name
                                  ,@(when reader-names-p `(:reader-names ,reader-names))
                                  :mread-struct ,mread-struct
                                  :make-struct ,make-struct)))))
