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
  "syntactic sugar for multiple calls to mread"
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



  
(defgeneric mlist-object-slots (object)
  (:documentation
   "List the persistent slots of an object. used by (msize-object-slots)
\(mwrite-object-slots) and (mread-object-slots) to reflectively obtain
the slots list from an object.
Must return a list of either slot names or closer-mop:slot-definition.

Default implementation for standard-objects is to call (closer-mop:class-slots (class-of object))"))


(defmethod mlist-object-slots ((object standard-object))
  (closer-mop:class-slots (class-of object)))


(defun msize-object-slots (object index
                           &key use-slot-names
                             (slots (mlist-object-slots object)))
  "Reflective implementation of (msize-object): loop on object's slots and call (msize) on each."
  (declare (type standard-object object)
           (type mem-size index)
           (type list slots))
  (dolist (slot slots)
    (let ((slot-name (if (symbolp slot)
                         slot
                         (closer-mop:slot-definition-name slot))))
      (when use-slot-names
        (setf index (msize slot-name index)))

      (setf index (msize (slot-value object slot-name) index))))

  (when use-slot-names
    ;; reserve space for 'nil slot-name, used as end-of-slots marker
    (setf index (msize nil index)))
  index)


(declaim (inline mwrite-object-slot))

(defun mwrite-slot (ptr index end-index slot-value &optional slot-name)
  (declare (type mem-size index end-index)
           (type symbol slot-name))
  
  (when slot-name
    (setf index (mwrite ptr index end-index slot-name)))

  (mwrite ptr index end-index slot-value))



(defmacro mwrite-object-slot (object ptr index end-index slot-name
                              &key use-slot-names)
  "note: slot-name MUST be quoted"
  `(mwrite-slot ,ptr ,index ,end-index (slot-value ,object ,slot-name)
                ,@(if (constantp use-slot-names)
                      (when (eval use-slot-names) `(,slot-name))
                      `((when use-slot-names ,slot-name)))))


(defun mwrite-object-slots (object ptr index end-index
                            &key use-slot-names (slots (mlist-object-slots object)))
  "Reflective implementation of (mwrite-object): loop on object's slots and call
 (mwrite-slot) on each."
  (declare (type standard-object object)
           (type mem-size index end-index)
           (type list slots))
  (dolist (slot slots)
    (let ((slot-name (if (symbolp slot)
                         slot
                         (closer-mop:slot-definition-name slot))))
      
      (setf index (mwrite-object-slot
                   object ptr index end-index
                   slot-name :use-slot-names use-slot-names))))

  (when use-slot-names
    ;; write 'nil slot-name, used as end-of-slots marker
    (setf index (mwrite ptr index end-index nil)))

  index)


(defun mread-object-slots-using-names (object ptr index end-index slots
                                       &key validate-slots)
  (declare (type standard-object object)
           (type mem-size index end-index)
           (type list slots))
  (let ((slot-names (when validate-slots
                      (loop for slot in slots collect
                           (if (symbolp slot)
                               slot
                               (closer-mop:slot-definition-name slot))))))
    (loop
       (with-mread* (slot-name new-index) (ptr index end-index)
         (setf index new-index)
         (when (null slot-name) (return))

         (with-mread* (value new-index) (ptr index end-index)
           (setf index new-index)
           (when (or (not validate-slots) (member slot-name slot-names))
             (setf (slot-value object slot-name) value)))))
    (values object index)))
       

(defmacro mread-object-slot (object ptr index end-index slot-name)
  "Read an object slot previously written WITHOUT its name.
Note: slot-name will be evaluated - use 'foo, NOT foo"
  (with-gensyms (value new-index)
    `(multiple-value-bind (,value ,new-index)
	 (mread ,ptr ,index ,end-index)
       (setf (slot-value ,object ,slot-name) ,value
	     ,index ,new-index))))


(defun mread-object-slots (object ptr index end-index
                           &key use-slot-names (slots (mlist-object-slots object)))
  "Reflective implementation of (mread-object): loop on object's slots and call (mread) on each."
  (declare (type standard-object object)
           (type mem-size index end-index)
           (type list slots))

  (if use-slot-names
      (mread-object-slots-using-names object ptr index end-index slots)

      (progn
	(dolist (slot slots)
	  (let ((slot-name (if (symbolp slot)
			       slot
			       (closer-mop:slot-definition-name slot))))
	    (mread-object-slot object ptr index end-index slot-name)))
	(values object index))))


(defun check-slot-names (slots)
  (declare (type list slots))
  (dolist (slot slots)
    (check-type slot symbol))
  t)


(defmacro decl-msize-class (class-name &key use-slot-names
                                         (slots nil slots-p)
                                         (msize-object 'msize-object-slots))
  "shortcut for (defmethod msize-object (...))"
  (declare (type symbol msize-object))
  (check-slot-names slots)
  
  (when (or slots msize-object)
    (with-gensyms (obj index)
      `(defmethod msize-object ((,obj ,class-name) ,index)
         (declare (type mem-size ,index))
         ,(if slots-p
              `(msize* index ,@(loop for slot in slots
                                  collect `(slot-value ,obj ',slot)))
              `(,msize-object ,obj ,index
                              :use-slot-names ,use-slot-names))))))



(defmacro decl-mwrite-class (class-name &key use-slot-names
                                          (slots nil slots-p)
                                          (mwrite-object 'mwrite-object-slots))
  "shortcut for (defmethod mwrite-object (...))"
  (declare (type symbol mwrite-object))
  (check-slot-names slots)
  
  (when (or slots mwrite-object)
    (with-gensyms (obj ptr index end-index)
      `(defmethod mwrite-object ((,obj ,class-name) ,ptr ,index ,end-index)
         (declare (type mem-size ,index ,end-index))
         ,(if slots-p
              `(progn
                 ,@(loop for slot in slots collect
                        `(setf ,index (mwrite-object-slot
                                       ,obj ,ptr ,index ,end-index ',slot
                                       :use-slot-names ,use-slot-names)))
                 ,index)
                
              `(,mwrite-object ,obj ,ptr ,index ,end-index
                               :use-slot-names ,use-slot-names))))))
  

(defmacro decl-mread-class (class-name
			    &key use-slot-names
			    (slots nil slots-p)
			    (mread-object 'mread-object-slots)
			    (new-instance  `(make-instance ',class-name)))
  "shortcut for (defmethod mread-object (...))"
  (declare (type symbol mread-object)
           (type list new-instance))
  (check-slot-names slots)
  
  (when (or slots mread-object)
    (with-gensyms (class ptr index end-index)
      `(defmethod mread-object ((,class (eql ',class-name)) ,ptr ,index ,end-index)
         (declare (type mem-size ,index ,end-index))
         ,(cond
	   (use-slot-names
	    (with-gensyms (object)
	      `(let ((,object ,new-instance))
		 (mread-object-slots-using-names ,object ,ptr ,index ,end-index))))

	   (slots-p
	    (with-gensyms (object)
	      `(let ((,object ,new-instance))
		 ,@(loop for slot-name in slots collect
			`(mread-object-slot ,object ,ptr ,index ,end-index ',slot-name))
		 (values ,object ,index))))

	   (t
	    `(,mread-object ,new-instance ,ptr ,index ,end-index
			    :use-slot-names ,use-slot-names)))))))
	 
    
(defmacro decl-serializable-class (class-name
				   &key use-slot-names
				   (slots nil slots-p)
				   (msize-object  'msize-object-slots)
				   (mwrite-object 'mwrite-object-slots)
				   (mread-object  'mread-object-slots)
				   (new-instance  `(make-instance ',class-name)))
  `(progn
     ,@(when msize-object
             `((decl-msize-class ,class-name :use-slot-names ,use-slot-names
                                 ,@(when slots-p `(:slots ,slots))
                                 :msize-object ,msize-object)))
     
     ,@(when mwrite-object
             `((decl-mwrite-class ,class-name :use-slot-names ,use-slot-names
                                  ,@(when slots-p `(:slots ,slots))
                                  :mwrite-object ,mwrite-object)))

     ,@(when mread-object
             `((decl-mread-class ,class-name :use-slot-names ,use-slot-names
                                 ,@(when slots-p `(:slots ,slots))
                                 :mread-object ,mread-object
                                 :new-instance ,new-instance)))))

             





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

    
