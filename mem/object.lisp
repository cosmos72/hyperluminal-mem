;; -*- lisp -*-

;; This file is part of Hyperluminal-MEM.
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

(declaim (inline extract-slot-name))

(defun extract-slot-name (slot)
  (if (symbolp slot)
      slot
      (the symbol (closer-mop:slot-definition-name slot))))

(defun extract-slot-names (slots)
  (mapcar #'extract-slot-name slots))


(defgeneric mlist-class-direct-slots (class)
  (:documentation
   "List the direct slots of a class that must be serialized/deserialized.
Used by (msize-list-class-slots class) to reflectively obtain
the serializable slots list from a class.
Must return a list of either slot names or closer-mop:slot-definition.

This function is relatively slow, and it is expected to be invoked
only at compile-time.

Default implementation calls (closer-mop:class-direct-slots class),
i.e. it assumes all slots must be serialized/deserialized."))

(defmethod mlist-class-direct-slots (class)
  (when (symbolp class)
    (setf class (find-class class)))
  (closer-mop:class-direct-slots class))


(defgeneric mlist-class-slots (class)
  (:documentation
   "List all the serializable slots of a class.
Must return a list of either slot names or closer-mop:slot-definition.
If you specialize this generic function, remember to also list
all superclasses' slots that must be serialized.

Invoked by the macros (decl-msize-class) (decl-mwrite-class) and (decl-mread-class)
to obtain the slots of a class that must be serialized/deserialized
and generate the methods (msize-object) (mwrite-object) and (mread-object).

Default implementation repeatedly calls (mlist-class-direct-slots)
on all the class' superclasses and collects the result.

This function is relatively slow, and it is normally invoked
only at compile-time.
May be useful also at runtime if one chooses to use the slow, reflective functions
\(msize-object-slots) (mwrite-object-slots) and (mread-object-slots)."))


(defmethod mlist-class-slots (class)
  (when (symbolp class)
    (setf class (find-class class)))
  (unless (closer-mop:class-finalized-p class)
    (closer-mop:finalize-inheritance class))
  (let ((slots nil))
    (dolist (superclass (closer-mop:class-precedence-list class) slots)
      (dolist (slot (mlist-class-direct-slots superclass))
        (setf slots (adjoin slot slots :test #'eq :key #'extract-slot-name))))))
    


(defmacro decl-mlist-class-direct-slots (class-name &key direct-slots)
  `(eval-always
     (defmethod mlist-class-direct-slots ((class (eql ',class-name)))
       ',direct-slots)))

(defmacro decl-mlist-class-slots (class-name &key slots)
  `(eval-always
     (defmethod mlist-class-slots ((class (eql ',class-name)))
       ',slots)))


(defun undecl-mlist-class-slots (class-name)
  "Remove the method (mlist-class-slots) specialized for CLASS-NAME"
  (let ((method (find-method #'mlist-class-slots nil `((eql ,class-name)) nil)))
    (when method
      (remove-method #'mlist-class-slots method)
      t)))


(defun undecl-mlist-class-direct-slots (class-name)
  "Remove the method (mlist-class-direct-slots) specialized for CLASS-NAME"
  (let ((method (find-method #'mlist-class-direct-slots nil `((eql ,class-name)) nil)))
    (when method
      (remove-method #'mlist-class-direct-slots method)
      t)))



(defun msize-object-slots (object index slot-names &key use-slot-names)
  "loop on specified object's slots and call (msize) on each."
  (declare (type standard-object object)
           (type mem-size index)
           (type list slot-names))
  (dolist (slot-name slot-names)
    (when use-slot-names
      (setf index (msize slot-name index)))

    (setf index (msize (slot-value object slot-name) index)))

  (when use-slot-names
    ;; reserve space for 'nil slot-name, used as end-of-slots marker
    (setf index (msize nil index)))
  index)


(declaim (inline mwrite-slot))

(defun mwrite-slot (ptr index end-index slot-value &optional slot-name)
  (declare (type mem-size index end-index)
           (type symbol slot-name))
  
  (when slot-name
    (setf index (mwrite ptr index end-index slot-name)))

  (mwrite ptr index end-index slot-value))


(declaim (inline mwrite-object-slot))

(defun mwrite-object-slot (object ptr index end-index slot-name
                              &key use-slot-names)
  (declare (type standard-object object)
           (type mem-size index end-index)
           (type symbol slot-name))
  (mwrite-slot ptr index end-index (slot-value object slot-name)
               (when use-slot-names slot-name)))


(defun mwrite-object-slots (object ptr index end-index slots &key use-slot-names)
  "loop on specified object's slots and call (mwrite-slot) on each."
  (declare (type standard-object object)
           (type mem-size index end-index)
           (type list slots))
  (dolist (slot slots)
    (let ((slot-name (extract-slot-name slot)))
      (setf index (mwrite-object-slot
                   object ptr index end-index
                   slot-name :use-slot-names use-slot-names))))

  (when use-slot-names
    ;; write 'nil slot-name, used as end-of-slots marker
    (setf index (mwrite ptr index end-index nil)))

  index)




(defun mread-object-slots-by-name (object ptr index end-index slots)
  (declare (type standard-object object)
           (type mem-size index end-index)
           (type list slots))
  (loop
     (with-mread* (slot-name new-index) (ptr index end-index)
       (setf index new-index)
       (when (null slot-name) (return))

       (with-mread* (value new-index) (ptr index end-index)
         (setf index new-index)
         (when (member slot-name slots :key #'extract-slot-name :test #'eq)
           (setf (slot-value object slot-name) value)))))
  (values object index))


(defmacro mread-object-slot (object ptr index end-index slot-name)
  "Read an object slot previously written WITHOUT its name.
Note: slot-name will be evaluated - use 'foo, NOT foo
Note: updates INDEX"
  (with-gensyms (value new-index)
    `(multiple-value-bind (,value ,new-index)
	 (mread ,ptr ,index ,end-index)
       (setf (slot-value ,object ,slot-name) ,value
	     ,index ,new-index))))


(defun mread-object-slots (object ptr index end-index slots &key use-slot-names)
  "loop on specified object's slots and call (mread) on each."
  (declare (type standard-object object)
           (type mem-size index end-index)
           (type list slots))

  (if use-slot-names
      (mread-object-slots-by-name object ptr index end-index slots)

      (dolist (slot slots (values object index))
        (let ((slot-name (extract-slot-name slot)))
          (mread-object-slot object ptr index end-index slot-name)))))




(defmacro decl-msize-class (class-name &key use-slot-names
                                         (slots (mlist-class-slots class-name))
                                         (msize-object 'msize-object-slots msize-object-p))
  "shortcut for (defmethod msize-object ((class (eql CLASS-NAME)) index) ...)"
  (declare (type list slots)
           (type symbol msize-object))

  (setf use-slot-names (check-compile-constant use-slot-names))
  
  (with-gensyms (obj index)
    `(defmethod msize-object ((,obj ,class-name) ,index)
       (declare (type mem-size ,index))
       ,(if msize-object-p
            `(,msize-object ,obj ,index ',(extract-slot-names slots)
                            :use-slot-names ,use-slot-names)
            `(msize* ,index
                     ,@(let ((forms))
                            (dolist (slot slots)
                              (let ((slot-name (extract-slot-name slot)))
                                (when use-slot-names
                                  (push `',slot-name forms))
                                (push `(slot-value ,obj ',slot-name) forms)))
                            (when use-slot-names
                              (push nil forms))
                            (nreverse forms)))))))


(defmacro decl-mwrite-class (class-name &key use-slot-names
                                          (slots (mlist-class-slots class-name))
                                          (mwrite-object 'mwrite-object-slots mwrite-object-p))
  "shortcut for (defmethod mwrite-object (...))"
  (declare (type list slots)
           (type symbol mwrite-object))

  (setf use-slot-names (check-compile-constant use-slot-names))
  
  (with-gensyms (obj ptr index end-index)
    `(defmethod mwrite-object ((,obj ,class-name) ,ptr ,index ,end-index)
       (declare (type mem-size ,index ,end-index))
       ,(if mwrite-object-p
            `(,mwrite-object ,obj ,ptr ,index ,end-index ',(extract-slot-names slots)
                             :use-slot-names ,use-slot-names)
            
            `(mwrite* ,ptr ,index ,end-index
                      ,@(let ((forms))
                             (dolist (slot slots)
                               (let ((slot-name (extract-slot-name slot)))
                                 (when use-slot-names
                                   (push `',slot-name forms))
                                 (push `(slot-value ,obj ',slot-name) forms)))
                             (when use-slot-names
                               (push nil forms))
                             (nreverse forms)))))))


(defmacro decl-mread-class (class-name
			    &key use-slot-names validate-slot-names
                              (slots (mlist-class-slots class-name))
                              (mread-object 'mread-object-slots mread-object-p)
                              (new-instance  `(make-instance ',class-name)))
  "shortcut for (defmethod mread-object (...))"
  (declare (type list slots)
           (type symbol mread-object))

  (setf use-slot-names (check-compile-constant use-slot-names))
  
  (with-gensyms (class ptr index end-index)
    `(defmethod mread-object ((,class (eql ',class-name)) ,ptr ,index ,end-index
                              &key &allow-other-keys)
       (declare (type mem-size ,index ,end-index))
       ,(cond
         (mread-object-p
          `(,mread-object ,new-instance ,ptr ,index ,end-index ',(extract-slot-names slots)
                          :use-slot-names ,use-slot-names))

         (use-slot-names
          `(mread-object-slots-by-name ,new-instance ,ptr ,index ,end-index
                                       ',(extract-slot-names slots)
                                       :validate-slot-names ,validate-slot-names))
         (t
          (with-gensyms (object)
            `(let ((,object ,new-instance))
               ,@(loop for slot in slots collect
                      `(mread-object-slot ,object ,ptr ,index ,end-index ',(extract-slot-name slot)))
               (values ,object ,index))))))))
	 
    
 
(defun undecl-msize-class (class-name)
  "Remove the method (msize-object) specialized for CLASS-NAME"
  (let ((method (find-method #'msize-object nil `(,class-name t) nil)))
    (when method
      (remove-method #'msize-object method)
      t)))


(defun undecl-mwrite-class (class-name)
  "Remove the method (mwrite-object) specialized for CLASS-NAME"
  (let ((method (find-method #'mwrite-object nil `(,class-name t t t) nil)))
    (when method
      (remove-method #'mwrite-object method)
      t)))

(defun undecl-mread-class (class-name)
  "Remove the method (mread-object) specialized for CLASS-NAME"
  (let ((method (find-method #'mread-object nil `((eql ,class-name) t t t) nil)))
    (when method
      (remove-method #'mread-object method)
      t)))


(defmacro decl-mserializable-class (class-name
                                    &key use-slot-names
                                      (direct-slots nil direct-slots-p)
                                      (slots nil slots-p)
                                      (msize-object  'msize-object-slots  msize-object-p)
                                      (mwrite-object 'mwrite-object-slots mwrite-object-p)
                                      (mread-object  'mread-object-slots  mread-object-p)
                                      (new-instance  `(make-instance ',class-name)))
  "shortcut for (decl-msize-class ...) (decl-mwrite-class ...) (decl-mread-class ...)"
  `(progn
     ,(if direct-slots-p
          `(decl-mlist-class-direct-slots ,class-name :direct-slots ,direct-slots)
          `(undecl-mlist-class-direct-slots ',class-name))

     ,(if slots-p
          `(decl-mlist-class-slots ,class-name :slots ,slots)
          `(undecl-mlist-class-slots ',class-name))

     (decl-msize-class ,class-name
                       :use-slot-names ,use-slot-names
                       ,@(when slots-p `(:slots ,slots))
                       ,@(when msize-object-p `(:msize-object ,msize-object)))

     (decl-mwrite-class ,class-name
                        :use-slot-names ,use-slot-names
                        ,@(when slots-p `(:slots ,slots))
                        ,@(when mwrite-object-p `(:mwrite-object ,mwrite-object)))

     (decl-mread-class ,class-name
                       :use-slot-names ,use-slot-names
                       ,@(when slots-p `(:slots ,slots))
                       ,@(when mread-object-p `(:mread-object ,mread-object))
                       :new-instance ,new-instance)
     ',class-name))



                

(defmacro undecl-mserializable-class (class-name)
  `(or-func
    (undecl-mlist-class-direct-slots ',class-name)
    (undecl-mlist-class-slots ',class-name)
    (undecl-msize-class ',class-name)
    (undecl-mread-class ',class-name)
    (undecl-mwrite-class ',class-name)))
  

(defmacro mserializable-class
    ((defclass name direct-superclasses direct-slots &rest options)
     &rest decl-mserializable-class-options)
  `(progn
     (,defclass ,name ,direct-superclasses ,direct-slots ,@options)
     (decl-mserializable-class ,@decl-mserializable-class-options)
     ,name))
