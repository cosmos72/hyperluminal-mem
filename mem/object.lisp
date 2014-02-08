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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    dispatchers for object types                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric msize-object (object msize-func index)
  (:documentation
   "Compute and return the number of memory words needed to serialize OBJECT,
not including its header"))

(defgeneric mwrite-object (object mwrite-func ptr index end-index)
  (:documentation
   "Serialize OBJECT by writing it into the memory starting at (+ PTR INDEX).
Assumes OBJECT header was already written.
The available memory ends immediately before (+ PTR END-INDEX)."))

(defgeneric mread-object (type mread-func ptr index end-index &key)
  (:documentation
   "Deserialize an object of type TYPE by reading it from the memory starting at (+ PTR INDEX).
Assumes OBJECT header was already read.
The available memory ends immediately before (+ PTR END-INDEX)."))













(defmacro call-msize1 ((msize-func index) value)
  `(the (values mem-size &optional)
     (funcall ,msize-func ,value ,index)))

(defmacro call-msize ((msize-func index) value &rest more-values)
  "Warning: this macro expands multiple references to MSIZE-FUNC"
  (if more-values
      (with-gensym new-index
        `(let1 ,new-index (call-msize1 (,msize-func ,index) ,value)
           (call-msize (,msize-func ,new-index) ,@more-values)))
      `(call-msize1 (,msize-func ,index) ,value)))
  


(defmacro call-mwrite1 ((mwrite-func ptr index end-index) value)
  `(the (values mem-size &optional)
     (funcall ,mwrite-func ,ptr ,index ,end-index ,value)))


(defmacro call-mwrite ((mwrite-func ptr index end-index) value &rest more-values)
  "Warning: this macro expands multiple references to MWRITE-FUNC PTR and END-INDEX"
  (if more-values
      (with-gensym new-index
        `(let1 ,new-index (call-mwrite1 (,mwrite-func ,ptr ,index ,end-index) ,value)
           (call-mwrite (,mwrite-func ,ptr ,new-index ,end-index) ,@more-values)))
      `(call-mwrite1 (,mwrite-func ,ptr ,index ,end-index) ,value)))



(defmacro call-mread1 ((mread-func ptr index end-index))
  `(the (values t mem-size &optional)
     (funcall ,mread-func ,ptr ,index ,end-index)))


(defmacro multiple-bind-mread1 ((new-index var) (mread-func ptr index end-index) &body body)
  `(multiple-value-bind (,var ,new-index)
       (call-mread1 (,mread-func ,ptr ,index ,end-index))
     ,@body))


(defmacro multiple-bind-mread ((new-index var &rest more-vars)
                               (mread-func ptr index end-index) &body body)
  "Warning: this macro expands multiple references to MREAD-FUNC PTR and END-INDEX"
  (if more-vars
      (with-gensym tmp-index
        `(multiple-bind-mread1 (,tmp-index ,var) (,mread-func ,ptr ,index ,end-index)
           (multiple-bind-mread (,new-index ,@more-vars) (,mread-func ,ptr ,tmp-index ,end-index)
               ,@body)))
      `(multiple-bind-mread1 (,new-index ,var) (,mread-func ,ptr ,index ,end-index)
         ,@body)))










(defun msize-obj (object msize-func &optional (index 0))
  "Compute and return the number of memory words needed to serialize OBJECT,
including its header"
  (declare (type function msize-func)
           (type mem-size index))

  (incf index +mem-box/header-words+)

  (let1 index (call-msize (msize-func index) (type-of object))
    (the (values mem-size &optional)
      (msize-object object msize-func index))))


(defun mwrite-obj (object mwrite-func ptr index end-index)
  "Serialize OBJECT by writing it into the memory starting at (+ PTR INDEX).
Also serializes OBJECT header.
The available memory ends immediately before (+ PTR END-INDEX)."
  (declare (type function mwrite-func)
           (type mem-size index end-index))

  ;; write OBJECT payload
  (let* ((index1 (call-mwrite (mwrite-func ptr (mem-size+ +mem-box/header-words+ index)
                                           end-index)
                              (type-of object)))

         (index2 (mwrite-object object mwrite-func ptr index1 end-index))
         (actual-words (mem-size- index2 index)))
         
    (when (> index2 end-index)
      (error "HYPERLUMINAL-DB internal error!
wrote ~S word~P at address (+ ~S ~S),
but only ~S words were available at that location.
Either this is a bug in hyperluminal-db, or some object
was concurrently modified while being written"
             actual-words actual-words ptr index (mem-size- end-index index)))

    (mwrite-box/header ptr index +mem-obj/first+ (round-up-size actual-words))
    index2))


(defun mread-obj (mread-func ptr index end-index)
  "Deserialize an object of type TYPE by reading it from the memory starting at (+ PTR INDEX).
Also deserializes OBJECT header.
The available memory ends immediately before (+ PTR END-INDEX)."
  (declare (type function mread-func)
           (type mem-size index end-index))
  
  ;; skip BOX header
  (incf index +mem-box/header-words+)

  ;; read OBJECT type
  (multiple-bind-mread (new-index type) (mread-func ptr index end-index)
    ;; TODO validate type against a set of trusted types
    (check-type type symbol)

    (the (values t mem-size &optional)
      (mread-object type mread-func ptr new-index end-index))))

    