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


(in-package :hyperluminal-db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    dispatchers for object types                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric mdetect-object-size (object mdetect-size-func index)
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













(defmacro call-mdetect-size1 (value)
  "WARNING: this macro expands references to the hard-coded symbols MDETECT-SIZE-FUNC INDEX"
  `(incf index (the mem-size (funcall mdetect-size-func ,value))))


(defmacro call-mdetect-size (value &rest more-values)
  "WARNING: this macro expands references to the hard-coded symbols MDETECT-SIZE-FUNC INDEX"
  (if more-values
      `(progn
         (call-mdetect-size1 ,value)
         ,@(loop for v in more-values
              collect `(call-mdetect-size1 ,v)))

      `(call-mdetect-size1 ,value)))
  


(defmacro call-mwrite1 (value)
  "WARNING: this macro expands references to the hard-coded symbols MWRITE-FUNC PTR INDEX END-INDEX"
  `(setf index (the mem-size (funcall mwrite-func ptr index end-index ,value))))


(defmacro call-mwrite (value &rest more-values)
  "WARNING: this macro expands references to the hard-coded symbols MWRITE-FUNC PTR INDEX END-INDEX"
  (if more-values
      `(progn
         (call-mwrite1 ,value)
         ,@(loop for v in more-values
              collect `(call-mwrite1 ,v)))

      `(call-mwrite1 ,value)))


(defmacro with-mread1 ((value) &body body)
  "WARNING: this macro expands references to the hard-coded symbols MREAD-FUNC PTR INDEX END-INDEX"
  (with-gensym new-index
    `(multiple-value-bind (,value ,new-index)
         (the (values t mem-size) (funcall mread-func ptr index end-index))
       (setf index ,new-index)
       ,@body)))


(defmacro with-mread ((var &rest more-vars) &body body)
  "WARNING: this macro expands references to the hard-coded symbols MREAD-FUNC PTR INDEX END-INDEX"
  (if more-vars
      `(with-mread1 (,var)
         (with-mread ,more-vars
           ,@body))
      `(with-mread1 (,var)
         ,@body)))










(defun mdetect-obj-size (object mdetect-size-func index)
  "Compute and return the number of memory words needed to serialize OBJECT,
including its header"
  (declare (type function mdetect-size-func)
           (type mem-size index))

  (incf index +mem-box/header-words+)
  (call-mdetect-size (type-of object))

  (the (values mem-size &optional)
    (mdetect-object-size object mdetect-size-func index)))


(defun mwrite-obj (object mwrite-func ptr index end-index)
  "Serialize OBJECT by writing it into the memory starting at (+ PTR INDEX).
Also serializes OBJECT header.
The available memory ends immediately before (+ PTR END-INDEX)."
  (declare (type function mwrite-func)
           (type mem-size index end-index))

  ;; write OBJECT payload
  (let* ((index1 (funcall mwrite-func ptr
                          (mem-size+ +mem-box/header-words+ index)
                          end-index (type-of object)))

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
  (with-mread (type)
    ;; TODO validate type against a set of trusted types
    (check-type type symbol)

    (the (values t mem-size &optional)
      (mread-object type mread-func ptr index end-index))))

    