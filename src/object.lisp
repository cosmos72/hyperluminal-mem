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
   "Compute and return the number of memory words needed to serialize OBJECT"))

(defgeneric mwrite-object (object mwrite-func ptr index end-index)
  (:documentation
   "Serialize OBJECT by writing it into the memory starting at (+ PTR INDEX).
The available memory ends immediately before (+ PTR END-INDEX)."))

(defgeneric mread-object (type mread-func ptr index end-index)
  (:documentation
   "Deserialize an object of type TYPE by reading it from the memory starting at (+ PTR INDEX).
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

      `(call-mdetect-size1 (,value))))
  


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

      `(call-mwrite1 (,value))))


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
