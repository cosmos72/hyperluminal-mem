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
;;;;    boxed vector                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
#|
(defmacro %mwrite-array-1-unboxed (a type)
  (with-gensym e
    `(progn
       (check-mem-overrun ptr index end-index len)
       (loop ;; with log = (log:info "1-unboxed ~S ~S" ',a ',type)
          for ,e across ,a do
            (mset-unboxed ptr index (the ,type ,e))
            (incf-mem-size index)))))



(defmacro %mwrite-array-1-t (a type)
  (with-gensym e
    `(loop
        for ,e across ,a
        do (setf index (mwrite ptr index end-index
                               (the ,type ,e))))))


|#

(deftype array1 (&optional (element-type '*))
  `(and (array ,element-type (*))
        (not (or string base-string bit-vector))))

(defun box-words/vector (index vector)
  "Return the number of words needed to store VECTOR in mmap memory,
not including BOX header."
  (declare (type array1 vector)
           (type mem-size index))

  (box-words/array index vector))



(defmacro %the-array1 (a type simple)
  `(the (,(if simple 'simple-array 'array)
          ,(if type type '*)
          (*))
     ,a))

         
(defmacro %mwrite-array1-unboxed (a type)
  (with-gensym e
    `(progn
       (check-mem-overrun ptr index end-index len)
       (loop for ,e across ,a do
            (mset-unboxed ptr index (the ,type ,e))
            (incf-mem-size index)))))


(defmacro %mwrite-array1-t (a type)
  (with-gensym e
    `(loop for ,e across ,a do
          (setf index (mwrite ptr index end-index
                              (the ,type ,e))))))


(defmacro %mwrite-array1 (a type simple)
  `(cond
     ((subtypep ,type 'mem-int)
      (if (subtypep 'mem-int ,type)
          (%mwrite-array1-unboxed (%the-array1 ,a mem-int ,simple) mem-int)
          (%mwrite-array1-unboxed (%the-array1 ,a   *     ,simple) mem-int)))

     #?+hldb/sfloat/inline
     ((eq ,type 'single-float)
      (%mwrite-array1-unboxed (%the-array1 ,a single-float ,simple) single-float))

     #?+hldb/dfloat/inline
     ((eq ,type 'double-float)
      (%mwrite-array1-unboxed (%the-array1 ,a double-float ,simple) double-float))

     ((eq ,type t)
      (%mwrite-array1-t (%the-array1 ,a t ,simple) t))

     (t
      (%mwrite-array1-t (%the-array1 ,a * ,simple) t))))



(defun mwrite-box/vector (ptr index end-index vector)
  "Write VECTOR into the memory starting at (PTR+INDEX).
Return number of words actually written.

Assumes BOX header is already written, and that enough memory is available
at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index end-index)
	   (type array1 vector))

  (check-mem-overrun ptr index end-index 1)

  (let ((type (array-element-type vector))
        (len  (length vector)))

    (mset-int ptr index (the mem-int len))
    (incf-mem-size index)

    (if (typep vector 'simple-array)
        (%mwrite-array1 vector type t)
        (%mwrite-array1 vector type nil)))
  index)


(defun mread-box/vector (ptr index end-index)
  "Read a vector from the memory starting at (PTR+INDEX) and return it.
Also returns number of words actually read as additional value.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (check-mem-length ptr index end-index 1)

  (let* ((len (mget-int ptr index)))
	 
    (check-array-length ptr index 'vector len)

    (incf-mem-size index)

    (let1 vector (the simple-vector (make-array len))
      (loop for i from 0 below len
	 do (multiple-value-bind (e e-index) (mread ptr index end-index)
	      (setf (svref vector i) e
                    index (the mem-size e-index))))

      (values vector index))))
