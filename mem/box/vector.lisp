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

    


(defun box-words/vector (index vector)
  "Return the number of words needed to store VECTOR in mmap memory,
not including BOX header."
  (declare (type (and vector (not (or string base-string bit-vector))) vector)
           (type mem-size index))

  (box-words/array index vector))


(defun mwrite-box/vector (ptr index end-index vector)
  "Write VECTOR into the memory starting at (PTR+INDEX).
Return number of words actually written.

Assumes BOX header is already written, and that enough memory is available
at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index end-index)
	   (type (and vector (not (or string base-string bit-vector))) vector))

  (check-mem-overrun ptr index end-index 1)

  (mset-int ptr index (the mem-int (length vector)))
  (incf-mem-size index)

  (if (typep vector 'simple-vector)
      (loop for e across vector do
           (setf index (mwrite ptr index end-index e)))
      (loop for e across vector do
           (setf index (mwrite ptr index end-index e))))

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
