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
;;;;    boxed vector                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    


(defun box-words/vector (vector index)
  "Return the number of words needed to store VECTOR in mmap memory,
not including BOX header."
  (declare (type (and vector (not (or string base-string bit-vector))) vector)
           (type mem-size index))

  (box-words/array vector index))


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
