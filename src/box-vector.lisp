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
;;;;    boxed vector                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-words/vector (vector)
  "Return the number of words needed to store VECTOR in mmap memory,
not including BOX header."
  (declare (type (and vector (not (or string base-string bit-vector))) vector))

  (let ((len (length vector)))
    (unless (<= len +most-positive-int+)
      (error "HYPERLUMINAL-DB: vector too large for object store.
it contains ~S elements, maximum supported is ~S elements"
	     len +most-positive-int+)))

  ;; -1 to store vector length
  (let ((words-left (1- +mem-box/max-payload-words+)))

    (declare (type mem-size words-left))
    ;; count downward: easier to check for overflows

    (macrolet
	((compute-n-words (vector words-left)
	   (with-gensyms (e e-len detect-n-words)
	     `(let ((,detect-n-words #'detect-n-words))
		(loop for ,e across ,vector
		   for ,e-len = (the mem-size (funcall ,detect-n-words ,e))
		   do
		     (unless (>= ,words-left ,e-len)
		       (error "HYPERLUMINAL-DB: vector too large for object store,
it requires more space than the maximum supported ~S words"
			      +mem-box/max-payload-words+))
		     (decf-mem-size ,words-left ,e-len))))))

      (if (typep vector 'simple-vector)
	  ;; optimize for simple-vector
	  (compute-n-words vector words-left)
	  (compute-n-words vector words-left)))

    (mem-size- +mem-box/max-payload-words+ words-left)))


(defun mwrite-box/vector (ptr index vector)
  "Write VECTOR into the memory starting at (PTR+INDEX).
Return number of words actually written.

Assumes BOX header is already written, and that enough memory is available
at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index)
	   (type (and vector (not (or string base-string bit-vector))) vector))

  (let ((orig-index index)
	(mwrite #'mwrite))

    (mset-int ptr index (the mem-int (length vector)))
    (incf-mem-size index)

    (if (typep vector 'simple-vector)
	(loop for e across vector
	   do (incf-mem-size index
			     (the mem-size (funcall mwrite ptr index e))))
	(loop for e across vector
	   do (incf-mem-size index
			     (the mem-size (funcall mwrite ptr index e)))))

    ;; return number of words actually written, including BOX header
    (mem-size+ +mem-box/header-words+
	       (mem-size- index orig-index))))


(defun mread-box/vector (ptr index)
  "Read a vector from the boxed memory starting at (PTR+INDEX) and return it.
Also returns number of words actually read as additional value.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (let* ((orig-index index)
	 (len        (mget-int ptr index))
	 (vector     (the simple-vector (make-array len))))

    (incf-mem-size index)

    (let ((mread #'mread))
      (loop for i from 0 below len
	 do (multiple-value-bind (e e-len) (funcall mread ptr index)
	      (setf (svref vector i) e)
	      (incf-mem-size index e-len))))

    (values
     vector
     (mem-size+ +mem-box/header-words+
		(mem-size- index orig-index)))))
