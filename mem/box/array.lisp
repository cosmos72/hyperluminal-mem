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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed array                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-words/array (array index)
  "Return the number of words needed to store ARRAY in mmap memory,
not including BOX header."
  (declare (type array array))

  (let* ((rank (array-rank array))
         (len (array-total-size array))
         (msize #'msize))

    (unless (<= rank +most-positive-int+)
      (error "HYPERLUMINAL-MEM: array has too many dimensions for object store.
it has rank ~S, maximum supported is rank ~S"
	     rank +most-positive-int+))

    (unless (<= len +most-positive-int+)
      (error "HYPERLUMINAL-MEM: array too large for object store.
it contains ~S elements, maximum supported is ~S elements"
	     len +most-positive-int+))

    (unless (= 1 rank)
      ;; N-dimensional arrays also need 1 word to store the rank
      (incf-mem-size index))

    ;; 1 word per dimension
    (incf-mem-size index rank)

    (macrolet
	((compute-n-words (array len index &optional (func-aref 'row-major-aref))
	   (with-gensyms (i e)
	     `(loop for ,i from 0 below ,len
                 for ,e = (,func-aref ,array ,i)
                 do
                   (setf , index (the mem-size (funcall msize ,e ,index)))))))

      (cond
        ((typep array 'simple-vector)          (compute-n-words array len index svref))
        ((typep array '(simple-array fixnum))  (compute-n-words array len index))
        ((typep array '(simple-array t))       (compute-n-words array len index))
        (t                                     (compute-n-words array len index)))

      index)))
  


(defun mwrite-box/array (ptr index end-index array)
  "Write ARRAY into the memory starting at (PTR+INDEX).
Return number of words actually written.

Assumes BOX header is already written, and that enough memory is available
at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index end-index)
	   (type array array))

  (let ((mwrite #'mwrite)
        (rank (array-rank array))
        (len (array-total-size array)))

    #-(and) (log:trace ptr index array)

    (check-mem-overrun ptr index end-index (1+ rank))

    (unless (= 1 rank)
      (mset-int ptr index (the mem-int rank))
      (incf-mem-size index))

    (loop for i from 0 below rank do
         (mset-int ptr index (the mem-int (array-dimension array i)))
         (incf-mem-size index))

    (macrolet
        ((loop-mwrite-array ()
            (with-gensyms (i e)
              `(loop for ,i from 0 below len
                  for ,e = (row-major-aref array ,i) do
                    (setf index (the mem-size (funcall mwrite ptr index end-index ,e)))))))
         
      (cond
        ((typep array '(simple-array fixnum)) (loop-mwrite-array))
        ((typep array '(simple-array t))      (loop-mwrite-array))
        (t                                    (loop-mwrite-array))))

    index))


(defun mread-box/array (ptr index end-index)
  "Read an array from the memory starting at (PTR+INDEX) and return it.
Also returns number of words actually read as additional value.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (let ((rank (mget-int ptr index))
        (len 1))
    (declare (type mem-int rank len))

    (check-array-rank ptr index 'array rank)
    (check-mem-length ptr index end-index rank)

    (let* ((dimensions
            (loop for i from 0 below rank collect
                 (let ((len-i (mget-int ptr (incf-mem-size index))))
                   (setf len (the mem-int (* len len-i)))
                   len-i)))
                  
           (array (the (simple-array t) (make-array (the list dimensions))))
           (mread #'mread))

      (incf-mem-size index)

      (loop for i from 0 below len
	 do (multiple-value-bind (e e-index) (funcall mread ptr index end-index)
	      (setf (row-major-aref array i) e
                    index (the mem-size e-index))))

      (values array index))))
