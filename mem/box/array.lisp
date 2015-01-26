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

(enable-#?-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed array                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-words/array (index array)
  "Return the number of words needed to store ARRAY in mmap memory,
not including BOX header."
  (declare (type array array))

  (let* ((rank (array-rank array))
         (len (array-total-size array)))

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
	((compute-n-words (array len index &optional (el-type t))
	   (with-gensyms (i e)
	     `(loop for ,i from 0 below ,len
                 for ,e = (row-major-aref ,array ,i)
                 do
                   (setf ,index (msize ,index (the ,el-type ,e)))))))

      (cond
        ((typep array '(simple-array fixnum))  (compute-n-words array len index))
        ((typep array '(simple-array t))       (compute-n-words array len index))
        (t                                     (compute-n-words array len index)))

      index)))
  
(defmacro %the-array (a type simple)
  `(the (,(if simple 'simple-array 'array)
          ,(if type type '*)
          *)
     ,a))

         
(defmacro %mwrite-array-unboxed (a type)
  (with-gensyms (i e)
    `(progn
       (check-mem-overrun ptr index end-index len)
       (loop for ,i from 0 below len
          for ,e = (row-major-aref ,a ,i) do
            (mset-unboxed ptr index (the ,type ,e))
            (incf-mem-size index)))))


(defmacro %mwrite-array-t (a type)
  (with-gensyms (i e)
    `(loop for ,i from 0 below len
        for ,e = (row-major-aref ,a ,i)
        do (setf index (mwrite ptr index end-index
                               (the ,type ,e))))))


(defmacro %mwrite-array (a type simple)
  `(cond
     ((subtypep ,type 'mem-int)
      (if (subtypep 'mem-int ,type)
          (%mwrite-array-unboxed (%the-array ,a mem-int ,simple) mem-int)
          (%mwrite-array-unboxed (%the-array ,a   *     ,simple) mem-int)))

     ((eq ,type 'character)
      (%mwrite-array-unboxed (%the-array ,a character ,simple) character))

     ((eq ,type 'base-char)
      (%mwrite-array-unboxed (%the-array ,a base-char ,simple) base-char))

     #?+hldb/sfloat/inline
     ((eq ,type 'single-float)
      (%mwrite-array-unboxed (%the-array ,a single-float ,simple) single-float))

     #?+hldb/dfloat/inline
     ((eq ,type 'double-float)
      (%mwrite-array-unboxed (%the-array ,a double-float ,simple) double-float))

     ((eq ,type t)
      (%mwrite-array-t (%the-array ,a t ,simple) t))

     (t
      (%mwrite-array-t (%the-array ,a * ,simple) t))))

      


(defun mwrite-box/array (ptr index end-index array)
  "Write ARRAY into the memory starting at (PTR+INDEX).
Return number of words actually written.

Assumes BOX header is already written, and that enough memory is available
at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index end-index)
	   (type array array))

  (let ((rank (array-rank array))
        (len  (array-total-size array))
        (type (array-element-type array))
        (simple (typep array 'simple-array)))

    #-(and) (log:trace ptr index array)

    (check-mem-overrun ptr index end-index (1+ rank))

    (mset-int ptr index (the mem-int rank))
    (incf-mem-size index)

    (loop for i from 0 below rank do
         (mset-int ptr index (the mem-int (array-dimension array i)))
         (incf-mem-size index))

    (if simple
        (%mwrite-array array type t)
        (%mwrite-array array type nil)))
  index)


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
