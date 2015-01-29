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


(defmacro %the-array (a type simple)
  (if simple
      `(the (simple-array ,(if type type '*) *) ,a)
      `(the (and (array ,(if type type '*) *)
                 (not simple-array))
            ,a)))

         
(defmacro %loop-array-unboxed (func a type)
  (ecase func
    (mwrite
     (with-gensyms (i e)
       `(progn
          (check-mem-overrun ptr index end-index len)
          (loop for ,i from 0 below len
             for ,e = (row-major-aref ,a ,i) do
               (mset-unboxed ptr index (the ,type ,e))
               (incf-mem-size index)))))
    (msize
     `(incf-mem-size index len))))
     


(defmacro %loop-array-t (func a type)
  (with-gensyms (i e)
    `(loop for ,i from 0 below len
        for ,e = (row-major-aref ,a ,i)
        do (setf index
                 ,(ecase func
                    (mwrite `(mwrite ptr index end-index (the ,type ,e)))
                    (msize  `(msize index (the ,type ,e))))))))


(defmacro %loop-array (func a type simple)
  `(cond
     ((mem-int=integer-type ,type)
      (%loop-array-unboxed ,func (%the-array1 ,a mem-int ,simple) mem-int))

     #?+hlmem/mem-int>fixnum
     ((eq ,type 'fixnum)
      (%loop-array-unboxed ,func (%the-array1 ,a fixnum  ,simple) fixnum))
       
     ((mem-int>integer-type ,type)
      (%loop-array-unboxed ,func (%the-array1 ,a   *     ,simple) mem-int))

     ((eq 'character ,type)
      (%loop-array-unboxed ,func (%the-array ,a character ,simple) character))

     ((eq 'base-char ,type)
      (%loop-array-unboxed ,func (%the-array ,a base-char ,simple) base-char))

     #?+hlmem/sfloat/inline
     ((eq 'single-float ,type)
      (%loop-array-unboxed ,func (%the-array ,a single-float ,simple) single-float))

     #?+hlmem/dfloat/inline
     ((eq 'double-float ,type)
      (%loop-array-unboxed ,func (%the-array ,a double-float ,simple) double-float))

     ((eq t ,type)
      (%loop-array-t ,func (%the-array ,a t ,simple) t))

     (t
      (%loop-array-t ,func (%the-array ,a * ,simple) t))))

      


(defun box-words/array (index array)
  "Return the number of words needed to store ARRAY in mmap memory,
not including BOX header."
  (declare (type mem-size index)
	   (type array array))

  (let ((rank (array-rank array))
        (len  (array-total-size array))
        (type (array-element-type array))
        (simple (typep array 'simple-array)))

    #-(and) (log:trace ptr index array)

    (unless (< rank (- +most-positive-int+ index))
      (error "HYPERLUMINAL-MEM: array has too many dimensions for object store.
it has rank ~S, but at most ~S words are available at index ~S"
	     rank (- +most-positive-int+ index 1) index))

    ;; 1 word to store the rank, +1 per dimension
    (incf-mem-size index (mem-size+1 rank))

    (unless (<= len (- +most-positive-int+ index))
      (error "HYPERLUMINAL-MEM: array too large for object store.
it contains ~S elements, but at most ~S words are available at index ~S"
	     len (- +most-positive-int+ index) index))

    (if simple
        (%loop-array msize array type t)
        ;; specializing on the element-type of non-simple arrays
        ;; is usually not needed, as they are slow in any case
        (%loop-array-t msize vector t)))
  index)

  

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
        (%loop-array mwrite array type t)
        ;; specializing on the element-type of non-simple arrays
        ;; is usually not needed, as they are slow in any case
        (%loop-array-t mwrite vector t)))
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
            (loop for i from 0 below rank
               for len-i = (mget-int ptr (incf-mem-size index))
               do 
                 (setf len (the mem-int (* len len-i)))
               collect len-i
               finally
                 (incf-mem-size index)
                 (check-mem-length ptr index end-index len)))
           
           (array (the (simple-array t) (make-array (the list dimensions)))))

      (loop for i from 0 below len
	 do (multiple-value-bind (e e-index) (mread ptr index end-index)
	      (setf (row-major-aref array i) e
                    index (the mem-size e-index))))

      (values array index))))
