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
;;;;    boxed vector                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
(deftype array1 (&optional (element-type '*))
  `(and (array ,element-type (*))
        (not (or string base-string bit-vector))))

(defmacro %the-array1 (a type simple)
  `(the (,(if simple 'simple-array 'array)
          ,(if type type '*)
          (*))
     ,a))

(defmacro %loop-array1-unboxed (func a type)
  (ecase func
    (mwrite
     (with-gensym e
       `(progn
          (check-mem-overrun ptr index end-index len)
          (loop for ,e across ,a do
               (mset-unboxed ptr index (the ,type ,e))
               (incf-mem-size index)))))
    (msize
     `(incf-mem-size index len))))


(defmacro %loop-array1-t (func a type)
  (with-gensym e
    `(loop for ,e across ,a do
          (setf index
                ,(ecase func
                   (mwrite `(mwrite ptr index end-index (the ,type ,e)))
                   (msize  `(msize index (the ,type ,e))))))))


(defmacro %loop-array1 (func a type simple)
  `(cond
     ((mem-int=integer-type ,type)
      (%loop-array1-unboxed ,func (%the-array1 ,a mem-int ,simple) mem-int))

     ,@(when +mem-int>fixnum+
         `(((eq ,type 'fixnum)
            (%loop-array1-unboxed ,func (%the-array1 ,a fixnum ,simple) fixnum))))
       
     ((mem-int>integer-type ,type)
      (%loop-array1-unboxed ,func (%the-array1 ,a   *     ,simple) mem-int))

     #?+hldb/sfloat/inline
     ((eq 'single-float ,type)
      (%loop-array1-unboxed ,func (%the-array1 ,a single-float ,simple) single-float))

     #?+hldb/dfloat/inline
     ((eq 'double-float ,type)
      (%loop-array1-unboxed ,func (%the-array1 ,a double-float ,simple) double-float))

     ((eq t ,type)
      (%loop-array1-t ,func (%the-array1 ,a t ,simple) t))

     (t
      (%loop-array1-t ,func (%the-array1 ,a * ,simple) t))))



(defun box-words/vector (index vector)
  "Return the number of words needed to store VECTOR in mmap memory,
not including BOX header."
  (declare (type array1 vector)
           (type mem-size index))

  (let ((len  (array-total-size vector))
        (type (array-element-type vector))
        (simple (typep vector 'simple-array)))

    #-(and) (log:trace ptr index array)

    (unless (< len (- +most-positive-int+ index))
      (error "HYPERLUMINAL-MEM: vector too large for object store.
it contains ~S elements, but at most ~S words are available at index ~S"
	     len (- +most-positive-int+ index 1) index))

    ;; 1 word to store vector length
    (incf-mem-size index)

    (if simple
        (%loop-array1 msize vector type t)
        (%loop-array1 msize vector type nil)))
  index)



         



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
        (%loop-array1 mwrite vector type t)
        (%loop-array1 mwrite vector type nil)))
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
