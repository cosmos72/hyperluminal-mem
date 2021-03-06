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
;;;;    boxed list                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun msize-box/list (index list)
  "Return the number of words needed to store LIST in mmap memory, not including BOX header."
  (declare (type list list))

  ;; +1 to store list length
  (incf-mem-size index)

  ;; note: list may end with a dotted pair
  (loop for cons = list then (rest cons)
     while (consp cons)
     do
       (setf index (msize index (first cons)))
     finally
       (when cons
         (setf index (msize index cons))))
  index)

       


(defun mwrite-box/list (ptr index end-index list)
  "Write LIST into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after written list.

Assumes BOX header is already written, and that enough memory is available
at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type list list))

  ;; note: written length is (length list) for proper lists,
  ;; but is (lognot (length list)) for lists ending with a dotted pair

  (check-mem-overrun ptr index end-index 1)

  (let ((orig-index index) ;; we write list length later
	(len 0)
    	(mwrite #'mwrite))
    (declare (type mem-int len))

    (incf-mem-size index) ;; leave space for list length

    (loop for cons = list then (rest cons)
       while (consp cons)
       for e = (first cons)
       do
	 (setf index (the mem-size (funcall mwrite ptr index end-index e)))
	 (incf len)
       finally
	 (when cons
	   (setf index (the mem-size (funcall mwrite ptr index end-index cons)))
	   (incf len)
	   ;; dotted pair: store (lognot len)
	   (setf len (lognot len))))

    ;; finally write list length
    (mset-int ptr orig-index len)
    index))



(defun mread-box/list (ptr index end-index)
  "Read a list from the memory starting at (PTR+INDEX) and return it.
Also returns as additional value the INDEX pointing to immediately after the list read.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (check-mem-length ptr index end-index 1)

  (let* ((len         (mget-int ptr index))
	 (dotted-pair (< len 0))
	 (list        nil)
	 (prev        nil)
	 (tail        nil))
	   

    (when dotted-pair
      (setf len (lognot len)))

    (incf-mem-size index)

    (unless (zerop len)
      (setf list (stmx.lang::cons^ nil nil)
	    tail list)

      (let ((mread #'mread))
	(loop for i from 0 below len
	   do
	     (multiple-value-bind (e e-index) (funcall mread ptr index end-index)
	       (setf index (the mem-size e-index))
	       (let ((cons (stmx.lang::cons^ e nil)))
		 (setf prev tail
		       (rest tail) cons
		       tail cons)))))

      ;; adjust for dotted pair
      (when (and dotted-pair prev tail)
	(setf (rest prev) (first tail))
	(stmx.lang::free-cons^ tail)))

    (values
     (prog1 (rest list)
       (stmx.lang::free-cons^ list))
     index)))
