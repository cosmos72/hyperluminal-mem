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
;;;;    boxed list                                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-words/list (list)
  "Return the number of words needed to store LIST in mmap memory, not including BOX header."
  (declare (type list list))

  ;; -1 to store list length
  (let ((detect-n-words #'detect-n-words)
	(words-left (1- +mem-box/max-payload-words+)))
    (declare (type mem-size words-left))
    
    ;; count downward: easier to check for overflows
    (labels
	((new-words-left (words-left e)
	   (let ((e-len (the mem-size (funcall detect-n-words e))))
	     (unless (>= words-left e-len)
	       (error "HYPERLUMINAL-DB: list too large for object store,
it requires more space than the maximum supported ~S words"
		      +mem-box/max-payload-words+ ))
	     (mem-size- words-left e-len))))

      ;; note: list may end with a dotted pair
      (loop for cons = list then (rest cons)
	 while (consp cons)
	 for e = (first cons)
	 do
	   (setf words-left (new-words-left words-left e))
	 finally
	   (when cons
	     (setf words-left (new-words-left words-left cons)))))

    (mem-size- +mem-box/max-payload-words+ words-left)))

       


(defun mwrite-box/list (ptr index list)
  "Write LIST into the memory starting at (PTR+INDEX).
Return number of words actually written.

Assumes BOX header is already written, and that enough memory is available
at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index)
           (type list list))

  ;; note: written length is (length list) for proper lists,
  ;; but is (lognot (length list)) for lists ending with a dotted pair

  (let ((orig-index index) ;; we write list length later
	(len 0)
    	(mwrite #'mwrite))
    (declare (type mem-int len))

    (incf-mem-size index) ;; leave space for list length

    (loop for cons = list then (rest cons)
       while (consp cons)
       for e = (first cons)
       do
	 (incf index (the mem-size (funcall mwrite ptr index e)))
	 (incf len)
       finally
	 (when cons
	   (incf index (the mem-size (funcall mwrite ptr index cons)))
	   (incf len)
	   ;; dotted pair: store (lognot len)
	   (setf len (lognot len))))

    ;; finally write len
    (mset-int ptr orig-index len)

    ;; return number of words actually written, including BOX header
    (mem-size+ +mem-box/header-words+
	       (mem-size- index orig-index))))



(defun mread-box/list (ptr index)
  "Read a list from the memory starting at (PTR+INDEX) and return it.
Also returns number of words actually read as additional value.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (let* ((orig-index  index)
	 (len         (mget-int ptr index))
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
	     (multiple-value-bind (e e-len) (funcall mread ptr index)
	       (incf-mem-size index e-len)
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
     (mem-size+ +mem-box/header-words+
		(mem-size- index orig-index)))))
