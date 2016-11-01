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
;;;;    boxed    RATIOs                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun msize-box/ratio (index value)
  "Return the number of words needed to store a BOX containing ratio VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (type ratio value)
           (type mem-size index))

  (msize* index (numerator value) (denominator value)))


(defun mwrite-box/ratio (ptr index end-index value)
  "Write ratio VALUE into the memory starting at (PTR+INDEX).
Assumes BOX header is already written.

ABI: Writes numerator, then denominator."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type ratio value))

  (let ((index (mwrite ptr index end-index (numerator value))))

    (mwrite ptr index end-index (denominator value))))



(defun mread-box/ratio (ptr index end-index)
  "Read a ratio from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header is already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (multiple-value-bind (numerator index) (mread ptr index end-index)
    (multiple-value-bind (denominator index) (mread ptr index end-index)
      (values
       (/ (the integer numerator) (the integer denominator))
       index))))
