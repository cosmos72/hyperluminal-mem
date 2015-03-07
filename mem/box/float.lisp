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
;;;;    boxed    SINGLE-FLOATs and DOUBLE-FLOATs                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline box-words/sfloat box-words/dfloat))

(defconstant +box-words/sfloat+ +sfloat/words+)
(defconstant +box-words/dfloat+ +dfloat/words+)

(defun box-words/sfloat (index value)
  "Return the number of words needed to store single-float VALUE in memory, not including BOX header."
  (declare (ignore value)
           (type mem-size index))
  (mem-size+ index +box-words/sfloat+))


(defun box-words/dfloat (index value)
  "Return the number of words needed to store a BOX containing double-float VALUE in memory."
  (declare (ignore value)
           (type mem-size index))
  (mem-size+ index +box-words/dfloat+))
  


(defun mwrite-box/sfloat (ptr index end-index value)
  "Write single-float VALUE into the memory starting at (+ PTR INDEX).
Assumes BOX header is already written.

ABI: single-float is stored raw (usually means IEEE format)"
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type single-float value))

  (let1 n-words +box-words/sfloat+
    (check-mem-overrun ptr index end-index n-words)

    (mset-t value :sfloat ptr index)
    (mem-size+ index n-words)))


(defun mwrite-box/dfloat (ptr index end-index value)
  "Write double-float VALUE into the memory starting at (+ PTR INDEX).
Assumes BOX header is already written.

ABI: double-float is stored raw (usually means IEEE format)"
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type double-float value))

  (let1 n-words +box-words/dfloat+
    (check-mem-overrun ptr index end-index n-words)
    
    (mset-t value :dfloat ptr index)
    (mem-size+ index n-words)))


(defun mread-box/sfloat (ptr index end-index)
  "Read a single-float from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (let1 n-words +box-words/sfloat+
    (check-mem-length ptr index end-index n-words)

    (values
     (the single-float (mget-t :sfloat ptr index))
     (mem-size+ index n-words))))


(defun mread-box/dfloat (ptr index end-index)
  "Read a double-float from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (let1 n-words +box-words/dfloat+
    (check-mem-length ptr index end-index n-words)

    (values
     (the double-float (mget-t :dfloat ptr index))
     (mem-size+ index n-words))))
