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


(deftype complex-sfloat   () '(complex single-float))
(deftype complex-dfloat   () '(complex double-float))
(deftype complex-rational () '(complex rational))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed    COMPLEXes                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline msize-box/complex-sfloat))

(defconstant +msize-box/complex-sfloat+ (* 2 +msize-box/sfloat+))

(defun msize-box/complex-sfloat (index value)
  "Return the number of words needed to store a complex-sfloat VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (ignore value)
           (type mem-size index))

  (mem-size+ index +msize-box/complex-sfloat+))

  
(defun mwrite-box/complex-sfloat (ptr index end-index value)
  "Reuse the memory starting at (PTR+INDEX) and write complex-sfloat VALUE into it.
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type complex-sfloat value))

  (let* ((n-words-real +msize-box/sfloat+)
         (n-words-imag +msize-box/sfloat+)
         (n-words (+ n-words-real n-words-imag)))
    (check-mem-overrun ptr index end-index n-words)

    (mset-t (realpart value) :sfloat ptr index)
    (incf-mem-size index n-words-real)
    (mset-t (imagpart value) :sfloat ptr index)
    (incf-mem-size index n-words-imag)))


(defun mread-box/complex-sfloat (ptr index end-index)
  "Read a complex-sfloat from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (let* ((n-words-real +msize-box/sfloat+)
         (n-words-imag +msize-box/sfloat+)
         (n-words (+ n-words-real n-words-imag)))
    (check-mem-length ptr index end-index n-words)

    (values
     (complex
      (the single-float (mget-t :sfloat ptr index))
      (the single-float (mget-t :sfloat ptr (incf-mem-size index n-words-real))))

     (incf-mem-size index n-words-imag))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline msize-box/complex-dfloat))

(defconstant +msize-box/complex-dfloat+ (* 2 +msize-box/dfloat+))

(defun msize-box/complex-dfloat (index value)
  "Return the number of words needed to store a complex-dfloat VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (ignore value)
           (type mem-size index))

  (mem-size+ index +msize-box/complex-dfloat+))

  
(defun mwrite-box/complex-dfloat (ptr index end-index value)
  "Reuse the memory starting at (PTR+INDEX) and write complex-dfloat VALUE into it.
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type complex-dfloat value))

  (let* ((n-words-real +msize-box/dfloat+)
         (n-words-imag +msize-box/dfloat+)
         (n-words (+ n-words-real n-words-imag)))
    (check-mem-overrun ptr index end-index n-words)

    (mset-t (realpart value) :dfloat ptr index)
    (incf-mem-size index n-words-real)
    (mset-t (imagpart value) :dfloat ptr index)
    (incf-mem-size index n-words-imag)))


(defun mread-box/complex-dfloat (ptr index end-index)
  "Read a complex-dfloat from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (let* ((n-words-real +msize-box/dfloat+)
         (n-words-imag +msize-box/dfloat+)
         (n-words (+ n-words-real n-words-imag)))
    (check-mem-length ptr index end-index n-words)

    (values
     (complex
      (the double-float (mget-t :dfloat ptr index))
      (the double-float (mget-t :dfloat ptr (incf-mem-size index n-words-real))))

     (incf-mem-size index n-words-imag))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun msize-box/complex-rational (index value)
  "Return the number of words needed to store a complex-rational VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (type complex-rational value))

  (msize* index (realpart value) (imagpart value)))
     

  
(defun mwrite-box/complex-rational (ptr index end-index value)
  "Write complex-rational VALUE into the memory starting at (PTR+INDEX).
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type complex-rational value))

  (mwrite* ptr index end-index (realpart value) (imagpart value)))


(defun mread-box/complex-rational (ptr index end-index)
  "Read a complex-rational from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header is already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (with-mread* (realpart imagpart index) (ptr index end-index)
    (values
     (complex realpart imagpart)
     index)))

