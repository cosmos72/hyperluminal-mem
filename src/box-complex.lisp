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


(deftype complex-sfloat   () '(complex single-float))
(deftype complex-dfloat   () '(complex double-float))
(deftype complex-rational () '(complex rational))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed    COMPLEXes                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline box-words/complex-sfloat))

(defun box-words/complex-sfloat (&optional value)
  "Return the number of words needed to store a complex-sfloat VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (ignore value))

  (* 2 (box-words/sfloat)))

  
(defun mwrite-box/complex-sfloat (ptr index end-index value)
  "Reuse the memory starting at (PTR+INDEX) and write complex-sfloat VALUE into it.
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type complex-sfloat value))

  (let* ((n-words-real (box-words/sfloat))
         (n-words-imag (box-words/sfloat))
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
  
  (let* ((n-words-real (box-words/sfloat))
         (n-words-imag (box-words/sfloat))
         (n-words (+ n-words-real n-words-imag)))
    (check-mem-length ptr index end-index n-words)

    (values
     (complex
      (the single-float (mget-t :sfloat ptr index))
      (the single-float (mget-t :sfloat ptr (mem-size+ index n-words-real))))

     (mem-size+ index n-words))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline box-words/complex-dfloat))

(defun box-words/complex-dfloat (&optional value)
  "Return the number of words needed to store a complex-dfloat VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (ignore value))

  (* 2 (box-words/dfloat)))

  
(defun mwrite-box/complex-dfloat (ptr index end-index value)
  "Reuse the memory starting at (PTR+INDEX) and write complex-dfloat VALUE into it.
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type complex-dfloat value))

  (let* ((n-words-real (box-words/dfloat))
         (n-words-imag (box-words/dfloat))
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
  
  (let* ((n-words-real (box-words/dfloat))
         (n-words-imag (box-words/dfloat))
         (n-words (+ n-words-real n-words-imag)))
    (check-mem-length ptr index end-index n-words)

    (values
     (complex
      (the double-float (mget-t :dfloat ptr index))
      (the double-float (mget-t :dfloat ptr (mem-size+ index n-words-real))))

     (mem-size+ index n-words))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun box-words/complex-rational (value)
  "Return the number of words needed to store a complex-rational VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (type complex-rational value))

  (let ((n-words (+ (detect-n-words (realpart value))
                    (detect-n-words (imagpart value)))))
    (unless (<= n-words +mem-box/max-payload-words+)
      (error "HYPERLUMINAL-DB: complex-rational too large for object store,
it requires ~S words, maximum supported is ~S words"
             (1+ n-words) +mem-box/max-words+))
    (the mem-size n-words)))
     

  
(defun mwrite-box/complex-rational (ptr index end-index value)
  "Reuse the memory starting at (PTR+INDEX) and write complex-rational VALUE into it.
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type complex-rational value))

  (let ((mwrite #'mwrite))
    (setf index (funcall mwrite ptr index end-index (realpart value)))
    (funcall mwrite ptr index end-index (imagpart value))))


(defun mread-box/complex-rational (ptr index end-index)
  "Read a complex-rational from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header is already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (let ((mread #'mread))
    (multiple-value-bind (realpart index) (funcall mread ptr index end-index)
      (multiple-value-bind (imagpart index) (funcall mread ptr index end-index)
        (values
         (complex realpart imagpart)
         index)))))

