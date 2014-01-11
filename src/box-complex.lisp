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

(defun box-words/complex-sfloat (value)
  "Return the number of words needed to store a complex-sfloat VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (type complex-sfloat value))

  (error "TODO"))

  
(defun mwrite-box/complex-sfloat (ptr index value)
  "Reuse the memory starting at (PTR+INDEX) and write complex-sfloat VALUE into it.
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index)
           (type complex-sfloat value))

  (error "TODO"))


(defun mread-box/complex-sfloat (ptr index)
  "Read a complex-sfloat from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (error "TODO"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun box-words/complex-dfloat (value)
  "Return the number of words needed to store a complex-dfloat VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (type complex-dfloat value))

  (error "TODO"))

  
(defun mwrite-box/complex-dfloat (ptr index value)
  "Reuse the memory starting at (PTR+INDEX) and write complex-dfloat VALUE into it.
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index)
           (type complex-dfloat value))

  (error "TODO"))


(defun mread-box/complex-dfloat (ptr index)
  "Read a complex-dfloat from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (error "TODO"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun box-words/complex-rational (value)
  "Return the number of words needed to store a complex-rational VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (type complex-rational value))

  (error "TODO"))

  
(defun mwrite-box/complex-rational (ptr index value)
  "Reuse the memory starting at (PTR+INDEX) and write complex-rational VALUE into it.
Assumes BOX header is already written.

ABI: Writes real part, then imaginary part."
  (declare (type maddress ptr)
           (type mem-size index)
           (type complex-rational value))

  (error "TODO"))


(defun mread-box/complex-rational (ptr index)
  "Read a complex-rational from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header is already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (error "TODO"))

