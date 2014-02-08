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


(in-package :hyperluminal-mem)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed    RATIOs                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-words/ratio (value index)
  "Return the number of words needed to store a BOX containing ratio VALUE in mmap memory.
Does not count the space needed by BOX header."
  (declare (type ratio value)
           (type mem-size index))

  (let1 index (mdetect-size (numerator value) index)

    (mdetect-size (denominator value) index)))


(defun mwrite-box/ratio (ptr index end-index value)
  "Write ratio VALUE into the memory starting at (PTR+INDEX).
Assumes BOX header is already written.

ABI: Writes numerator, then denominator."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type ratio value))

  (let1 index (mwrite ptr index end-index (numerator value))

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
