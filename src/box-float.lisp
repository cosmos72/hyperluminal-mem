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
;;;;    boxed    SINGLE-FLOATs and DOUBLE-FLOATs                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun box-words/sfloat (value)
  "Return the number of words needed to store single-float VALUE in memory, not including BOX header."
  (declare (type single-float value)
           (ignore value))
  #.(ceiling +msizeof-sfloat+ +msizeof-word+)) ;; round up

(defun box-words/dfloat (value)
  "Return the number of words needed to store a BOX containing double-float VALUE in memory."
  (declare (type double-float value)
           (ignore value))
  #.(ceiling +msizeof-dfloat+ +msizeof-word+)) ;; round up
  


(defun mwrite-box/sfloat (ptr index value)
  "Write single-float VALUE into the memory starting at (+ PTR INDEX).
Assumes BOX header is already written.

ABI: single-float is stored raw (usually means IEEE format)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type single-float value))

  (mset-t value :sfloat ptr index)
  t)


(defun mwrite-box/dfloat (ptr index value)
  "Write double-float VALUE into the memory starting at (+ PTR INDEX).
Assumes BOX header is already written.

ABI: double-float is stored raw (usually means IEEE format)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type double-float value))

  (mset-t value :dfloat ptr index)
  t)


(defun mread-box/sfloat (ptr index)
  "Read a single-float from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (the single-float (mget-t :sfloat ptr index)))


(defun mread-box/dfloat (ptr index)
  "Read a double-float from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (the double-float (mget-t :dfloat ptr index)))
  
