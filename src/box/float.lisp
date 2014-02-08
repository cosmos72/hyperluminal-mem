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

(declaim (inline box-words/sfloat box-words/dfloat))

(defun box-words/sfloat (value index)
  "Return the number of words needed to store single-float VALUE in memory, not including BOX header."
  (declare (ignore value)
           (type mem-size index))
  (mem-size+ index #.(ceiling +msizeof-sfloat+ +msizeof-word+))) ;; round up

(defun box-words/dfloat (value index)
  "Return the number of words needed to store a BOX containing double-float VALUE in memory."
  (declare (ignore value)
           (type mem-size index))
  (mem-size+ index #.(ceiling +msizeof-dfloat+ +msizeof-word+))) ;; round up
  


(defun mwrite-box/sfloat (ptr index end-index value)
  "Write single-float VALUE into the memory starting at (+ PTR INDEX).
Assumes BOX header is already written.

ABI: single-float is stored raw (usually means IEEE format)"
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type single-float value))

  (let ((n-words (box-words/sfloat)))
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

  (let ((n-words (box-words/dfloat)))
    (check-mem-overrun ptr index end-index n-words)

    (mset-t value :dfloat ptr index)
    (mem-size+ index n-words)))


(defun mread-box/sfloat (ptr index end-index)
  "Read a single-float from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (let ((n-words (box-words/sfloat)))
    (check-mem-length ptr index end-index n-words)

    (values
     (the single-float (mget-t :sfloat ptr index))
     (mem-size+ index n-words))))


(defun mread-box/dfloat (ptr index end-index)
  "Read a double-float from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (let ((n-words (box-words/dfloat)))
    (check-mem-length ptr index end-index n-words)

    (values
     (the double-float (mget-t :dfloat ptr index))
     (mem-size+ index n-words))))
