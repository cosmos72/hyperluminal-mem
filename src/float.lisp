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

(declaim (inline sfloat-words dfloat-words))

(defun sfloat-words (n)
  "Return the number of words needed to store single-float N in memory."
  (declare (ignore n))
  #.(truncate (+ +msizeof-sfloat+ +msizeof-word+ -1) ;; round up
              +msizeof-word+))

(defun dfloat-words (n)
  "Return the number of words needed to store double-float N in memory."
  (declare (ignore n))
  #.(truncate (+ +msizeof-dfloat+ +msizeof-word+ -1) ;; round up
              +msizeof-word+))

(defun box-words/sfloat (n)
  "Return the number of words needed to store a BOX containing single-float N in memory."
  (declare (type single-float n))
  (mem-size+ +mem-box/header-words+ (sfloat-words n)))

(defun box-words/dfloat (n)
  "Return the number of words needed to store a BOX containing double-float N in memory."
  (declare (type double-float n))
  (mem-size+ +mem-box/header-words+ (dfloat-words n)))
  


(defun mwrite-box/sfloat (ptr index n-words n)
  "Reuse the memory block starting at (+ PTR INDEX)
and write single-float N into it.

ABI: single-float is stored as box prefix, followed by raw single-float value"
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type single-float n))

  (setf index
        (mwrite-box/header ptr index n-words +mem-box-sfloat+))

  (mset-t n :sfloat ptr index)
  t)


(defun mwrite-box/dfloat (ptr index n-words n)
  "Reuse the memory block starting at (+ PTR INDEX)
and write double-float N into it.

ABI: double-float is stored as box prefix, followed by raw double-float value"
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type double-float n))

  (setf index
        (mwrite-box/header ptr index n-words +mem-box-dfloat+))

  (mset-t n :dfloat ptr index)
  t)


(defun mread-box/sfloat (ptr index)
  "Read a boxed single-float from the memory starting at (PTR+INDEX).
Return the single-float"
  (declare (type maddress ptr)
           (type mem-size index))
  
  ;; skip the box header
  (incf-mem-size index +mem-box/header-words+)

  (mget-t :sfloat ptr index))


(defun mread-box/dfloat (ptr index)
  "Read a boxed double-float from the memory starting at (PTR+INDEX).
Return the double-float"
  (declare (type maddress ptr)
           (type mem-size index))
  
  ;; skip the box header
  (incf-mem-size index +mem-box/header-words+)

  (mget-t :dfloat ptr index))
  
