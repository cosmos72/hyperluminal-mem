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
;;;;    dispatcher for all boxed types                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mwrite-box (ptr index n-words value)
  "Write a boxed value into the memory starting at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type t value))

  (etypecase value
    (integer      (mwrite-box/bignum     ptr index n-words value))
    (ratio        (mwrite-box/ratio      ptr index n-words value))
    (single-float (mwrite-box/sfloat     ptr index n-words value))
    (double-float (mwrite-box/dfloat     ptr index n-words value))
    (complex      (mwrite-box/complex    ptr index n-words value))
    (list         (mwrite-box/list       ptr index n-words value))
    (array        (mwrite-box/array      ptr index n-words value))
    (hash-table   (mwrite-box/hash-table ptr index n-words value))
    (pathname     (mwrite-box/pathname   ptr index n-words value))))

             

(defun mwrite-box/box (ptr box)
  "Write a boxed value into the memory starting at (PTR+(box-index BOX))."
  (declare (type maddress ptr)
           (type box box))

  (mwrite-box ptr (box-index box) (box-n-words box) (box-value box)))


(defun mread-box (ptr index)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the value and number of words allocated for it as multiple values."
  (declare (type maddress ptr)
           (type mem-size index))

  ;; read BOX header
  (multiple-value-bind (n-words boxed-type) (mread-box/header ptr index)
    (values
     (case boxed-type
       (#.+mem-box-unallocated+ (error "attempt to read an unallocated area at mmap address ~S + ~S"
                                       ptr index))
       (#.+mem-box-bignum+           (mread-box/bignum           ptr index))
       (#.+mem-box-ratio+            (mread-box/ratio            ptr index))
       (#.+mem-box-sfloat+           (mread-box/sfloat           ptr index))
       (#.+mem-box-dfloat+           (mread-box/dfloat           ptr index))
       (#.+mem-box-complex-sfloat+   (mread-box/complex/sfloat   ptr index))
       (#.+mem-box-complex-dfloat+   (mread-box/complex/dfloat   ptr index))
       (#.+mem-box-complex-rational+ (mread-box/complex/rational ptr index))
       (#.+mem-box-pathname+         (mread-box/pathname         ptr index))
       (#.+mem-box-list+             (mread-box/list             ptr index))
       (#.+mem-box-hash-table+       (mread-box/hash-table       ptr index))
       (#.+mem-box-array+            (mread-box/array            ptr index))
       (#.+mem-box-vector+           (mread-box/vector           ptr index))
       (#.+mem-box-string+           (mread-box/string           ptr index))
       (#.+mem-box-base-string+      (mread-box/base-string      ptr index))
       (#.+mem-box-bit-vector+       (mread-box/bit-vector       ptr index))
       (otherwise  (error "unsupported boxed type ~S at mmap address ~S + ~S"
                          boxed-type ptr index)))
     n-words)))


(defun mread-box/box (ptr index)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the boxed value."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (multiple-value-bind (value n-words) (mread-box ptr index)
    (make-box index n-words value)))



