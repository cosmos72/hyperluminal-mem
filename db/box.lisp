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


;;;; boxed values, i.e. mem-box, are variable-length mmap areas
;;;; used to store all kind of CL built-in types that do not fit a single CPU word:
;;;; bignums, ratios, single-floats and double-floats, complexes,
;;;; pathnames, cons cells and lists, vectors, arrays, strings and hash-tables.
;;;;
;;;; mem-boxes are allocates in multiples of 4 (actually +mem-box/min-words+) CPU words,
;;;; and they contain a 2 CPU-word header followed by type-specific payload:
;;;;
;;;;   word 0: tag = type. it uses a different coding than pointer tags (see +mem-box/...+ constants)
;;;;           value = pointer to owner. used by GC.
;;;;
;;;;   word 1: tag = available for type-specific data, for example sign bits
;;;;           value = number of allocated words / 4. also counts the header (i.e. words 0 and 1)
;;;;
;;;;   word 2... : payload. depends on type


(declaim (inline box-index   (setf box-index)
                 box-n-words (setf box-n-words)
                 box-value   (setf box-value)
                 box-next    (setf box-next)
                 reuse-box))


;; wrapper for values that cannot be stored as unboxed
(deftype box () 'cons)

(defun make-box (index n-words &optional value)
  "Create a new box to wrap VALUE. Assumes VALUE will be stored at INDEX in memory store."
  (declare (type mem-size index n-words))
  `(,value ,index . ,n-words))

(defun box-value (box)
  (declare (type box box))
  (first box))

(defun (setf box-value) (value box)
  (declare (type box box))
  (setf (first box) value))

(defun box-index (box)
  (declare (type box box))
  (the mem-size (second box)))

(defun (setf box-index) (index box)
  (declare (type box box)
           (type mem-size index))
  (setf (second box) index))

(defun box-n-words (box)
  (declare (type box box))
  (the mem-size (rest (rest box))))

(defun (setf box-n-words) (n-words box)
  (declare (type box box)
           (type mem-size n-words))
  (setf (rest (rest box)) n-words))

(defun reuse-box (box index n-words value)
  "Set BOX slots to specified values. Return BOX."
  (declare (type box box)
           (type mem-size index n-words))
  (setf (box-value box) value)
  (let ((tail (rest box)))
    (setf (first tail)  index
          (rest  tail)  n-words))
  box)
    

  
  

#|
(declaim (inline %make-box setf-box-value-index-n-words))
(defstruct (box (:constructor %make-box))
  (index   0 :type mem-size)
  (n-words 0 :type mem-size)
  (value   nil))


(defun make-box (index n-words &optional value)
  "Create a new box to wrap VALUE. Assumes VALUE will be stored at INDEX in memory store."
  (declare (type mem-size index n-words))
  (%make-box :index index :n-words n-words :value value))

(defun reuse-box (box index n-words value)
  (setf (box-value   box) value
        (box-index   box) index
        (box-n-words box) n-words)
  box)
|#

(defun box-next (box)
  (declare (type box box))
  (box-value box))

(defun (setf box-next) (value box)
  (declare (type box box))
  (setf (box-value box) value))

           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-null? (box)
  "Return T if box is full of zeroes, for example when loaded from a newly created file."
  (declare (type box box))
  (and (<     (box-index   box) +mem-box/min-words+)
       (zerop (box-n-words box))
       (null  (box-value   box))))


(declaim (inline mwrite-fbox-next mwrite-fbox-n-words))

(defun mwrite-fbox-next (ptr box)
  "Write the NEXT slot of a free box into mmap memory starting
at (+ PTR (box-index BOX))"

  (declare (type maddress ptr)
           (type box box))

  (let* ((index (box-index box))
         (next  (box-next box))
         (next-index (if next (box-index next) 0)))

    (mset-fulltag-and-value ptr (mem-size-1 index) +mem-unallocated+ (size->box-pointer next-index))))


(defun mwrite-fbox-n-words (ptr box &optional (n-words (box-n-words box)))
  "Write the N-WORDS slot of a free box into mmap memory starting
at (+ PTR (box-index BOX))"

  (let ((index (box-index box)))
    (mset-fulltag-and-value ptr index +mem-unallocated+ (size->box-pointer n-words))))



(defun mwrite-box/free (ptr box)
  "Write a free box into mmap memory starting at (+ PTR (box-index BOX))"
  (declare (type maddress ptr)
           (type box box))

  (mwrite-fbox-next    ptr box)
  (mwrite-fbox-n-words ptr box))







(declaim (inline mread-fbox-next mread-fbox-n-words))

(defun mread-fbox-next (ptr index)
  "Read the NEXT slot of a free box from mmap memory starting at PTR+INDEX"
  (declare (type maddress ptr)
           (type mem-size index))

  (mem-size+ +mem-box/min-payload-words+
             (box-pointer->size (mget-value ptr (mem-size-1 index)))))


(defun mread-fbox-n-words (ptr index)
  "Read N-WORDS from box in mmap memory starting at (PTR+INDEX) and return it."
  (declare (type maddress ptr)
           (type mem-size index))

  (box-pointer->size (mget-value ptr index)))


(defun mread-box/free (ptr index)
  "Read a free box from mmap memory starting at (PTR+INDEX) and return it.
Note: NEXT slot of returned object always contains NIL,
      instead NEXT value stored in mmap is returned as multiple values."

  (declare (type maddress ptr)
           (type mem-size index))

  (let* ((next-index (mread-fbox-next    ptr index))
         (n-words    (mread-fbox-n-words ptr index)))
    (values
     (make-box index n-words)
     (the mem-size next-index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun !mzero-box (ptr box)
  "Fill an allocated box with zeroes."
  (declare (type maddress ptr)
           (type box box))
  (let* ((index   (box-index box))
         (n-words (box-n-words box))
         (start   index)
         (end     (mem-size+ start n-words)))

    (!mzero-words ptr start end)))



(defun !mzero-fbox (ptr box)
  "Fill a free box with zeroes."
  (declare (type maddress ptr)
           (type box box))
  (let* ((index   (box-index box))
         (n-words (box-n-words box))
         ;; free boxes are written at the end of the free mmap area they represent!
         (start   (mem-size- index (mem-size- n-words +mem-box/header-words+)))
         (end     (mem-size+ start n-words)))
    
    (!mzero-words ptr start end)))


