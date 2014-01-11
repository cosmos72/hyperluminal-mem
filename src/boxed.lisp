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


(defun box-words/unallocated (value)
  (declare (ignore value))
  (error "internal error! attempt to write boxed-type +MEM-BOX/UNALLOCATED+"))

(defun mwrite-box/unallocated (ptr index value)
  (declare (ignore ptr index value))
  (error "internal error! attempt to write boxed-type +MEM-BOX/UNALLOCATED+"))

(defun mread-box/unallocated (ptr index)
  (declare (ignore ptr index))
  (error "internal error! attempt to read boxed-type +MEM-BOX/UNALLOCATED+"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    dispatchers for all boxed types                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (type vector +box-words-funcs+ +mwrite-box-funcs+  +mread-box-funcs+))

(defmacro define-global-constant (name value &optional documentation)
  `(#+(and) defparameter #-(and) define-constant-once
      ,name ,value
      ,@(when documentation `(,documentation))))


(define-global-constant +box-words-funcs+
    (let* ((syms +mem-box-symbols+)
           (array (make-array (length syms))))

      (loop for i from 0 below (length syms) do
           (setf (svref array i) (fdefinition (symbol-concat 'box-words/ (svref syms i)))))
      array))


(define-global-constant +mwrite-box-funcs+
     (let* ((syms +mem-box-symbols+)
            (array (make-array (length syms))))

       (loop for i from 0 below (length syms) do
            (setf (svref array i) (fdefinition (symbol-concat 'mwrite-box/ (svref syms i)))))
       array))


(define-global-constant +mread-box-funcs+
    (let* ((syms +mem-box-symbols+)
           (array (make-array (length syms))))

      (loop for i from 0 below (length syms) do
           (setf (svref array i) (fdefinition (symbol-concat 'mread-box/ (svref syms i)))))
      array))


(declaim (notinline detect-box-type))

(defun detect-box-type (value)
  "Detect the boxed-type of VALUE. Returns one of the constants +mem-box/...+"

  (the mem-box-type
    (etypecase value
      (integer      +mem-box/bignum+)
      (ratio        +mem-box/ratio+)

      (single-float +mem-box/sfloat+)
      (double-float +mem-box/dfloat+)

      ((complex single-float) +mem-box/complex-sfloat+)
      ((complex double-float) +mem-box/complex-dfloat+)
      (complex      (etypecase (realpart value)
                      (rational     +mem-box/complex-rational+)))

      (list         +mem-box/list+)

      (array        (if (= 1 (array-rank value))
                        (case (array-element-type value)
                          (character +mem-box/string+)
                          (base-char +mem-box/base-string+)
                          (bit       +mem-box/bit-vector+)
                          (otherwise +mem-box/vector+))
                        +mem-box/array+))
      
      (hash-table   +mem-box/hash-table+)
      (pathname     +mem-box/pathname+))))

(declaim (inline detect-box-n-words))

(defun detect-box-n-words (value boxed-type)
  "Return the number of words needed to store boxed VALUE in memory,
not including BOX header."

  (declare (type mem-box-type boxed-type))

  (let ((funcs +box-words-funcs+))
    #-(and)
    (check-vector-index  funcs boxed-type
                         "internal error! invalid boxed-type detected for value ~S:
    found boxed-type ~S, expecting a number between 0 and ~S"
                         value boxed-type (length funcs))

    (funcall (the function (svref funcs boxed-type)) value)))


(defun detect-box-type-n-words (value)
  "Return boxed-type and the number of words needed to store boxed VALUE in memory as multiple values,
not including BOX header."

  (let ((boxed-type (detect-box-type value)))
    (values
     boxed-type
     (detect-box-n-words value boxed-type))))


(declaim (inline %mwrite-box))

(defun %mwrite-box (ptr index value n-words boxed-type)
  "Write a boxed value into the mmap memory starting at (PTR+INDEX).
Also writes BOX header."
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type mem-box-type boxed-type))

  ;; write BOX header
  (let ((index (mwrite-box/header ptr index n-words boxed-type)))
      
    (funcall (the function (svref +mwrite-box-funcs+ boxed-type))
             ptr index value)))



(defun mwrite-box (ptr value &optional box)
  "Write a boxed value into the object store, (re)allocating space if needed.
Return the written box."
  (declare (type maddress ptr)
           (type (or null box) box))

  (let* ((boxed-type (detect-box-type    value))
         (n-words    (mem-size+ +mem-box/header-words+ (detect-box-n-words value boxed-type)))
         (allocated-n-words (if box (box-n-words box) 0)))
    
    (if (and (<= n-words allocated-n-words)
             (>= n-words (ash allocated-n-words -1)))
        ;; reuse the existing memory
        (setf n-words allocated-n-words)
        ;; we must (re)allocate memory
        (setf box (box-realloc ptr box n-words)
              ;; read back rounded-up n-words - absolutely necessary!
              n-words (box-n-words box)))

    (setf (box-value box) value)
    (%mwrite-box ptr (box-index box) value n-words boxed-type)
    box))



(declaim (inline %%mread-box %mread-box))

(defun %%mread-box (ptr index boxed-type)
  "Read a boxed value from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-fulltag boxed-type))

  (let ((funcs +mread-box-funcs+))
    
    (check-vector-index funcs boxed-type
                        "invalid BOX-TYPE at mmap word (+ ~S ~S):
found ~S, expecting a number between 0 and ~S"
                        ptr index boxed-type (1- (length funcs)))

    (funcall (the function (svref funcs boxed-type))
             ptr index)))


(defun %mread-box (ptr index)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the value and the number of read words as multiple values."
  (declare (type maddress ptr)
           (type mem-size index))

  ;; read BOX header
  (multiple-value-bind (n-words boxed-type) (mread-box/header ptr index)
    (values
     (%%mread-box ptr (mem-size+ +mem-box/header-words+ index) boxed-type)
     n-words)))



(defun mread-box (ptr index &optional box)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the boxed value."
  (declare (type maddress ptr)
           (type mem-size index)
           (type (or null box) box))

  (multiple-value-bind (value n-words) (mread-box ptr index)
    (if box
        (reuse-box box index n-words value)
        (make-box index n-words value))))

