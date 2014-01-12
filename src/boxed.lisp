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

(enable-#?-syntax)


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
    (let* ((syms +mem-boxed-type-syms+)
           (array (make-array (length syms))))

      (loop for i from 0 below (length syms) do
           (setf (svref array i) (fdefinition (concat-symbols 'box-words/ (svref syms i)))))
      array))


(define-global-constant +mwrite-box-funcs+
    (let* ((syms +mem-boxed-type-syms+)
            (array (make-array (length syms))))

       (loop for i from 0 below (length syms) do
            (setf (svref array i) (fdefinition (concat-symbols 'mwrite-box/ (svref syms i)))))
       array))


(define-global-constant +mread-box-funcs+
    (let* ((syms +mem-boxed-type-syms+)
           (array (make-array (length syms))))

      (loop for i from 0 below (length syms) do
           (setf (svref array i) (fdefinition (concat-symbols 'mread-box/ (svref syms i)))))
      array))



(declaim (inline get-box-func))

(defun get-box-func (funcs boxed-type)
  (declare (type vector funcs)
           (type mem-box-type boxed-type))

  (let ((i (- boxed-type +mem-box/first+)))

    #-(and)
    (check-vector-index funcs i
                        "out-of-range boxed-type! found ~S, expecting a number between ~S and ~S"
                        boxed-type +mem-box/first+ (+ +mem-box/first+ -1 (length funcs)))

    (the function (svref funcs i))))



(defmacro call-box-func (funcs boxed-type &rest args)
  `(funcall (get-box-func ,funcs ,boxed-type) ,@args))


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


(declaim (inline round-up-n-words %mwrite-box))

(defun round-up-n-words (n-words)
  "Round up N-WORDS to a multiple of +MEM-BOX/MIN-WORDS+"
  (declare (type mem-size n-words))

  (logand (mem-size+ n-words #.(1- +mem-box/min-words+))
	  #.(lognot (1- +mem-box/min-words+))))


(declaim (inline detect-box-n-words detect-box-n-words-rounded-up))

(defun detect-box-n-words (value &optional (boxed-type (detect-box-type value)))
  "Return the number of words needed to store boxed VALUE in memory,
also including BOX header.
Does NOT round up the returned value to a multiple of +MEM-BOX/MIN-WORDS+"

  (declare (type mem-box-type boxed-type))

  (log.trace value boxed-type)

  (mem-size+ +mem-box/header-words+
	     (call-box-func +box-words-funcs+ boxed-type value)))


(defun detect-box-n-words-rounded-up (value &optional
				      (boxed-type (detect-box-type value)))
  "Return the number of words needed to store boxed VALUE in memory,
also including BOX header.
Rounds up the returned value to a multiple of +MEM-BOX/MIN-WORDS+"

  (declare (type mem-box-type boxed-type))

  (round-up-n-words (detect-box-n-words value boxed-type)))






(defun %mwrite-box (ptr index value n-words boxed-type)
  "Write a boxed value into the mmap memory starting at (PTR+INDEX).
Also writes BOX header. Returns T."
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type mem-box-type boxed-type))

  ;; write BOX header.
  ;; assumes n-words is already rounded up to a multiple of +mem-box/min-words+
  ;; because only such multiples can be written accurately into the store.
  (let ((index (mwrite-box/header ptr index n-words boxed-type)))
      
    (call-box-func +mwrite-box-funcs+ boxed-type ptr index value)))



(defun mwrite-box (ptr value &optional box)
  "Write a boxed value into the object store, (re)allocating space if needed.
Return the written box."
  (declare (type maddress ptr)
           (type (or null box) box))

  (let* ((boxed-type (detect-box-type    value))
         (n-words    (detect-box-n-words-rounded-up value boxed-type))
         (allocated-n-words (if box (box-n-words box) 0)))
    
    (if (and (<= n-words allocated-n-words)
             (>= n-words (ash allocated-n-words -1)))
        ;; reuse the existing memory
        (setf n-words allocated-n-words)
        ;; we must (re)allocate memory
        (setf box (box-realloc ptr box n-words)
              ;; ABSOLUTELY NECESSARY! read back actually allocated
	      ;; n-words (usually rounded up somewhat)
              n-words (box-n-words box)))

    (setf (box-value box) value)
    (%mwrite-box ptr (box-index box) value n-words boxed-type)
    box))



(declaim (inline %%mread-box %mread-box))

(defun %%mread-box (ptr index boxed-type)
  "Read a boxed value from the memory starting at (PTR+INDEX) and return it.
Return the number of words actually read as additional value.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-fulltag boxed-type))

  (call-box-func +mread-box-funcs+ boxed-type ptr index))


(defun %mread-box (ptr index)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the value and the number of words actually read as multiple values."
  (declare (type maddress ptr)
           (type mem-size index))

  ;; read BOX header
  (let ((boxed-type (mget-fulltag ptr index)))
    (%%mread-box ptr (mem-size+ +mem-box/header-words+ index) boxed-type)))



(defun mread-box (ptr index &optional box)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the boxed value."
  (declare (type maddress ptr)
           (type mem-size index)
           (type (or null box) box))

  (multiple-value-bind (value n-words) (%mread-box ptr index)
    (if box
        (reuse-box box index n-words value)
        (make-box index n-words value))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mid-level functions accepting both boxed and unboxed values             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun detect-n-words (value)
  "Compute and return the number of CPU words needed to store VALUE.
If VALUE can be stored unboxed, returns 1. Otherwise forwards the call
to DETECT-BOX-N-WORDS.
Does NOT round up the returned value to a multiple of +MEM-BOX/MIN-WORDS+"
  (if (is-unboxed? value)
      1
      (detect-box-n-words value)))


(defun detect-n-words-rounded-up (value)
  "Compute and return the number of CPU words needed to store VALUE.
If VALUE can be stored unboxed, returns 1. Otherwise forwards the call
to DETECT-BOX-N-WORDS.
Also rounds up the returned value to a multiple of +MEM-BOX/MIN-WORDS+"

  (if (is-unboxed? value)
      1
      (detect-box-n-words-rounded-up value)))


(defun mwrite (ptr index value)
  "Write a value (either boxed or unboxed) into the memory starting at (PTR+INDEX).
Return the number of words actually written (not rounded up).

WARNING: enough memory must be already allocated at (PTR+INDEX) !!!"
  (declare (type maddress ptr)
           (type mem-size index))

  (when (mset-unboxed ptr index value)
    (return-from mwrite 1))

  ;; TODO: handle symbols and pointers!
  (the mem-size
    (let* ((boxed-type (detect-box-type value))
	   (n-words    (detect-box-n-words-rounded-up value boxed-type)))
      (%mwrite-box ptr index value n-words boxed-type))))


(defun mread (ptr index)
  "Read a value (either boxed or unboxed) from the memory starting at (PTR+INDEX).
Return the value and the number of words actually read as multiple values."
  (declare (type maddress ptr)
           (type mem-size index))

  (multiple-value-bind (value boxed-type) (mget-unboxed ptr index)
    (if boxed-type
        ;; TODO: handle symbols and pointers!
        (%mread-box ptr index)
        (values value 1))))
