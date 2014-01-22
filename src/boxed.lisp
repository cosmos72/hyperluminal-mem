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
    (the function (svref funcs i))))



(defmacro call-box-func (funcs boxed-type &rest args)
  `(progn
     (log:debug 'funcall ,boxed-type ,@args)
     (funcall (get-box-func ,funcs ,boxed-type) ,@args)))


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

      (array        (if (/= 1 (array-rank value))
                        +mem-box/array+
                        (case (array-element-type value)
                          (character +mem-box/string+)
                          (base-char +mem-box/base-string+)
                          (bit       +mem-box/bit-vector+)
                          (otherwise +mem-box/vector+))))
      
      (symbol       +mem-box/symbol+) ;; it would be the last one... out of order for speed.

      (hash-table   (ecase (hash-table-test value)
                      (eq +mem-box/hash-table-eq+)
                      (eql +mem-box/hash-table-eq+)
                      (equal +mem-box/hash-table-equal+)
                      (equalp +mem-box/hash-table-equal+)))
                              
      (pathname     +mem-box/pathname+))))


(declaim (inline round-up-n-words))

(defun round-up-n-words (n-words)
  "Round up N-WORDS to a multiple of +MEM-BOX/MIN-WORDS+"
  (declare (type mem-size n-words))

  (logand (mem-size+ n-words #.(1- +mem-box/min-words+))
	  #.(lognot (1- +mem-box/min-words+))))


(declaim (inline detect-box-n-words))

(defun detect-box-n-words (value &optional (boxed-type (detect-box-type value)))
  "Return the number of words needed to store boxed VALUE in memory,
also including BOX header.
Does NOT round up the returned value to a multiple of +MEM-BOX/MIN-WORDS+"

  (declare (type mem-box-type boxed-type))

  (log.trace value boxed-type)

  (mem-size+ +mem-box/header-words+
	     (call-box-func +box-words-funcs+ boxed-type value)))


(declaim (inline detect-box-n-words-rounded-up))

(defun detect-box-n-words-rounded-up (value &optional
				      (boxed-type (detect-box-type value)))
  "Return the number of words needed to store boxed VALUE in memory,
also including BOX header.
Rounds up the returned value to a multiple of +MEM-BOX/MIN-WORDS+"

  (declare (type mem-box-type boxed-type))

  (round-up-n-words (detect-box-n-words value boxed-type)))




(declaim (ftype (function (maddress mem-size mem-size t mem-box-type)
			  (values mem-size &optional))
		 %mwrite-box)
	 (inline %mwrite-box))

(defun %mwrite-box (ptr index end-index value boxed-type)
  "Write a boxed value into the mmap memory starting at (PTR+INDEX).
Also writes BOX header. Returns INDEX pointing to immediately after written value."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type mem-box-type boxed-type))

  ;; write BOX payload.
  (let* ((new-index
          (call-box-func +mwrite-box-funcs+ boxed-type ptr
                         (mem-size+ index +mem-box/header-words+)
                         end-index value))
         (actual-words (mem-size- new-index index)))
         
    (when (> new-index end-index)
      (error "HYPERLUMINAL-DB internal error!
wrote ~S word~P at address (+ ~S ~S),
but only ~S words were available at that location.
Either this is a bug in hyperluminal-db, or some object
was concurrently modified while being written"
             actual-words actual-words ptr index (mem-size- end-index index)))

    (mwrite-box/header ptr index boxed-type (round-up-n-words actual-words))
    new-index))



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
    (let ((index (box-index box)))
      (%mwrite-box ptr index (mem-size+ index n-words) value boxed-type))
    box))



(declaim (inline %%mread-box %mread-box))

(defun %%mread-box (ptr index end-index boxed-type)
  "Read a boxed value from the memory starting at (PTR+INDEX) and return it.
Return the number of words actually read as additional value.
Skips over BOX header."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type mem-fulltag boxed-type))

  (call-box-func +mread-box-funcs+ boxed-type ptr
                 (mem-size+ index +mem-box/header-words+) end-index))


         


(defun %mread-box2 (ptr index end-index boxed-type)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the value and the number of words actually read as multiple values."
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-fulltag boxed-type))

  ;; validate BOXED-TYPE
  (unless (typep boxed-type 'mem-box-type)
    (box-type-error ptr index boxed-type))

  (%%mread-box ptr index end-index boxed-type))


(defun %mread-box (ptr index end-index)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the value and the number of words actually read as multiple values."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (multiple-value-bind (boxed-type n-words) (mread-box/header ptr index)
    (check-box-type ptr index boxed-type)
    (check-mem-length ptr index end-index n-words)

    (let ((end-index (mem-size+ index n-words)))
      
      (%%mread-box ptr index end-index boxed-type))))





(defun mread-box (ptr index box)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Return the boxed value."
  (declare (type maddress ptr)
           (type mem-size index)
           (type box box))

  (multiple-value-bind (value n-words) (%mread-box ptr index (mem-size+ index (box-n-words box)))
    (reuse-box box index n-words value)))



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
      (the mem-size (detect-box-n-words value))))


(defun detect-n-words-rounded-up (value)
  "Compute and return the number of CPU words needed to store VALUE.
If VALUE can be stored unboxed, returns 1. Otherwise forwards the call
to DETECT-BOX-N-WORDS.
Also rounds up the returned value to a multiple of +MEM-BOX/MIN-WORDS+"

  (if (is-unboxed? value)
      1
      (the mem-size (detect-box-n-words-rounded-up value))))


;; (declaim (ftype (...) mwrite)) is in box.lisp

(defun mwrite (ptr index end-index value)
  "Write a value (either boxed or unboxed) into the memory starting at (PTR+INDEX).
Return the INDEX pointing to immediately after the value just written.

WARNING: enough memory must be already allocated at (PTR+INDEX) !!!"
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type t value))

  (check-mem-overrun ptr index end-index 1)

  (if (mset-unboxed ptr index value)
      (mem-size+1 index)
      ;; TODO: handle symbols and pointers!
      (%mwrite-box ptr index end-index value (detect-box-type value))))


;; (declaim (ftype (...) mread)) is in box.lisp

(defun mread (ptr index end-index)
  "Read a value (either boxed or unboxed) from the memory starting at (PTR+INDEX).
Return multiple values:
1) the value 
2) the INDEX pointing to immediately after the value just read"
  (declare (type maddress ptr)
           (type mem-size index end-index))

  (check-mem-length ptr index end-index 1)

  (multiple-value-bind (value boxed-type) (mget-unboxed ptr index)
    (if boxed-type
        ;; TODO: handle symbols and pointers!
        (%mread-box2 ptr index end-index boxed-type)
        (values value (mem-size+1 index)))))


(defun !mwrite (ptr index value)
  "Used only for debugging."
  (declare (type maddress ptr)
           (type mem-size index))

  (mwrite ptr index +mem-box/max-words+ value))

(defun !mread (ptr index)
  "Used only for debugging."
  (declare (type maddress ptr)
           (type mem-size index))
  (mread ptr index +mem-box/max-words+))
