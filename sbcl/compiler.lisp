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


(in-package :hyperluminal-sbcl)



(deftype fast-sap ()
  "A faster implementation of foreign pointers (sap).
ONLY supports aligned pointers!
On x86, FAST-SAP must be aligned at 4-byte boundary.
On x86-64, FAST-SAP must be aligned at 2-byte boundary.

Implementation note: FAST-SAP instances are internally represented
as FIXNUM, so they do NOT support TYPEP and TYPECASE"
  'fixnum)


(defconstant +n-fixnum-tag-bits+ sb-vm:n-fixnum-tag-bits
  "Number of low bits that are always ZERO
in the representation of a FIXNUM")

(defconstant +fixnum-zero-mask+1+ (ash 1 sb-vm:n-fixnum-tag-bits)
  "1+ mask of the low bits that are always ZERO
in the representation of a FIXNUM")

;;;; new compiler intrinsic functions

(defconstant +defknown-has-overwrite-fndb-silently+
  (dolist (arg (second (sb-kernel:type-specifier (sb-int:info :function :type 'sb-c::%defknown))))
    (when (and (consp arg)
               (eq (first arg) :overwrite-fndb-silently))
      (return t))))

(defmacro defknown (&rest args)
  `(sb-c:defknown ,@args
       ,@(if +defknown-has-overwrite-fndb-silently+ '(:overwrite-fndb-silently t) ())))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defknown %word=>fast-sap ((unsigned-byte #.sb-vm::n-word-bits)) fixnum
    (sb-c::movable sb-c::flushable sb-c::foldable
		     sb-c::always-translatable))

(defknown %fast-sap=>word (fixnum) (unsigned-byte #.sb-vm::n-word-bits)
    (sb-c::movable sb-c::flushable sb-c::foldable
		     sb-c::always-translatable))


(sb-c:define-vop (%word=>fast-sap/safe)
  (:policy :fast-safe)
  (:translate %word=>fast-sap)
  (:args      (address :scs (sb-vm::unsigned-reg) :target r))
  (:arg-types sb-vm::unsigned-num)
  (:results   (r :scs (sb-vm::any-reg)))
  (:result-types sb-vm::tagged-num)
  (:generator 2
   (sb-c:move r address)
   (sb-assem:inst and r (- +fixnum-zero-mask+1+))))


(sb-c:define-vop (%word=>fast-sap)
  (:policy :fast)
  (:translate %word=>fast-sap)
  (:args      (address :scs (sb-vm::unsigned-reg) :target r))
  (:arg-types sb-vm::unsigned-num)
  (:results   (r :scs (sb-vm::any-reg)))
  (:result-types sb-vm::tagged-num)
  (:generator 1
   (sb-c:move r address)))

(sb-c:define-vop (%fast-sap=>word)
  (:policy :fast-safe)
  (:translate %fast-sap=>word)
  (:args      (num :scs (sb-vm::any-reg) :target r))
  (:arg-types sb-vm::tagged-num)
  (:results   (r :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 1
   (sb-c:move r num)))



(declaim (inline sap=>fast-sap fast-sap=>sap))

(defun sap=>fast-sap (ptr)
  (declare (type sb-vm::system-area-pointer ptr))
  (the fast-sap
    (%word=>fast-sap (sb-vm::sap-int ptr))))

(defun fast-sap=>sap (num)
  (declare (type fast-sap num))
  (the sb-vm::system-area-pointer
    (sb-vm::int-sap (%fast-sap=>word num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-ffi-fast-mem ((var-name n-bytes) &body body)
  (with-gensym temp-name
    `(with-ffi-mem (,temp-name ,n-bytes)
       (let ((,var-name (sap=>fast-sap ,temp-name)))
	 ,@body))))


(defmacro with-vector-fast-mem ((var-name vector) &body body)
  (with-gensym temp-name
    `(with-vector-mem (,temp-name ,vector)
       (let ((,var-name (sap=>fast-sap ,temp-name)))
	 ,@body))))



