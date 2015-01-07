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
Implementation note: for SBCL, FAST-SAP is identical to normal CFFI-SYS:FOREIGN-POINTER
i.e. they are SB-SYS:SYSTEM-AREA-POINTER"
  'cffi-sys:foreign-pointer)


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


(declaim (inline sap=>fast-sap fast-sap=>sap))

(defun sap=>fast-sap (x)
  x)

(defun fast-sap=>sap (x)
  x)

