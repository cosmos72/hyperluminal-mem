;; -*- lisp -*-

;; This file is part of Hyperluminal-mem.
;; Copyright (c) 2013-2015 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


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


(declaim (inline sap=>fast-sap))
(defun sap=>fast-sap (x)
  x)

(declaim (inline fast-sap=>sap))
(defun fast-sap=>sap (x)
  x)

