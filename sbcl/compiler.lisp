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


(in-package :hyperluminal-mem-sbcl)


(deftype word () 'sb-ext:word)

(defconstant +n-fixnum-tag-bits+ sb-vm:n-fixnum-tag-bits
  "Number of low bits that are always ZERO
in the representation of a FIXNUM")

(defconstant +fixnum-zero-mask+1+ (ash 1 sb-vm:n-fixnum-tag-bits)
  "1+ mask of the low bits that are always ZERO
in the representation of a FIXNUM")

;;;; compiler intrinsic functions

(defconstant +defknown-has-overwrite-fndb-silently+
  (dolist (arg (second (sb-kernel:type-specifier (sb-int:info :function :type 'sb-c::%defknown))))
    (when (and (consp arg)
               (eq (first arg) :overwrite-fndb-silently))
      (return t))))

(defmacro defknown (&rest args)
  `(sb-c:defknown ,@args
       ,@(if +defknown-has-overwrite-fndb-silently+ '(:overwrite-fndb-silently t) ())))


;;;;; check sbcl features

;; return '(:and) if sbcl version is >= version-int-list, otherwise '(:or)
(defun lisp-version>= (version-int-list)
  (declare (type (or string list) version-int-list))
  (stmx.asm::compile-if
   (stmx.asm::lisp-version>= version-int-list)))

;; return '(:and) if symbol exists in given package and is (fboundp), otherwise '(:or)
(defun compile-if-func (pkg-name sym-name)
  (declare (type symbol pkg-name sym-name))
  (stmx.asm::compile-if
   (let ((pkg (find-package pkg-name)))
     (when pkg
       (let ((sym (find-symbol (symbol-name sym-name) pkg)))
         (fboundp sym))))))
