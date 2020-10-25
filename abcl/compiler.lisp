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


(in-package :hyperluminal-mem-abcl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +jclass-int-array+  (java:jclass-of (java:jnew-array "int" 0)))
(defconstant +jclass-long-array+ (java:jclass-of (java:jnew-array "long" 0)))

(defconstant +sap=>buf4+
  (java:jmethod "java.nio.ByteBuffer" "asIntBuffer"))
(defconstant +sap=>buf8+
  (java:jmethod "java.nio.ByteBuffer" "asLongBuffer"))
(defconstant +copy-buf4+
  (java:jmethod "java.nio.IntBuffer"  "duplicate"))
(defconstant +copy-buf8+
  (java:jmethod "java.nio.LongBuffer" "duplicate"))
(defconstant +buf4-position+
  (java:jmethod "java.nio.IntBuffer"  "position"))
(defconstant +buf8-position+
  (java:jmethod "java.nio.LongBuffer" "position"))
(defconstant +buf4-set-position+
  (java:jmethod "java.nio.IntBuffer"  "position" "int"))
(defconstant +buf8-set-position+
  (java:jmethod "java.nio.LongBuffer" "position" "int"))
(defconstant +buf4-get+
  (java:jmethod "java.nio.IntBuffer"  "get" "int"))
(defconstant +buf4-put+
  (java:jmethod "java.nio.IntBuffer"  "put" "int" "int"))
(defconstant +buf8-get+
  (java:jmethod "java.nio.LongBuffer" "get" "int"))
(defconstant +buf8-put+
  (java:jmethod "java.nio.LongBuffer" "put" "int" "long"))
(defconstant +buf4-bulkget+
  (java:jmethod "java.nio.IntBuffer"  "get" +jclass-int-array+ "int" "int"))
(defconstant +buf4-bulkput+
  (java:jmethod "java.nio.IntBuffer"  "put" +jclass-int-array+ "int" "int"))
(defconstant +buf8-bulkget+
  (java:jmethod "java.nio.LongBuffer" "get" +jclass-long-array+ "int" "int"))
(defconstant +buf8-bulkput+
  (java:jmethod "java.nio.LongBuffer" "put" +jclass-long-array+ "int" "int"))



(declaim (inline sap=>buf4 (x)))
(defun sap=>buf4 (x)
  (java:jcall +sap=>buf4+ x))

(declaim (inline sap=>buf8 (x)))
(defun sap=>buf8 (x)
  (java:jcall +sap=>buf8+ x))

(declaim (inline sap=>buf8))
(declaim (inline copy-buf4))
(defun copy-buf4 (x)
  (java:jcall +copy-buf4+ x))

(declaim (inline copy-buf8))
(defun copy-buf8 (x)
  (java:jcall +copy-buf8+ x))

(declaim (inline buf4-position))
(defun buf4-position (sap)
  (java:jcall +buf4-position+ sap))

(declaim (inline buf8-position))
(defun buf8-position (sap)
  (java:jcall +buf8-position+ sap))

(declaim (inline buf4-set-position))
(defun buf4-set-position (sap index)
  (java:jcall +buf4-set-position+ sap index)
  index)

(declaim (inline buf8-set-position))
(defun buf8-set-position (sap index)
  (java:jcall +buf8-set-position+ sap index)
  index)

(defsetf buf4-position buf4-set-position)
(defsetf buf8-position buf8-set-position)

(defmacro buf4-inc-position (sap index &key (scale 1) (offset 0))
  `(incf (the fixnum (buf4-position ,sap))
	 (fixnum+ (fixnum* ,index ,scale) ,offset)))

(defmacro buf8-inc-position (sap index &key (scale 1) (offset 0))
  `(incf (the fixnum (buf8-position ,sap))
	 (fixnum+ (fixnum* ,index ,scale) ,offset)))
