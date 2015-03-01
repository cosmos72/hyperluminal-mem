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


(in-package :hyperluminal-abcl)



(deftype fast-sap/4 ()
  "A faster implementation of foreign pointers (sap).
Implementation note: for ABCL, FAST-SAP/4 is java.nio.IntBuffer
while normal SAP are java.nio.ByteBuffer"
  'java:java-object)

(deftype fast-sap/8 ()
  "A faster implementation of foreign pointers (sap).
Implementation note: for ABCL, FAST-SAP/8 is java.nio.LongBuffer
while normal SAP are java.nio.ByteBuffer"
  'java:java-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +jclass-int-array+ (java:jclass-of (java:jnew-array "int" 0)))
(defconstant +jclass-long-array+ (java:jclass-of (java:jnew-array "long" 0)))

(defconstant +sap=>fast-sap/4+
  (java:jmethod "java.nio.ByteBuffer" "asIntBuffer"))
(defconstant +sap=>fast-sap/8+
  (java:jmethod "java.nio.ByteBuffer" "asLongBuffer"))
(defconstant +copy-fast-sap/4+
  (java:jmethod "java.nio.IntBuffer" "duplicate"))
(defconstant +copy-fast-sap/8+
  (java:jmethod "java.nio.LongBuffer" "duplicate"))
(defconstant +fast-sap/4-position+
  (java:jmethod "java.nio.IntBuffer" "position"))
(defconstant +fast-sap/8-position+
  (java:jmethod "java.nio.LongBuffer" "position"))
(defconstant +fast-sap/4-setposition+
  (java:jmethod "java.nio.IntBuffer" "position" "int"))
(defconstant +fast-sap/8-setposition+
  (java:jmethod "java.nio.LongBuffer" "position" "int"))
(defconstant +fast-sap/4-get+
  (java:jmethod "java.nio.IntBuffer" "get" "int"))
(defconstant +fast-sap/4-put+
  (java:jmethod "java.nio.IntBuffer" "put" "int" "int"))
(defconstant +fast-sap/8-get+
  (java:jmethod "java.nio.LongBuffer" "get" "int"))
(defconstant +fast-sap/8-put+
  (java:jmethod "java.nio.LongBuffer" "put" "int" "long"))
(defconstant +fast-sap/4-bulkget+
  (java:jmethod "java.nio.IntBuffer" "get" +jclass-int-array+ "int" "int"))
(defconstant +fast-sap/4-bulkput+
  (java:jmethod "java.nio.IntBuffer" "put" +jclass-int-array+ "int" "int"))
(defconstant +fast-sap/8-bulkget+
  (java:jmethod "java.nio.LongBuffer" "get" +jclass-long-array+ "int" "int"))
(defconstant +fast-sap/8-bulkput+
  (java:jmethod "java.nio.LongBuffer" "put" +jclass-long-array+ "int" "int"))


         
(declaim (inline sap=>fast-sap/4))
(defun sap=>fast-sap/4 (x)
  (java:jcall +sap=>fast-sap/4+ x))

(declaim (inline sap=>fast-sap/8))
(defun sap=>fast-sap/8 (x)
  (java:jcall +sap=>fast-sap/8+ x))

(declaim (inline copy-fast-sap/4))
(defun copy-fast-sap/4 (x)
  (java:jcall +copy-fast-sap/4+ x))

(declaim (inline copy-fast-sap/8))
(defun copy-fast-sap/8 (x)
  (java:jcall +copy-fast-sap/8+ x))

(declaim (inline fast-sap/4-position))
(defun fast-sap/4-position (sap)
  (java:jcall +fast-sap/4-position+ sap))

(declaim (inline fast-sap/8-position))
(defun fast-sap/8-position (sap)
  (java:jcall +fast-sap/8-position+ sap))

(declaim (inline fast-sap/4-setposition))
(defun fast-sap/4-setposition (sap index)
  (java:jcall +fast-sap/4-setposition+ sap index)
  index)

(declaim (inline fast-sap/8-setposition))
(defun fast-sap/8-setposition (sap index)
  (java:jcall +fast-sap/8-setposition+ sap index)
  index)

(defsetf fast-sap/4-position fast-sap/4-setposition)
(defsetf fast-sap/8-position fast-sap/8-setposition)

(defmacro fast-sap/4-incposition (sap index &key (scale 1) (offset 0))
  `(incf (the fixnum (fast-sap/4-position ,sap))
	 (fixnum+ (fixnum* ,index ,scale) ,offset)))

(defmacro fast-sap/8-incposition (sap index &key (scale 1) (offset 0))
  `(incf (the fixnum (fast-sap/8-position ,sap))
	 (fixnum+ (fixnum* ,index ,scale) ,offset)))

