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


(defun fast-memcpy/4 (dst dst-index src src-index n-words &key
			(dst-scale 4) (dst-offset 0)
			(src-scale 4) (src-offset 0))
  (declare (type ffi-address dst src)
	   (type fixnum dst-index dst-scale dst-offset)
	   (type fixnum src-index src-scale src-offset)
	   (type fixnum n-words))

  (when (<= n-words 8)
    (let ((dst-index (fixnum+ (fixnum* dst-index dst-scale) dst-offset))
	  (src-index (fixnum+ (fixnum* src-index src-scale) src-offset)))
      (loop for i from 0 below (fixnum* n-words 4) by 4
	 do
	   (let ((value (ffi-mem-get src :int (fixnum+ i src-index))))
	     (ffi-mem-set value dst :int (fixnum+ i dst-index))))
      (return-from fast-memcpy/4 nil)))

  (let* ((dst   (sap=>buf4 dst))
	 (src   (sap=>buf4 src))
	 (chunk 16384)
	 (n     (min chunk n-words))
	 (buf   (java:jnew-array "int" n)))

    ;; buf4 i.e. java.nio.IntBuffer offsets are counted in ints, not in bytes
    (buf4-inc-position dst dst-index
		       :scale (ash dst-scale -2) :offset (ash dst-offset -2))
    (buf4-inc-position src src-index
		       :scale (ash src-scale -2) :offset (ash src-offset -2))

    (loop while (plusp n-words)
       do
	 (setf n (min chunk n-words))
	 ;; bulkget and bulkput offset refer to the int[], not to the buffer!
	 (java:jcall +buf4-bulkget+ src buf 0 n)
	 (java:jcall +buf4-bulkput+ dst buf 0 n)
	 (decf n-words n))))


(defun fast-memcpy/8 (dst dst-index src src-index n-words &key
			(dst-scale 8) (dst-offset 0)
			(src-scale 8) (src-offset 0))
  (declare (type ffi-address dst src)
	   (type fixnum dst-index dst-scale dst-offset)
	   (type fixnum src-index src-scale src-offset)
	   (type fixnum n-words))

  (when (<= n-words 8)
    (let ((dst-index (fixnum+ (fixnum* dst-index dst-scale) dst-offset))
	  (src-index (fixnum+ (fixnum* src-index src-scale) src-offset)))
      (loop for i from 0 below (fixnum* n-words 8) by 8
	 do
	   (let ((value (ffi-mem-get src :long (fixnum+ i src-index))))
	     (ffi-mem-set value dst :long (fixnum+ i dst-index))))

      (return-from fast-memcpy/8 nil)))

  (let* ((dst   (sap=>buf8 dst))
	 (src   (sap=>buf8 src))
	 (chunk 8192)
	 (n     (min chunk n-words))
	 (buf   (java:jnew-array "long" n)))

    ;; buf8 i.e. java.nio.LongBuffer offsets are counted in longs, not in bytes
    (buf8-inc-position dst dst-index
		       :scale (ash dst-scale -3) :offset (ash dst-offset -3))
    (buf8-inc-position src src-index
		       :scale (ash src-scale -3) :offset (ash src-offset -3))

    (loop while (plusp n-words)
       do
	 (setf n (min chunk n-words))
	 ;; bulkget and bulkput offset refer to the long[], not to the buffer!
	 (java:jcall +buf8-bulkget+ src buf 0 n)
	 (java:jcall +buf8-bulkput+ dst buf 0 n)
	 (decf n-words n))))


(defun fast-memset/4 (ptr index n-words fill-word &key (scale 4) (offset 0))
  (declare (type ffi-address dst src)
	   (type fixnum index scale offset n-words)
	   (type (unsigned-byte 32) fill-word))

  (when (<= n-words 8)
    (let* ((index (fixnum+ (fixnum* index scale) offset))
	   (end   (fixnum+ index n-words)))
      (loop while (< index end) do
	   (ffi-mem-set fill-word ptr :unsigned-int index)
           (incf index))

      (return-from fast-memset/4 nil)))

  (let* ((ptr   (sap=>buf4 ptr))
	 (chunk 16384)
	 (n     (min chunk n-words))
	 (buf   (java:jnew-array "int" n)))

    (unless (zerop fill-word)
      (dotimes (i n)
	(java:jarray-set buf fill-word i)))

    ;; buf4 i.e. java.nio.IntBuffer offsets are counted in ints, not in bytes
    (buf4-inc-position ptr index :scale (ash scale -2) :offset (ash offset -2))

    (loop while (plusp n-words)
       do
	 (setf n (min chunk n-words))
	 ;; bulkput offset refer to the int[], not to the buffer!
	 (java:jcall +buf4-bulkput+ ptr buf 0 n)
	 (decf n-words n))))


(defun fast-memset/8 (ptr index n-words fill-word &key (scale 8) (offset 0))
  (declare (type ffi-address dst src)
	   (type fixnum index scale offset n-words)
	   (type (unsigned-byte 32) fill-word))

  (when (<= n-words 8)
    (let* ((index (fixnum+ (fixnum* index scale) offset))
	   (end   (fixnum+ index n-words)))
      (loop while (< index end) do
	   (ffi-mem-set fill-word ptr :unsigned-long index)
           (incf index))

      (return-from fast-memset/8 nil)))

  (let* ((ptr   (sap=>buf8 ptr))
	 (chunk 8192)
	 (n     (min chunk n-words))
	 (buf   (java:jnew-array "long" n)))

    (unless (zerop fill-word)
      (dotimes (i n)
	(java:jarray-set buf fill-word i)))

    ;; buf8 i.e. java.nio.LongBuffer offsets are counted in longs, not in bytes
    (buf8-inc-position ptr index :scale (ash scale -3) :offset (ash offset -3))

    (loop while (plusp n-words)
       do
	 (setf n (min chunk n-words))
	 ;; bulkput offset refer to the int[], not to the buffer!
	 (java:jcall +buf8-bulkput+ ptr buf 0 n)
	 (decf n-words n))))
