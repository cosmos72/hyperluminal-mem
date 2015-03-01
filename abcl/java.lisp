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

(defun fast-memcpy/4 (dst dst-index src src-index n-words &key
			(dst-scale 4) (dst-offset 0)
			(src-scale 4) (src-offset 0))
  (declare (type fast-sap/4 dst src)
	   (type fixnum dst-index dst-scale dst-offset)
	   (type fixnum src-index src-scale src-offset)
	   (type fixnum n-words))

  (when (<= n-words 8)
    ;; fast-sap/4 i.e. java.nio.IntBuffer offsets are counted in ints, not in bytes
    (let ((dst-index (fixnum+ (fixnum* dst-index (ash dst-scale -2)) dst-offset))
	  (src-index (fixnum+ (fixnum* src-index (ash src-scale -2)) src-offset)))
      (dotimes (i n-words)
	(java:jcall +fast-sap/4-put+ dst (fixnum+ i dst-index)
		    (java:jcall +fast-sap/4-get+ src (fixnum+ i src-index))))
      (return-from fast-memcpy/4 nil)))

  (let* ((dst   (copy-fast-sap/4 dst))
	 (src   (copy-fast-sap/4 src))
	 (chunk 65536)
	 (n     (min chunk n-words))
	 (buf   (java:jnew-array "int" n)))

    ;; fast-sap/4 i.e. java.nio.IntBuffer offsets are counted in ints, not in bytes
    (fast-sap/4-incposition dst dst-index
			    :scale (ash dst-scale -2) :offset dst-offset)
    (fast-sap/4-incposition src src-index
			    :scale (ash src-scale -2) :offset src-offset)

    (loop while (plusp n-words)
       do
	 (setf n (min chunk n-words))
	 ;; bulkget and bulkput offset refer to the int[], not to the buffer! 
	 (java:jcall +fast-sap/4-bulkget+ src buf 0 n)
	 (java:jcall +fast-sap/4-bulkput+ dst buf 0 n)
	 (decf n-words n))))


(defun fast-memcpy/8 (dst dst-index src src-index n-words &key
			(dst-scale 8) (dst-offset 0)
			(src-scale 8) (src-offset 0))
  (declare (type fast-sap/8 dst src)
	   (type fixnum dst-index dst-scale dst-offset)
	   (type fixnum src-index src-scale src-offset)
	   (type fixnum n-words))

  (when (<= n-words 8)
    ;; fast-sap/8 i.e. java.nio.LongBuffer offsets are counted in longs, not in bytes
    (let ((dst-index (fixnum+ (fixnum* dst-index (ash dst-scale -3)) dst-offset))
	  (src-index (fixnum+ (fixnum* src-index (ash src-scale -3)) src-offset)))
      (dotimes (i n-words)
	(java:jcall +fast-sap/8-put+ dst (fixnum+ i dst-index)
		    (java:jcall +fast-sap/8-get+ src (fixnum+ i src-index))))
      (return-from fast-memcpy/8 nil)))

  (let* ((dst   (copy-fast-sap/8 dst))
	 (src   (copy-fast-sap/8 src))
	 (chunk 32768)
	 (n     (min chunk n-words))
	 (buf   (java:jnew-array "long" n)))

    ;; fast-sap/8 i.e. java.nio.LongBuffer offsets are counted in longs, not in bytes
    (fast-sap/8-incposition dst dst-index
			    :scale (ash dst-scale -3) :offset dst-offset)
    (fast-sap/8-incposition src src-index
			    :scale (ash src-scale -3) :offset src-offset)

    (loop while (plusp n-words)
       do
	 (setf n (min chunk n-words))
	 ;; bulkget and bulkput offset refer to the int[], not to the buffer! 
	 (java:jcall +fast-sap/8-bulkget+ src buf 0 n)
	 (java:jcall +fast-sap/8-bulkput+ dst buf 0 n)
	 (decf n-words n))))
