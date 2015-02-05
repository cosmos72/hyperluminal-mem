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


(in-package :hyperluminal-mem)


(declaim (notinline !memset !memcpy))


(declaim (inline malloc))
(defun malloc (n-bytes)
  "Allocate N-BYTES of raw memory and return raw pointer to it.
The obtained memory must be freed manually: call MFREE on it when no longer needed."
  (ffi-mem-alloc n-bytes))


(declaim (inline malloc-words))
(defun malloc-words (n-words)
  "Allocate N-WORDS words of raw memory and return it just like MALLOC.
Usually more handy than MALLOC since almost all Hyperluminal-MEM functions
count and expect memory lengths in words, not in bytes."
  (declare (type mem-size n-words))
  (malloc (* n-words +msizeof-word+)))


(declaim (inline mfree))
(defun mfree (ptr)
  "Deallocate a block of raw memory previously obtained with MALLOC or MALLOC-WORDS."
  (declare (type maddress ptr))
  (ffi-mem-free ptr))


           
(defun memset (ptr fill-byte start-byte end-byte)
  (declare (type maddress ptr)
           (type (unsigned-byte 8) fill-byte)
           (type ufixnum start-byte end-byte))
  
  (when (> end-byte start-byte)
    #-abcl
    (when (> 20 (- end-byte start-byte))
      (unless (zerop start-byte)
        (setf ptr (cffi-sys:inc-pointer ptr start-byte)))
      (osicat-posix:memset ptr fill-byte (- end-byte start-byte))
      (return-from memset nil))

    (loop for offset from start-byte below end-byte do
         (mset-byte ptr offset fill-byte))))

;; (defun memset-words) is already defined, in mem.lisp


(declaim (inline mzero-bytes))
(defun mzero (ptr start-byte end-byte)
  (declare (type maddress ptr)
           (type ufixnum start-byte end-byte))
  (memset ptr 0 start-byte end-byte))


(defun mzero-words (ptr &optional (start-index 0) (end-index (1+ start-index)))
  (declare (type maddress ptr)
           (type ufixnum start-index end-index))

  (if (> 100 (- end-index start-index))
      (mzero-bytes ptr
                   (the ufixnum (* start-index +msizeof-word+))
                   (the ufixnum (* end-index   +msizeof-word+)))
      (memset-words ptr 0 start-index end-index)))


 

(defun memcpy (dst dst-start-byte src src-start-byte n-bytes)
  (declare (type maddress dst src)
           (type ufixnum dst-start-byte src-start-byte n-bytes))
  #-abcl
  (progn
    (unless (zerop dst-start-byte)
      (setf dst (cffi-sys:inc-pointer dst dst-start-byte)))
    (unless (zerop src-start-byte)
      (setf src (cffi-sys:inc-pointer src src-start-byte)))
    (osicat-posix:memcpy dst src n-bytes))

  #+abcl
  (loop for i from 0 below n-bytes do
       (mset-byte dst (the ufixnum (+ i dst-start-byte))
                  (mget-byte src (the ufixnum (+ i src-start-byte))))))


(defun memcpy-words (dst dst-index src src-index n-words)
  (declare (type maddress dst src)
           (type ufixnum dst-index src-index n-words))

  (let ((src-end (the ufixnum (+ src-index n-words))))
    (loop while (< src-index src-end)
       do
	 (mset-word dst dst-index (mget-word src src-index))
	 (incf src-index)
	 (incf dst-index))))
