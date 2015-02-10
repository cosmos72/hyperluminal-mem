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

(enable-#?-syntax)

(declaim (inline malloc))
(defun malloc (n-bytes)
  "Allocate N-BYTES of raw memory and return raw pointer to it.
The obtained memory must be freed manually: call MFREE on it when no longer needed."
  (ffi-mem-alloc n-bytes))


(declaim (inline malloc-words))
(defun malloc-words (n-words)
  "Allocate N-WORDS words of raw memory and return raw pointer to it.
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
           (type mem-word start-byte end-byte))
  
  (when (> end-byte start-byte)
    #-abcl
    (let ((n-bytes (- end-byte start-byte)))
      (declare (type mem-word n-bytes))
      (when (> n-bytes 32)
        (unless (zerop start-byte)
          (setf ptr (cffi-sys:inc-pointer ptr start-byte)))
        (osicat-posix:memset ptr fill-byte n-bytes)
        (return-from memset nil)))

    (let ((i start-byte))
      (declare (type mem-word start-byte))
      (loop while (< i end-byte) do
           (mset-byte ptr i fill-byte)
           (incf i)))))


(defun memset-words (ptr fill-word start-index end-index)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type maddress ptr)
           (type mem-word fill-word)
           (type mem-size start-index end-index))

  (when (> end-index start-index)
    ;; ARM has no SAP+INDEX*SCALE+OFFSET addressing,
    ;; and 32-bit x86 is register-starved, so this currently leaves x86-64
    #?+(and hlmem/fast-mem x86-64)
    (let ((i        (the mem-size start-index))
          (sap      (the hl-asm:fast-sap (hl-asm:sap=>fast-sap ptr))))
    
      (let ((bulk-end (the mem-size (+ i (logand -8 (- end-index i))))))
        (loop while (< i bulk-end)
           do
             (fast-mset-word fill-word sap i :offset (* 0 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 1 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 2 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 3 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 4 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 5 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 6 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 7 +msizeof-word+))
             (incf i 8)))

      (loop while (< i end-index)
         do
           (fast-mset-word fill-word sap i)
           (incf i)))
    
    #?-(and hlmem/fast-mem x86-64)
    (let ((i (the mem-size start-index)))
      ;; 32-bit x86 is register-starved
      #-x86
      (let ((bulk-end (the mem-size (+ i (logand -4 (the mem-size (- end-index i)))))))
        (loop while (< i bulk-end)
           do
             (let ((i1 (mem-size+ i 1))
                   (i2 (mem-size+ i 2))
                   (i3 (mem-size+ i 3)))
               (mset-word ptr i  fill-word)
               (mset-word ptr i1 fill-word)
               (mset-word ptr i2 fill-word)
               (mset-word ptr i3 fill-word)
               (incf-mem-size i 4))))
      
      (loop while (< i end-index)
         do
           (mset-word ptr i fill-word)
           (incf-mem-size i)))))



(declaim (inline mzero))
(defun mzero (ptr start-byte end-byte)
  (declare (type maddress ptr)
           (type mem-word start-byte end-byte))
  (memset ptr 0 start-byte end-byte))


(defun mzero-words (ptr start-index end-index)
  (declare (type maddress ptr)
           (type mem-size start-index end-index))

  (when (> end-index start-index)
    #-abcl
    (when (> 128 (- end-index start-index))
      (return-from mzero-words
        (memset ptr 0
		(the mem-word (* start-index +msizeof-word+))
		(the mem-word (* end-index   +msizeof-word+)))))

    (memset-words ptr 0 start-index end-index)))
  



 

(defun memcpy (dst dst-start-byte src src-start-byte n-bytes)
  (declare (type maddress dst src)
           (type mem-word dst-start-byte src-start-byte n-bytes))
  #-abcl
  (when (> n-bytes 32)
    (unless (zerop dst-start-byte)
      (setf dst (cffi-sys:inc-pointer dst dst-start-byte)))
    (unless (zerop src-start-byte)
      (setf src (cffi-sys:inc-pointer src src-start-byte)))
    (osicat-posix:memcpy dst src n-bytes)
    (return-from memcpy nil))

  (loop for i from 0 below n-bytes do
       (mset-byte dst (the mem-size (+ i dst-start-byte))
                  (mget-byte src (the mem-size (+ i src-start-byte))))))


(defun memcpy-words (dst dst-index src src-index n-words)
  (declare (type maddress dst src)
           (type mem-size dst-index src-index n-words))

  #-abcl
  (when (> n-words 64)
    (return-from memcpy-words
      (memcpy dst (* dst-index +msizeof-word+)
	      src (* src-index +msizeof-word+)
	      (* n-words +msizeof-word+))))

  (let* ((si (the mem-size src-index))
	 (di (the mem-size dst-index))
	 (n-words (the mem-size (min (- +most-positive-size+ si)
				     (- +most-positive-size+ di)
				     n-words)))
	 (end (mem-size+ (+ si n-words))))
    
    (loop while (< si end)
       do
	 (mset-word dst di (mget-word src si))
	 (incf-mem-size si)
	 (incf-mem-size di))))



