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


(defun !hex (value)
  (format t "#x~X" value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; memset ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           
(defun !memset-bytes (ptr start-byte n-bytes fill-byte)
  (declare (type maddress ptr)
           (type (unsigned-byte 8) fill-byte)
           (type mem-word start-byte n-bytes))
  
  #-abcl
  (when (> n-bytes 32)
    (unless (zerop start-byte) (setf ptr (cffi-sys:inc-pointer ptr start-byte)))
    (osicat-posix:memset ptr fill-byte n-bytes)
    (return-from !memset-bytes nil))

  (let ((i start-byte)
        (end (the mem-word (+ start-byte n-bytes))))
    (declare (type mem-word i end))
    (loop while (< i end)
       do
         (mset-byte ptr i fill-byte)
         (incf i))))


(defun memset-words (ptr start-index n-words fill-word)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type maddress ptr)
           (type mem-word fill-word)
           (type mem-size start-index n-words))

  (symbol-macrolet ((i start-index)
                    (end end-index))

    #?+hlmem/fast-memset
    (fast-memset-words (sap=>fast-sap ptr) i n-words fill-word)

    #?-hlmem/fast-memset
    (progn
      ;; ARM has no SAP+INDEX*SCALE+OFFSET addressing,
      #?+(and hlmem/fast-mem (or x86 x86-64))
      (let ((sap      (the fast-sap (sap=>fast-sap ptr))))

        (loop while (>= n-words 8)
           do
             (fast-mset-word fill-word sap i :offset (* 0 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 1 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 2 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 3 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 4 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 5 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 6 +msizeof-word+))
             (fast-mset-word fill-word sap i :offset (* 7 +msizeof-word+))
             (incf-mem-size i 8)
             (decf-mem-size n-words 8))

        (loop while (> n-words 0)
           do
             (fast-mset-word fill-word sap i)
             (incf-mem-size i)
             (decf-mem-size n-words)))
      
      #?-(and hlmem/fast-mem (or x86 x86-64))
      (progn
        #+(and sbcl (not x86))
        ;; 32-bit x86 is register-starved
        (loop while (>= n-words 4)
           do
             (let ((i1 (mem-size+ i 1))
                   (i2 (mem-size+ i 2))
                   (i3 (mem-size+ i 3)))
               (mset-word ptr i  fill-word)
               (mset-word ptr i1 fill-word)
               (mset-word ptr i2 fill-word)
               (mset-word ptr i3 fill-word)
               (incf-mem-size i 4)
               (decf-mem-size n-words 4)))
        
        (loop while (> n-words 0)
           do
             (mset-word ptr i fill-word)
             (incf-mem-size i)
             (decf-mem-size n-words))))))

#?+hlmem/fast-memset
(define-compiler-macro memset-words (&whole form ptr start-index n-words fill-word)
  (if (constantp n-words)
      `(fast-memset-words ,ptr ,start-index ,(eval n-words) ,fill-word)
      form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mzero ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mzero-bytes))
(defun !mzero-bytes (ptr start-byte n-bytes)
  (declare (type maddress ptr)
           (type mem-word start-byte n-bytes))
  (!memset-bytes ptr 0 start-byte n-bytes))


#?+(or hlmem/fast-memset (and hlmem/fast-mem (or x86 x86-64)))
(declaim (inline mzero-words))
(defun mzero-words (ptr start-index n-words)
  (declare (type maddress ptr)
           (type mem-size start-index n-words))

  #?-(or hlmem/fast-memset (and hlmem/fast-mem (or x86 x86-64)))
  (progn
    #-abcl
    (when (> n-words 32)
      (unless (zerop start-index)
	(setf ptr (cffi-sys:inc-pointer ptr (* start-index +msizeof-word+))))
      (osicat-posix:memset ptr 0 (* n-words +msizeof-word+))
      (return-from mzero-words nil)))

  (memset-words ptr 0 start-index n-words))
  

#?+hlmem/fast-memset
(define-compiler-macro mzero-words (&whole form ptr start-index n-words)
  (if (constantp n-words)
      `(fast-memset-words ,ptr ,start-index ,(eval n-words) 0)
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; memcpy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun !memcpy-bytes (dst dst-byte src src-byte n-bytes)
  (declare (type maddress dst src)
           (type mem-word dst-byte src-byte n-bytes))
  (symbol-macrolet ((si src-byte)
                    (di dst-byte))
    #-abcl
    (when (> n-bytes 32)
      (unless (zerop di) (setf dst (cffi-sys:inc-pointer dst di)))
      (unless (zerop si) (setf src (cffi-sys:inc-pointer src si)))
      (osicat-posix:memcpy dst src n-bytes)
      (return-from !memcpy-bytes nil))
        
    (dotimes (i n-bytes)
      (mset-byte dst (mem-size+ i di)
		 (mget-byte src (mem-size+ i si))))))

(defun memcpy-words (dst dst-index src src-index n-words)
  (declare (type maddress dst src)
           (type mem-size dst-index src-index n-words))

  (symbol-macrolet ((si src-index)
                    (di dst-index))

    #?+hlmem/fast-memcpy
    (fast-memcpy-words (sap=>fast-sap dst) di (sap=>fast-sap src) si n-words)
      
    #?-hlmem/fast-memcpy
    (progn
      #-abcl ;; no osicat-posix on ABCL yet :-(
      (when (> n-words #+sbcl 64 #-sbcl 16)
	(unless (zerop di) (setf dst (cffi-sys:inc-pointer dst (* di +msizeof-word+))))
	(unless (zerop si) (setf src (cffi-sys:inc-pointer src (* si +msizeof-word+))))
	(osicat-posix:memcpy dst src (* n-words +msizeof-word+))
	(return-from memcpy-words nil))

      (loop with end = (mem-size+ si n-words)
	 while (< si end)
	 do
	   (mset-word dst di (mget-word src si))
	   (incf-mem-size si)
	   (incf-mem-size di)))))


#?+hlmem/fast-memcpy
(define-compiler-macro memcpy-words (&whole form dst dst-index src src-index n-words)
  (if (constantp n-words)
      `(fast-memcpy-words ,dst ,dst-index ,src ,src-index ,(eval n-words))
      form))
