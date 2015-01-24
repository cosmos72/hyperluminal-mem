;; -*- lisp -*-

;; This file is part of Hyperluminal-MEM.
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


(in-package :hyperluminal-mem)


(declaim (inline malloc mfree)
         (notinline !memset !memcpy))


(defun malloc (n-bytes)
  "Allocate N-BYTES of raw memory and return raw pointer to it.
The obtained memory must be freed manually: call MFREE on it when no longer needed."
  #-abcl (cffi-sys:%foreign-alloc n-bytes)
  #+abcl (java:jstatic "allocate" "java.nio.ByteBuffer" n-bytes))


(defun mfree (ptr)
  "Deallocate a block of raw memory previously obtained with MALLOC."
  (declare (type maddress ptr))
  #-abcl (cffi-sys:foreign-free ptr))

           
(defun !memset (ptr fill-byte start-byte end-byte)
  (declare (type maddress ptr)
           (type (unsigned-byte 8) fill-byte)
           (type ufixnum start-byte end-byte))
  #-abcl
  (progn
    (unless (zerop start-byte)
      (setf ptr (cffi-sys:inc-pointer ptr start-byte)))
    (osicat-posix:memset ptr fill-byte (- end-byte start-byte)))

  #+abcl
  (loop for offset from start-byte below end-byte do
       (mset-byte ptr offset fill-byte)))



(declaim (inline !mzero !mzero-words))

(defun !mzero (ptr start-byte end-byte)
  (declare (type maddress ptr)
           (type ufixnum start-byte end-byte))
  (!memset ptr 0 start-byte end-byte))


(defun !mzero-words (ptr &optional (start-index 0) (end-index (1+ start-index)))
  (declare (type maddress ptr)
           (type ufixnum start-index end-index))

  (if (> 100 (- end-index start-index))
      (!mzero ptr
              (the ufixnum (* start-index +msizeof-word+))
              (the ufixnum (* end-index   +msizeof-word+)))
      (!memset-words ptr 0 start-index end-index)))


(declaim (inline memcpy-words))

(defun memcpy-words (dst dst-index src src-index n-words)
  (declare (type maddress dst src)
           (type ufixnum dst-index src-index n-words))

  (let ((src-end (the ufixnum (+ src-index n-words))))
    (loop while (< src-index src-end)
       do
	 (mset-word dst dst-index (mget-word src src-index))
	 (incf src-index)
	 (incf dst-index))))


  

(defun !memcpy (dst dst-start-byte src src-start-byte n-bytes)
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

