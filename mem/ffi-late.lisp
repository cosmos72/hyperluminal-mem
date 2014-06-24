;; -*- lisp -*-

;; This file is part of hyperluminal-DB.
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
  #+abcl (java:jnew-array +native-word-type+
                          (ceiling n-bytes +msizeof-word+)))


(defun mfree (ptr)
  "Deallocate a block of raw memory previously obtained with MALLOC."
  (declare (type maddress ptr))
  #-abcl (cffi-sys:foreign-free ptr))

           
(defun !memset (ptr fill-byte start-byte end-byte)
  (declare (type maddress ptr)
           (type (unsigned-byte 8) fill-byte)
           (type ufixnum start-byte end-byte))
  #-abcl
  (osicat-posix:memset (if (zerop start-byte)
                           ptr
                           (cffi-sys:inc-pointer ptr start-byte))
                       fill-byte
                       (- end-byte start-byte))
  #+abcl
  (error "!MEMSET not implemented on ABCL. Use !MEMSET-WORDS instead")

  nil)


(declaim (inline !mzero))

(defun !mzero (ptr start-byte end-byte)
  (declare (type maddress ptr)
           (type ufixnum start-byte end-byte))
  (!memset ptr 0 start-byte end-byte))



(defun !memcpy (dst src n-bytes)
  (declare (type maddress dst src)
           (type ufixnum n-bytes))
  #-abcl
  (osicat-posix:memcpy dst src n-bytes)

  #+abcl
  (error "!MEMCPY not implemented on ABCL. Use !MEMCPY-WORDS instead"))

