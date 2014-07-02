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


(in-package :hyperluminal-ffi)

(declaim (inline os-getpagesize))

(defun os-getpagesize ()
  #-abcl (osicat-posix:getpagesize)
  #+abcl 8192)   ;; just a guess...

(deftype fd ()
  #-abcl 'fixnum
  #+abcl '(or null java:java-object)) ;; java.nio.channels.FileChannel

(defconstant +bad-fd+
  #-abcl -1 
  #+abcl nil)



(declaim (inline bad-fd?))
(defun bad-fd? (fd)
  (declare (type fd fd))
  #-abcl (< fd 0)
  #+abcl (null fd))


(defun os-open-fd-rw (filename)
  #-abcl (osicat-posix:open filename (logior osicat-posix:o-rdwr osicat-posix:o-creat))
  #+abcl
  (let ((f (java:jnew "java.io.RandomAccessFile" filename "rw")))
    (java:jcall "getChannel" f)))


(defun os-close-fd (fd)
  (declare (type fd fd))
  #-abcl (osicat-posix:close fd)
  #+abcl (java:jcall "close" fd))


(defun os-stat-fd-size (fd)
  (declare (type fd fd))
  #-abcl (osicat-posix:stat-size (osicat-posix:fstat fd))
  #+abcl (java:jcall "size" fd))


(defun os-truncate-fd (fd bytes)
  (declare (type fd fd))
  #-abcl (osicat-posix:ftruncate fd bytes)
  #+abcl (java:jcall "truncate" fd bytes))


(defun os-mmap-fd-rw (fd offset-bytes length-bytes)
  (declare (type fd fd))

  #-abcl
  (osicat-posix:mmap +null-pointer+ length-bytes
                     (logior osicat-posix:prot-read osicat-posix:prot-write)
                     osicat-posix:map-shared
                     fd offset-bytes)

  #+abcl
  (let ((fd (java:jcall "map" fd
                        (java:jfield "java.nio.channels.FileChannel$MapMode" "READ_WRITE")
                        offset-bytes
                        length-bytes))
        (native-endian (java:jstatic "nativeOrder" "java.nio.ByteOrder")))
    (java:jcall "order" fd native-endian)
    fd))


(defun os-munmap-ptr (ptr length-bytes)
  (declare (type maddress ptr))
  #-abcl (osicat-posix:munmap ptr length-bytes)
  #+abcl nil) ;; MappedByteBuffer javadoc say it is unmapped when garbage collected.


(defun os-msync-ptr (ptr length-bytes sync)
  (declare (type maddress ptr)
           (type (integer 0) length-bytes))

  #-abcl
  (osicat-posix:msync ptr length-bytes
                      (if sync
                          #.(logior osicat-posix:ms-sync  osicat-posix:ms-invalidate)
                          #.(logior osicat-posix:ms-async osicat-posix:ms-invalidate)))
  #+abcl
  (java:jcall "force" ptr))


