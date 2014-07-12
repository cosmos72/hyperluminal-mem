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

(eval-always
  (defun string-to-code-vector (s)
    (declare (type string s))
    (let* ((n (length s))
           (v (make-array n)))
      (loop for i from 0 below n do
           (setf (svref v i) (char-code (char s i))))
      v)))

(define-global *endian-magic*
    (string-to-code-vector (if +mem/little-endian+ "hldb" "HLDB")))

(define-global *x-endian-magic*
    (string-to-code-vector (if +mem/little-endian+ "HLDB" "hldb")))

(define-global *magic-write-list*
    '((4  abi-major-version   #.(first  +hlmem-abi-version+))
      (5  abi-minor-version   #.(second +hlmem-abi-version+))
      (6  abi-patch-version   #.(third  +hlmem-abi-version+))
      (7  bits-per-tag        #.+mem-tag/bits+)
      (8  sizeof-word         #.+msizeof-word+)
      (9  sizeof-single-float #.+msizeof-sfloat+)
      (10 sizeof-double-float #.+msizeof-dfloat+)
      (11 unused              0)
      (12 unused              0)
      (13 unused              0)
      (14 unused              0)
      (15 unused              0)))


(define-global *magic-read-list*
    (remove-if (lambda (x) (member (second x) '(abi-minor-version abi-patch-version unused)))
               *magic-write-list*)
  "When opening an HLDB file or exchanging HLMEM serialized data with another process,
we do not check ABI-MINOR-VERSION and ABI-PATCH-VERSION:
they are allowed to differ between data and compiled library")


(define-global *magic*
    (concatenate 'vector *endian-magic*
                 (mapcar #'third *magic-write-list*)))

(define-global *zero-magic* (make-array (length *magic*) :initial-element 0))

(defun mwrite-magic (ptr index end-index)
  (declare (type maddress ptr)
           (type mem-size index end-index))

  (let* ((n-bytes (length *magic*))
         (n-words (ceiling n-bytes +msizeof-word+))
         (byte-index (* index +msizeof-word+)))
    (check-mem-overrun ptr index end-index n-words)

    (loop for i from 0 below n-bytes do
         (setf (mget-byte ptr (+ i byte-index)) (svref *magic* i)))
    (+ index n-words)))


(defun mread-magic (ptr index end-index)
  (declare (type maddress ptr)
           (type mem-size index end-index))

  (let* ((n-bytes (length *magic*))
         (n-words (ceiling n-bytes +msizeof-word+))
         (byte-index (* index +msizeof-word+))
         (magic (make-array n-bytes)))

    (check-mem-length ptr index end-index n-words)

    (loop for i from 0 below n-bytes do
         (setf (svref magic i) (mget-byte ptr (+ i byte-index))))

    (when (equalp magic *zero-magic*)
      (return-from mread-magic nil))

    (let1 endian-magic (subseq magic 0 (length *endian-magic*))

      (unless (equalp endian-magic *endian-magic*)

        (let ((list   (mapcar #'code-char (coerce endian-magic 'list)))
              (n-list (mapcar #'code-char (coerce *endian-magic* 'list))))

          (error "HYPERLUMINAL-DB: unsupported file format.
expecting magic sequence (~{~S~^ ~}), found (~{~S~^ ~})~A"
                 n-list list
                 (if (equalp endian-magic *x-endian-magic*)
                     "
file was created on a system with opposite endianity"
                     "")))))

    (loop for (i name expected) in *magic-read-list*
       for value = (svref magic i)
       unless (eql value expected) do
         (error "HYPERLUMINAL-DB: unsupported file format. expecting ~S = ~S, found ~S"
                name expected value))

    (+ index n-words)))


