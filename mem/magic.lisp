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

(eval-always
  (defun string-to-code-vector (s)
    (declare (type string s))
    (let* ((n (length s))
           (v (make-array n)))
      (dotimes (i n)
        (setf (svref v i) (char-code (char s i))))
      v)))

(define-global *endian-magic*
    (string-to-code-vector (if +mem/little-endian+ "hldb" "HLDB")))

(define-global *x-endian-magic*
    (string-to-code-vector (if +mem/little-endian+ "HLDB" "hldb")))


(eval-always
  (defun compact-sizeof (sizeof)
    (declare (type (integer 0) sizeof))

    (macrolet ((%compact-sizeof (sizeof)
                 `(let* ((bits% (integer-length ,sizeof))
                         ;; bump powers-of-2 to the previous interval
                         (bits (- bits% (if (zerop (logand sizeof (1- ,sizeof))) 1 0)))
                         (shift (- bits 3))
                         (mask (1- (ash 1 shift))))

                    (if (zerop (logand sizeof mask))
                        (the fixnum
                             (+ (* 4 bits) -12
                                (ash sizeof (- shift))))
                        nil))))
      (if (typep sizeof 'fixnum)
          (if (<= sizeof 8)
              sizeof
              (%compact-sizeof (the fixnum sizeof)))
          (%compact-sizeof (the bignum sizeof))))))

     
(eval-always
  (defun uncompact-sizeof (csizeof)
    (declare (type (unsigned-byte 8) csizeof))
    (when (<= csizeof 8)
      (return-from uncompact-sizeof csizeof))

    (let* ((mod-4 (logand csizeof 3))
           (power-of-2 (1- (ash csizeof -2))))

      ;;(log:info csizeof power-of-2 mod-4)
      (ash (+ 4 mod-4) power-of-2))))


     
      
(define-global *magic-write-list*
    `((4  abi-major-version   ,(first  +hlmem-abi-version+))
      (5  abi-minor-version   ,(second +hlmem-abi-version+))
      (6  abi-patch-version   ,(third  +hlmem-abi-version+))
      (7  bits-per-tag        ,+mem-tag/bits+)
      (8  compact-sizeof-word         ,(compact-sizeof +msizeof-word+)   ,+msizeof-word+)
      (9  compact-sizeof-single-float ,(compact-sizeof +msizeof-sfloat+) ,+msizeof-sfloat+)
      (10 compact-sizeof-double-float ,(compact-sizeof +msizeof-dfloat+) ,+msizeof-dfloat+)
      (11 bits-lost-per-mem-int ,(- +mem-word/bits+ +mem-int/bits+))
      (12 unused              0)
      (13 unused              0)
      (14 unused              0)
      (15 unused              0)))


(define-global *magic-read-list*
    (loop for x in *magic-write-list*
       unless (third x)
       do (error "HYPERLUMINAL-MEM compile error.
  The constant ~A = ~S cannot be represented as a compressed size.
  Please fix the customization in \"constants.lisp\" before recompiling."
                 (subseq (symbol-name (second x)) 8) (fourth x))
         
       unless (member (second x) '(abi-minor-version abi-patch-version unused))
       collect x)

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

          (error "HYPERLUMINAL-MEM: unsupported file format.
expecting magic sequence (~{~S~^ ~}), found (~{~S~^ ~})~A"
                 n-list list
                 (if (equalp endian-magic *x-endian-magic*)
                     "
file was created on a system with opposite endianity"
                     "")))))

    (loop for (i name expected) in *magic-read-list*
       for value = (svref magic i)
       unless (eql value expected) do
         (error "HYPERLUMINAL-MEM: unsupported file format. expecting ~S = ~S, found ~S"
                name expected value))

    (+ index n-words)))


