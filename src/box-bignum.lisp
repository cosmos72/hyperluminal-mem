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


(in-package :hyperluminal-db)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed    BIGNUMs                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline %bignum-words))

(defun %bignum-words (n)
  "Return the number of words needed to store bignum N in memory, not including BOX header
nor the N-WORDS prefix."
  (declare (type integer n))

  (the ufixnum
    (ceiling (integer-length n) +mem-word/bits+))) ;; round up 


(defun box-words/bignum (n)
  "Return the number of words needed to store bignum N in memory, not including BOX header."
  (declare (type integer n))

  (let ((words (%bignum-words n)))
    (unless (< words +mem-bignum/max-words+)
      (error "HYPERLUMINAL-DB: bignum too large for object store,
it requires ~S words, maximum supported is ~S words"
             (1+ words) +mem-bignum/max-words+))

    ;; add 1 word for N-WORDS prefix
    (the (integer 0 #.+mem-bignum/max-words+) (1+ words))))
  


(defun %mwrite-bignum-loop (ptr index n-words n)
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type integer n))

  (let ((shift (- +mem-word/bits+))
        (mask +mem-word/mask+))

    (loop for i-word from n-words downto 1 do
         (mset-word ptr index (logand n mask))
         (incf (the mem-size index))
         (setf n (ash n shift)))))


(defun %mwrite-bignum-recurse (ptr index n-words n)
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type integer n))

  (log.trace "index: ~S n-words: ~S n: #x~X" index n-words n)

  (if (<= n-words 16)
      (%mwrite-bignum-loop ptr index n-words n)

      (let* ((n-words-high (truncate n-words 2))
             (n-words-low  (- n-words n-words-high))
             (high-shift (the fixnum (* n-words-low +mem-word/bits+)))
             (low-mask (1- (ash 1 high-shift))))

        (%mwrite-bignum-recurse ptr index n-words-low (logand n low-mask))
        (%mwrite-bignum-recurse ptr (the mem-size (+ index n-words-low))
                                n-words-high (ash n (- high-shift))))))


(defun mwrite-box/bignum (ptr index n)
  "Write bignum N into memory starting at (PTR+INDEX).
Assumes BOX header is already written.
Returns number of words actually written.

ABI: writes mem-int N-WORDS, i.e. (%bignum-words N)
\(if bignum is negative, writes (lognot N-WORDS) instead)
followed by an array of words containing N in two's complement."
  (declare (type maddress ptr)
           (type mem-size index)
           (type integer n))

  (let ((n-words (%bignum-words n)))

    (mset-int ptr index (if (< n 0) (lognot n-words) n-words))
    (%mwrite-bignum-recurse ptr (mem-size+1 index) n-words n)

    ;; also add 1 word for N-WORDS prefix
    (mem-size+ +mem-box/header-words+ 1 n-words)))
    




(defun %mread-pos-bignum-loop (ptr index n-words)
  "Read an unsigned bignum"
  (declare (type maddress ptr)
           (type mem-size index n-words))

  (let* ((bits +mem-word/bits+)
         (limit (the fixnum (* bits n-words)))
         (n 0))
    (declare (type integer n))

    (loop for shift from 0 below limit by bits
       for word = (mget-word ptr (incf (the mem-size index)))
       do
         (setf n (logior n (ash word shift))))

    (the integer n)))


(defun %mread-neg-bignum-loop (ptr index n-words)
  "Read a negative bignum"
  (declare (type maddress ptr)
           (type mem-size index n-words))

  (when (zerop n-words)
    (return-from %mread-neg-bignum-loop -1))

  (decf (the mem-size n-words))

  (let* ((n (%mread-pos-bignum-loop ptr index n-words))
         ;; read last word as negative
         (bits  +mem-word/bits+)
         (limit (the fixnum (* bits n-words)))
         (word  (mget-word ptr (mem-size+ n-words 1 index))))

    (the integer (logior n (ash (logior word #.(- -1 +mem-word/mask+)) limit)))))




(defun %mread-bignum-recurse (ptr index n-words sign)
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type bit sign))

  (if (<= n-words 16)
      (if (zerop sign)
          (%mread-pos-bignum-loop ptr index n-words)
          (%mread-neg-bignum-loop ptr index n-words))

      (let* ((n-words-high (truncate n-words 2))
             (n-words-low  (- n-words n-words-high))

             (n-low  (%mread-bignum-recurse ptr index n-words-low 0))
             (n-high (%mread-bignum-recurse ptr (mem-size+ index n-words-low)
                                            n-words-high sign))

             (high-shift (the fixnum (* n-words-low +mem-word/bits+))))

        (log.trace "n-low: #x~X n-high: #x~X" n-low n-high)

        (logior n-low (ash n-high high-shift)))))


(defun mread-box/bignum (ptr index)
  "Read a bignum from the memory starting at (PTR+INDEX) and return it.
Also returns the number of words actually written as additional value.
Assumes the BOX header was read already."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (let* ((sign-n-words (mget-int ptr index))
         (sign         0)
         (n-words      sign-n-words))
    
    (when (< sign-n-words 0)
      (setf sign    1
            n-words (lognot n-words)))

    (values
     (%mread-bignum-recurse ptr index n-words sign)
     ;; also add 1 word for N-WORDS prefix
     (mem-size+ +mem-box/header-words+ 1 n-words))))



  
