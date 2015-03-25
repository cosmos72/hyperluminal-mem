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


(eval-always
  (unless (= 1 +sfloat/words+)
    (error "cannot compile STMX: SINGLE-FLOAT (mapped to CFFI ~S) is ~S bytes,
  supported values are <= ~S bytes"
           (parse-type :dfloat) +msizeof-sfloat+ +msizeof-word+))
  (unless (<= 1 +dfloat/words+ 2)
    (error "cannot compile STMX: DOUBLE-FLOAT (mapped to CFFI ~S) is ~S bytes,
  supported values are <= ~S bytes"
           (parse-type :dfloat) +msizeof-dfloat+ (* 2 +msizeof-word+))))


#+(or ccl sbcl)
(declaim (inline sfloat-bits))
(defun sfloat-bits (value ptr byte-offset)
  (declare (optimize (safety 0) (speed 3))
           (type single-float value)
           (type maddress ptr)
           (type mem-word byte-offset)
           (ignorable ptr byte-offset))
  
  #+ccl  (the (unsigned-byte 32) (ccl::single-float-bits value))
  #+sbcl (the (unsigned-byte 32)
              (logand #.+mem-sfloat/mask+
                      (sb-kernel:single-float-bits value)))
  #-(or ccl sbcl)
  (progn
    (%mset-t value :sfloat ptr byte-offset)
    (%mget-t :sfloat-word ptr byte-offset)))

#+(or ccl sbcl)
(declaim (inline make-sfloat))
(defun make-sfloat (bits)
  (declare (optimize (safety 0) (speed 3))
           (type (unsigned-byte 32) bits))
  #+ccl  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+sbcl (sb-kernel:make-single-float (the (signed-byte 32) bits))
  #-(or ccl sbcl)
  (with-mem-words (tmp #.+sfloat/words+)
    (%mset-t bits :sfloat-word tmp 0)
    (%mget-t :sfloat tmp 0)))

#+(or ccl sbcl)
(declaim (inline dfloat-bits))
(defun dfloat-bits (value ptr byte-offset)
  (declare (optimize (safety 0) (speed 3))
           (type double-float value)
           (type maddress ptr)
           (type mem-word byte-offset)
           (ignorable ptr byte-offset))
  #+ccl  (ccl::double-float-bits value)
  #+sbcl (values
          (logand (sb-kernel:double-float-high-bits value) #xFFFFFFFF)
          (sb-kernel:double-float-low-bits value))
  #-(or ccl sbcl)
  (progn
    (%mset-t value :dfloat ptr byte-offset)

    (let ((n1 (%mget-t :half-dfloat-word ptr byte-offset))
          (n2 (%mget-t :half-dfloat-word ptr
                       (the mem-word (+ byte-offset +msizeof-half-dfloat+)))))
      ;; always return HI LO
      #?+little-endian (values n2 n1)
      #?-little-endian (values n1 n2))))

#+(or ccl sbcl)
(declaim (inline make-dfloat))
(defun make-dfloat (hi lo)
  (declare (optimize (safety 0) (speed 3)))

  #+ccl  (ccl::double-float-from-bits hi lo)
  #+sbcl (sb-kernel:make-double-float (the (signed-byte 32) hi) lo)
  #-(or ccl sbcl)
  (with-mem-words (tmp #.+dfloat/words+)
    (let #?+little-endian ((n1 lo) (n2 hi))
         #?-little-endian ((n2 lo) (n1 hi))
         
         (%mset-t n1 :half-dfloat-word tmp 0)
         (%mset-t n2 :half-dfloat-word tmp +msizeof-half-dfloat+)

         (%mget-t :dfloat tmp 0))))


  

(declaim (inline mset-sfloat))
(defun mset-sfloat (value ptr byte-offset)
  (declare (type single-float value)
           (type maddress ptr)
           (type mem-word byte-offset))

  #?+(or abcl hlmem/native-endianity)
  (%mset-t value :sfloat ptr byte-offset)

  #?-(or abcl hlmem/native-endianity)
  (let* ((bits  (sfloat-bits value ptr byte-offset))
         (xbits (maybe-invert-endianity/integer :sfloat-word bits)))
    (%mset-t xbits :sfloat-word ptr byte-offset)
    value))



(declaim (inline mget-sfloat))
(defun mget-sfloat (ptr byte-offset)
  (declare (type maddress ptr)
           (type mem-word byte-offset))

  #?+(or abcl hlmem/native-endianity)
  (%mget-t :sfloat ptr byte-offset)

  #?-(or abcl hlmem/native-endianity)
  (let* ((xbits (%mget-t :sfloat-word ptr byte-offset))
         (bits  (maybe-invert-endianity/integer :sfloat-word xbits)))
    (make-sfloat bits)))


;; ABI: double-float LOW bits are always at lower address, HIGH bits ad higher address,
;; independently from chosen endianity.
;; reason: to provide trivial conversion between little and big endian ABIs.
;;
;; In practice, if CPU is big endian and double-float is wider than mem-word,
;; i.e. (> +msizeof-dfloat+ +msizeof-word),
;; double-floats are serialized by first splitting them into HIGH and LOW bits,
;; then swapping the HIGH and LOW values.
(eval-always
  (set-feature 'hlmem/dfloat/native
               (or
                #?+(and hlmem/native-endianity
                        (or cpu/double-float/unaligned (eql :hlmem/dfloat/words 1))
                        (or cpu/little-endian          (eql :hlmem/dfloat/words 1)))
                t)))


#?+(or abcl ccl sbcl hlmem/native-endianity)
(declaim (inline mset-dfloat))
(defun mset-dfloat (value ptr byte-offset)
  (declare (type double-float value)
           (type maddress ptr)
           (type mem-word byte-offset))

  #?+hlmem/dfloat/native
  (progn
    (%mset-t value :dfloat ptr byte-offset)
    value)

  #?-hlmem/dfloat/native
  (multiple-value-bind (hi lo) (dfloat-bits value ptr byte-offset)
        
    #?+(eql :hlmem/dfloat/words 1)
    (let* ((bits  (logior lo (ash hi #.(/ +mem-dfloat/bits+ 2))))
           (xbits (maybe-invert-endianity/integer :dfloat-word bits)))
      (%mset-t xbits :word ptr byte-offset) ; not :dfloat-word, in case word > double-float
      value)
    
    #?+(eql :hlmem/dfloat/words 2)
    (progn
      ;; always write LO at lower address and HI ad higher address
      ;; to provide uniform word-based endianity.
      ;; i.e. when mem-word is 4 bytes, we do *not* write 8-byte double-floats,
      ;; instead we split double-floats in LO and HI words, and write them separately
      ;; using configured endianity
      (%mset-t (maybe-invert-endianity/integer :word lo) ; not :half-dfloat-word,
                                        ; in case word > half-double-float
               :word ptr byte-offset)
      (%mset-t (maybe-invert-endianity/integer :word hi) ; idem
               :word ptr (the mem-word (+ byte-offset +msizeof-word+)))
      value)
    
    #?-(or (eql :hlmem/dfloat/words 1) (eql :hlmem/dfloat/words 2))
    (error "~S not implemented for ~S = ~S" 'mset-dfloat :hlmem/dfloat/words
           (get-feature :hlmem/dfloat/words))))


#?+(or abcl ccl sbcl hlmem/native-endianity)
(declaim (inline mget-dfloat))
(defun mget-dfloat (ptr byte-offset)
  (declare (type maddress ptr)
           (type mem-word byte-offset))

  #?+hlmem/dfloat/native
  (%mget-t :dfloat ptr byte-offset)

  #?-hlmem/dfloat/native
  (progn
    #?+(eql :hlmem/dfloat/words 1)
    (let* ((xbits (logand +mem-dfloat/mask+
                          ;; not :dfloat-word, in case word > double-float
                          (%mget-t :word ptr byte-offset)))
           (bits  (maybe-invert-endianity/integer :dfloat-word xbits)))
      (make-dfloat (ash    bits #.(- +mem-half-dfloat/bits+))
                   (logand bits #.+mem-half-dfloat/mask+)))
      
    #?+(eql :hlmem/dfloat/words 2)
                                        ; not :half-dfloat-word, in case word > half-double-float
    (let* ((xlo (logand +mem-half-dfloat/mask+ (%mget-t :word ptr byte-offset)))
           (lo (maybe-invert-endianity/integer :half-dfloat-word xlo))
           
           (xhi (logand +mem-half-dfloat/mask+ (%mget-t :word ptr
                                                        (the mem-word (+ byte-offset
                                                                         +msizeof-word+)))))
           (hi (maybe-invert-endianity/integer :half-dfloat-word xhi)))

      (make-dfloat hi lo))
    
    #?-(or (eql :hlmem/dfloat/words 1) (eql :hlmem/dfloat/words 2))
    (error "~S not implemented for ~S = ~S" 'mget-dfloat :hlmem/dfloat/words
           (get-feature :hlmem/dfloat/words))))
