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


(deftype mem-int     () `(  signed-byte ,+mem-int/bits+))
(deftype mem-uint    () `(unsigned-byte ,+mem-int/value-bits+))

(deftype mem-word    () `(unsigned-byte ,+mem-word/bits+))


;; use the fastest available implementation of mword=>mem-int
(eval-always
  (let* ((name (stringify 'fast-mword/ +msizeof-word+ '=>fixnum))
         (pkg (find-package (symbol-name 'hl-asm)))
         (sym (when pkg (find-symbol name pkg))))
    
    (cond
      ;; hl-asm:fast-mword=>fixnum is usable for mword=>mem-int
      ;; only if mem-int equals fixnum
      ((and sym (get-feature 'hlmem/mem-int=fixnum))
       (set-feature 'hlmem/mword=>mem-int :asm)
       (defmacro mword=>mem-int (x)
         `(,sym ,x)))

      ;; cheat and tell the compiler to treat
      ;; an (unsigned-byte N) as a (signed-byte N)
      ;; this only works if the internal representation
      ;; of mem-int has NO space for bits above the sign bit,
      ;; which can happen only when mem-int equals fixnum,
      ;; and even in such case is known to work reliably only on SBCL
      #+sbcl
      ((get-feature 'hlmem/mem-int=fixnum)
       (set-feature 'hlmem/mword=>mem-int :logand)
       (defmacro mword=>mem-int (x)
         `(locally
              (declare (optimize (speed 3) (safety 0)))
            (the mem-int (logand ,x +mem-int/mask+)))))

      (t
       (set-feature 'hlmem/mword=>mem-int :slow)
       (defmacro mword=>mem-int (word)
         (with-gensym word_
           `(locally
                (declare (optimize (safety 0) (speed 3)))
              (let ((,word_ ,word))
                (the mem-int (- (logand +mem-int/value-mask+ ,word_)
                                (the #+sbcl mem-int ;; cheat a bit to get tighter compiled code
                                     #-sbcl mem-word
                                     (logand +mem-int/sign-mask+ ,word_))))))))))))

       
#?+hlmem/mem-int=fixnum
(defmacro mem-int=>mword (value)
  `(logior +mem-int/flag+
           #-sbcl (logand +mem-word/mask+ ,value) ;; faster
           #+sbcl (logand +mem-int/mask+ ,value)))

#?-hlmem/mem-int=fixnum
(defmacro mem-int=>mword (value)
  `(logior +mem-int/flag+
           (logand +mem-int/mask+ ,value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mset-int))
(defun mset-int (ptr index value)
  "Write mem-int VALUE into the memory at (PTR+INDEX)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-int value)
           (optimize (safety 0) (speed 3)))

  (mset-word ptr index (mem-int=>mword value))
  t)


(declaim (inline mget-int))
(defun mget-int (ptr index)
  "Return the mem-int stored at (PTR+INDEX)"
  (declare (type maddress ptr)
           (type mem-size index))

  (mword=>mem-int (mget-word ptr index)))


(defsetf mget-int mset-int)


(declaim (inline mget-uint))
(defun mget-uint (ptr index)
  "Return the two's complement value of mem-int stored at (PTR+INDEX),
ignoring any sign bit"
  (declare (type maddress ptr)
           (type mem-size index))

  (the mem-uint (logand +mem-int/value-mask+ (mget-word ptr index))))



