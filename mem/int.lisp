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


;; use the fastest available implementation of mword=>mem-int
;;
;; deferred from fast-mem.lisp, it needs #?+hlmem/mem-int=fixnum
;; computed in constants.lisp
(eval-always
  (let ((sym (get-fbound-symbol 'hlm-asm (stringify 'fast-mword/ +msizeof-word+ '=>fixnum))))
    
    ;; hlm-asm:fast-mword=>fixnum is usable for mword=>mem-int
    ;; only if mem-int equals fixnum
    (set-feature 'hlmem/mword=>mem-int
                 ;; we store sym in the features!
                 (if (get-feature :hlmem/mem-int=fixnum) sym nil))))
     


(defmacro mword=>mem-int (word)
  #?+hlmem/mword=>mem-int
  `(,(get-feature :hlmem/mword=>mem-int) ,word)

  #?-hlmem/mword=>mem-int
  (with-gensym x
    `(locally
         (declare (optimize (safety 0) (speed 3)))
       (let ((,x ,word))
         (the mem-int (- (logand +mem-int/value-mask+ ,x)
                         (logand +mem-int/sign-mask+ ,x)))))))


(defmacro mem-int=>mword (value)
  #?+hlmem/mem-int=fixnum
  `(logior +mem-int/flag+
           ;; on some archs, SBCL is smart enough to optimize away this
           #+(and sbcl (or x86 x86-64 arm)) (logand +mem-word/mask+ ,value)
           #-(and sbcl (or x86 x86-64 arm)) (logand +mem-int/mask+ ,value))

  #?-hlmem/mem-int=fixnum
  `(logior +mem-int/flag+
           (logand +mem-int/mask+ ,value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline mget-uint))
(defun mget-uint (ptr index)
  "Return the two's complement value of mem-int stored at (PTR+INDEX),
ignoring any sign bit"
  (declare (type maddress ptr)
           (type mem-size index))

  (the mem-uint (logand +mem-int/value-mask+ (mget-word ptr index))))


(declaim (inline mget-int))
(defun mget-int (ptr index)
  "Return the mem-int stored at (PTR+INDEX)"
  (declare (type maddress ptr)
           (type mem-size index))

  (mword=>mem-int (mget-word ptr index)))


(declaim (inline mset-int))
(defun mset-int (ptr index value)
  "Write mem-int VALUE into the memory at (PTR+INDEX)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-int value)
           (optimize (safety 0) (speed 3)))

  (mset-word ptr index (mem-int=>mword value))
  t)

(defsetf mget-int mset-int)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#?+hlmem/fast-mem
(define-compiler-macro mget-int (&whole form ptr index)
  (if (constantp index)
      `(mword=>mem-int
        (fast-mget-word (the maddress ,ptr) 0 :offset (* +msizeof-word+ ,index)))
      form))
        
#?+hlmem/fast-mem
(define-compiler-macro mset-int (&whole form ptr index value)
  (if (constantp index)
      (with-gensym p
        ;; preserve evaluation order
        `(let ((,p (the maddress ,ptr)))
           (fast-mset-word (mem-int=>mword (the mem-int ,value))
                           ,p 0 :offset (* +msizeof-word+ ,index))
           t))
      form))
        
#?-hlmem/fast-mem
;; sanity
(eval-always
  (setf (compiler-macro-function 'mget-int) nil
        (compiler-macro-function 'mset-int) nil))
