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

(deftype maddress () 'ffi-address)

(deftype ufixnum () '(and fixnum (integer 0)))


(eval-always
  (declaim (type keyword +chosen-word-type+))
  (defconstant +chosen-word-type+ (choose-word-type))

  (defconstant +native-word-type+ (ffi-native-type-name +chosen-word-type+))

  (declaim (inline sfloat-word-type dfloat-word-type))
  
  (defun parse-type (type)
    (case type
      (:sfloat :float)         ;; this is the ONLY code mapping :sfloat to a CFFI type
      (:dfloat :double)        ;; this is the ONLY code mapping :dfloat to a CFFI type
      (:byte   :unsigned-char) ;; this is the ONLY code mapping :byte to a CFFI type
      (:word   +chosen-word-type+) ;; :word is mapped to a CFFI type by (choose-word-type)
      (:sfloat-word (sfloat-word-type)) ;; an unsigned integer as wide as :sfloat
      (:dfloat-word (dfloat-word-type)) ;; an unsigned integer as wide as :dfloat
      (otherwise type)))


  (defun choose-float-word-type (type lisp-type)
    (let ((types '(:unsigned-char :unsigned-short :unsigned-int
                   :unsigned-long :unsigned-long-long)))
      (loop
         with float-size fixnum = (ffi-sizeof (parse-type type))
         for type in types
         do
           (when (= float-size (ffi-sizeof type))
             (return type))
         finally
           (error "cannot compile STMX: no CFFI integer type as wide as ~S (~S bytes),
  tried ~S" lisp-type float-size types))))

  (defconstant +sfloat-type+ (parse-type :sfloat))
  (defconstant +dfloat-type+ (parse-type :dfloat))
  (defconstant +sfloat-word-type+ (choose-float-word-type :sfloat 'single-float))
  (defconstant +dfloat-word-type+ (choose-float-word-type :dfloat 'double-dloat))

  (defun sfloat-word-type ()
     +sfloat-word-type+)

  (defun dfloat-word-type ()
     +dfloat-word-type+))




;; not really used, but handy
#-(and)
(eval-always
 (cffi:defctype mfloat  #.(parse-type :sfloat))
 (cffi:defctype mdouble #.(parse-type :dfloat))
 (cffi:defctype mbyte   #.(parse-type :byte))
 (cffi:defctype mword   #.(parse-type :word)))



(defmacro %msizeof (type)
  "Wrapper for (CFFI-SYS:%FOREIGN-TYPE-SIZE), interprets :SFLOAT :DFLOAT :BYTE AND :WORD"
  `(ffi-sizeof ,(if (constantp type)
                    (parse-type type)
                    `(parse-type ,type))))

(defmacro msizeof (type)
  "Wrapper for (%MSIZEOF), computes (CFFI:%FOREIGN-TYPE-SIZE) at compile time whenever possible"
  (if (constantp type)
      (%msizeof (eval type))
      `(%msizeof ,type)))



(eval-always
  (defconstant +msizeof-sfloat+  (msizeof :sfloat))
  (defconstant +msizeof-dfloat+  (msizeof :dfloat))
  (defconstant +msizeof-byte+    (msizeof :byte))
  (defconstant +msizeof-word+    (msizeof :word))
  (defconstant +sfloat/words+    (ceiling +msizeof-sfloat+ +msizeof-word+))
  (defconstant +dfloat/words+    (ceiling +msizeof-dfloat+ +msizeof-word+))
  (set-feature 'hlmem/sfloat/words +sfloat/words+)
  (set-feature 'hlmem/dfloat/words +dfloat/words+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro with-mem-bytes ((var-name n-bytes &optional n-bytes-var) &body body)
  `(with-ffi-mem (,var-name ,n-bytes ,@(when n-bytes-var `(,n-bytes-var)))
     ,@body))


(defmacro with-mem-words ((ptr n-words &optional n-words-var)
			      &body body)
  "Bind PTR to N-WORDS words of raw memory while executing BODY.
Raw memory is automatically deallocated when BODY terminates."

  (let* ((n-const? (constantp n-words))
         (n-words  (if n-const? (eval n-words) n-words))
         (n-bytes  (if n-const?
                       (* n-words +msizeof-word+)
                       `(the mem-word (* ,(or n-words-var n-words) +msizeof-word+)))))
    
    `(let ,(when n-words-var `((,n-words-var ,n-words)))
       (with-mem-bytes (,ptr ,n-bytes)
         ,@body))))

          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


