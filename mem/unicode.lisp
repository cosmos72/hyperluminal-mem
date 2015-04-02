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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          Unicode functions: codepoints, UTF-8                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-#?-syntax)


(defmacro %code-is-high-surrogate (code)
  `(<= #xD800 ,code #xDBFF))

(defmacro %code-is-low-surrogate (code)
  `(<= #xDC00 ,code #xDFFF))

(declaim (inline %codepoint-is-reserved))
(defun %codepoint-is-reserved (code)
  (declare (type codepoint code))
  (<= #xD800 code #xDFFF)) ;; high and low surrogates
      
      

(defmacro %char-is-high-surrogate (ch)
  `(%code-is-high-surrogate (char-code ,ch)))

(defmacro %char-is-low-surrogate (ch)
  `(%code-is-low-surrogate (char-code ,ch)))




#?-hlmem/character=utf-16
(progn
  (declaim (inline %codepoint->character))
  (defun %codepoint->character (code)
    "Convert Unicode codepoint to a character"
    (declare (optimize (speed 3) (safety 0) (debug 1))
             (type fixnum code))
    (code-char (logand code +character/mask+))))





;; support UTF-16 strings used at least by CMUCL and ABCL
#?+hlmem/character=utf-16
(progn
  (defmacro %utf-16->codepoint (code string char-func i n-chars)
    "Convert utf-16 CODE to Unicode codepoint. If CODE is a high-surrogate,
check next char in STRING: if it's a low-surrogate, consume it,
otherwise assume a low-surrogate equal #xDC00.
In any case, convert the code or the high/low surrogate pair to a codepoint."
    (with-gensyms (hi lo)
      `(let ((,hi ,code))
         (if (%code-is-high-surrogate ,hi)
             (let ((,lo (if (< ,i ,n-chars) (char-code (,char-func ,string ,i)) 0)))
               (if (%code-is-low-surrogate ,lo)
                   (incf (the fixnum ,i)) ;; low-surrogate, consume it
                   (setf ,lo #xDC00))     ;; invalid, pretend we found #xDC00
               (+ (ash ,hi 10) ,lo #x-35FDC00))
             ,hi))))

  (declaim (inline %codepoint->utf-16))
  (defun %codepoint->utf-16 (code)
    "Convert Unicode codepoint to one or two UTF-16 characters"
    (declare (optimize (speed 3) (safety 0) (debug 1))
             (type fixnum code))

    (if (<= code #xFFFF)
        (code-char code)
        (values (code-char (+ (ash code -10) #xD7C0)) ;; i.e. (+ (ash (- code #x10000) -10) #xD800)
                (code-char (+ (logand code #x3FF) #xDC00))))))





(declaim (inline %codepoint->utf-8-word))
(defun %codepoint->utf-8-word (n)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type codepoint n))

  (let ((word n)
        (bits 8))
    (declare (type (unsigned-byte 32) word)
             (type (member 8 16 24 32) bits))
    (cond
      ((<= n #x7F))
      ((<= n #x7FF) (setf word (logior #x80C0
                                       (ash (logand n #x03F)  8)
                                       (ash (logand n #x7C0) -6))
                          bits 16))
      ((<= n #xFFFF) (setf word (logior #x8080E0
                                        (ash (logand n #x003F)  16)
                                        (ash (logand n #x0FC0)   2)
                                        (ash (logand n #xF000) -12))
                           bits 24))
      (t             (setf word (logior #x808080F0
                                        (ash (logand n #x00003F)  24)
                                        (ash (logand n #x000FC0)  10)
                                        (ash (logand n #x03F000) -4)
                                        (ash (logand n #x1C0000) -18))
                           bits 32)))
    (values word bits)))



(defun invalid-utf8-error (byte)
  (declare (type (unsigned-byte 8) byte))
  (error "invalid byte. UTF-8 sequence cannot start with #x~X" byte))


(declaim (inline %utf-8-word->codepoint))
(defun %utf-8-word->codepoint (word)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type mem-word word))

  (let ((n 0)
        (bits 8)
        (byte0 (logand #xFF word)))
    
    (declare (type codepoint n)
             (type (member bits 8 16 24 32 bits)))
    (cond
      ((<= byte0 #x7F) (setf n byte0))
      
      ((<= byte0 #xDF)
       (setf n (logior (ash (logand #x3F00 word) -8)
                       (ash (logand #x001F word)  6))
             bits 16))

      ((<= byte0 #xEF)
       (setf n (logior (ash (logand #x3F0000 word) -16)
                       (ash (logand #x003F00 word)  -2)
                       (ash (logand #x00000F word)  12))
             bits 24))
      
      ((<= byte0 #xF7)
       (setf n (logior (ash (logand #x3F000000 word) -24)
                       (ash (logand #x003F0000 word) -10)
                       (ash (logand #x00003F00 word)   4)
                       (ash (logand #x00000007 word)  18))
             bits 32))

      (t
       (invalid-utf8-error byte0)))

    (values (the codepoint n) bits)))
                   
