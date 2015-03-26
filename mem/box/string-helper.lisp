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
;;;;    boxed    STRING. uses UTF-8 to reduce memory usage                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-#?-syntax)


(defmacro %code-is-high-surrogate (code)
  `(<= #xD800 ,code #xDBFF))

(defmacro %code-is-low-surrogate (code)
  `(<= #xDC00 ,code #xDFFF))

(declaim (inline %codepoint-is-reserved))
(defun %codepoint-is-reserved (code)
  (declare (type codepoint code))
  (or (<= #xD800 code #xDFFF) ;; high and low surrogates
      (= code #xFFFE)
      (= code #xFFFF)))
      
      

(defmacro %char-is-high-surrogate (ch)
  `(%code-is-high-surrogate (char-code ,ch)))

(defmacro %char-is-low-surrogate (ch)
  `(%code-is-low-surrogate (char-code ,ch)))




#?-hlmem/character<=FFFF
(progn
  (declaim (inline %codepoint->character))
  (defun %codepoint->character (code)
    "Convert Unicode codepoint to a character"
    (declare (optimize (speed 3) (safety 0) (debug 1))
             (type fixnum code))
    (code-char (logand code +character/mask+))))





;; support UTF-16 strings used at least by CMUCL and ABCL
#?+hlmem/character<=FFFF
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



