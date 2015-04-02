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


(in-package :hyperluminal-mem-test)

(enable-#?-syntax)

(def-suite mem-string-suite :in suite)
(in-suite mem-string-suite)


#?+hlmem/character=utf-16
(defun make-rainbow-string ()
  (let* ((n #x110000)
         (string (make-array n :element-type 'character :adjustable t :fill-pointer 0)))

    (dotimes (code n)
      ;; UTF-16 cannot represent Unicode codepoints in the range #xD800 .. #xDFFF
      ;; which are anyway permanently reserved for high and low surrogates.
      ;;
      ;; It can represent the codepoints #xFFFE and #xFFFF,
      ;; but they are reserved too so we do not test them.
      (unless (hlmem::%codepoint-is-reserved code)
        (multiple-value-bind (ch1 ch2) (hlmem::%codepoint->utf-16 code)
          (vector-push-extend ch1 string)
          (when ch2
            (vector-push-extend ch2 string)))))
    string))


#?-hlmem/character=utf-16
(defun make-rainbow-string ()
  (let* ((n #x110000)
         (string (make-string n)))
    (dotimes (i n)
      ;; CCL (and possibly others) interpret strictly the Unicode standard:
      ;; (code-char I) returns NIL for reserved Unicode codepoints.
      (unless (hlmem::%codepoint-is-reserved i)
        (setf (char string i) (code-char i))))
    string))


(defun string-test ()
  (let ((x (make-rainbow-string))
        (index 0))
    (with-mem-words (ptr (msize index x) end-index)
      (mwrite-mread-test ptr index end-index x))))


(def-test mem-string (:compile-at :definition-time)
  (string-test))

