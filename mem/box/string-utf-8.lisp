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


(declaim (inline msize-box/string-utf-8))

(defun msize-box/string-utf-8 (index string)
  "Return the number of words needed to store STRING in memory, not including BOX header."
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type string string)
           (type mem-size index))

  (let ((n-bytes 0))
    (declare (type ufixnum n-bytes))

    (macrolet
        ((%count-utf-8-bytes ()
           `(loop for ch across string
               for code = (char-code ch)
               do
                 (incf (the fixnum n-bytes)

                       ;; support UTF-16 strings. Used at least by CMUCL and ABCL
                       #?+hlmem/character<=FFFF
                       (cond
                         ((<= code #x7F) 1)
                         ((<= code #x7FF) 2)
                         ((<= #xD800 code #xDBFF) 4) ;; high surrogate
                         ((<= #xDC00 code #xDFFF) 0) ;; low surrogate, space included above
                         (t 3))

                       ;; easier, strings contain Unicode codepoints
                       #?-hlmem/character<=FFFF
                       (cond
                         ((<= code #x7F) 1)
                         ((<= code #x7FF) 2)
                         ((<= code #xFFFF) 3)
                         (t 4))))))
      (cond
        ((typep string '(simple-array character)) (%count-utf-8-bytes))
        #?-hlmem/base-char<=ascii-char ;; if base-char<=ascii-char, we write an ASCII string
        ((typep string '(simple-array base-char)) (%count-utf-8-bytes))
        (t                                        (%count-utf-8-bytes))))

    ;; +1 to store N-CHARS prefix
    (mem-size+ index 1 (ceiling n-bytes +msizeof-word+))))






(declaim (inline %mwrite-string-utf-8))
(defun %mwrite-string-utf-8 (ptr index end-index string n-chars)
  "Write characters from string STRING to the memory starting at (PTR+INDEX).
Return the number of words actually written.

ABI: characters will be stored using UTF-8 encoding."
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type maddress ptr)
           (type mem-size index end-index)
           (type string string)
           (type ufixnum n-chars))

  (let ((save-index index)
        (i 0)
        #?+hlmem/character<=FFFF (n-codepoints 0)
        (word 0)
        (word-bits 0)
        (word-bits-left +mem-word/bits+))
    (declare (type mem-size save-index)
             (type fixnum i #?+hlmem/character<=FFFF n-codepoints)
             (type (integer 0 #.(1- +mem-word/bits+)) word-bits)
             (type (integer 1 #.+mem-word/bits+) word-bits-left))

    (incf-mem-size index) ;; we will store n-codepoints at save-index later
    
    (macrolet
        ((%mwrite-utf-8-words (char-func)
           `(loop while (< i n-chars)
               do
                 (let ((code (char-code (,char-func string i))))
                   (incf (the fixnum i))

                   ;; if strings are UTF-16, skip naked (and invalid) low surrogates
                   (when #?+hlmem/character<=FFFF (not (%code-is-low-surrogate code))
                         #?+hlmem/character<=FFFF t
                                                   
                     (multiple-value-bind (next next-bits)
                         (%codepoint->utf-8-word

                          ;; support UTF-16 strings.
                          #?+hlmem/character<=FFFF
                          (%utf-16->codepoint code string ,char-func i n-chars)

                          #?-hlmem/character<=FFFF
                          code)

                       (declare (type mem-word word next)
                                (type (integer 0 32) next-bits))

                       #?+hlmem/character<=FFFF
                       (incf n-codepoints)
                       
                       (setf word (logior word (logand +mem-word/mask+ (ash next word-bits)))
                             word-bits-left (- +mem-word/bits+ word-bits))
              
                       (when (>= next-bits word-bits-left)
                         (check-mem-overrun ptr index end-index 1)
                         (mset-word ptr index word)
                         (setf index     (mem-size+1 index)
                               word      (ash next (- word-bits-left))
                               word-bits (- next-bits word-bits-left)
                               next      0
                               next-bits 0))
                       
                       (incf word-bits next-bits)))))))

      (cond
        ((typep string '(simple-array character)) (%mwrite-utf-8-words schar))
        #?-hlmem/base-char<=ascii-char ;; if base-char<=ascii-char, we write an ASCII string
        ((typep string '(simple-array base-char)) (%mwrite-utf-8-words schar))
        (t                                        (%mwrite-utf-8-words  char))))

    (unless (zerop word-bits)
      (check-mem-overrun ptr index end-index 1)
      (mset-word ptr index word)
      (incf-mem-size index))

    (mset-int ptr save-index
              #?+hlmem/character<=FFFF n-codepoints
              #?-hlmem/character<=FFFF n-chars)

    index))



(defun mwrite-box/string-utf-8 (ptr index end-index string)
  "write STRING into the memory starting at (+ PTR INDEX).
Assumes BOX header is already written.

ABI: writes string length as mem-int, followed by packed array of UTF-8 encoded characters"
  (declare (type maddress ptr)
           (type mem-size index)
           (type string string))

  (let* ((n-chars (length string))
         (min-n-words (mem-size+1 (ceiling n-chars +msizeof-word+))))
    
    (check-mem-overrun ptr index end-index min-n-words)

    (%mwrite-string-utf-8 ptr index end-index string n-chars)))




(declaim (inline %mread-string-utf-8))
(defun %mread-string-utf-8 (ptr index end-index n-codepoints)
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type ufixnum n-codepoints))

  (let ((word 0)
        (word-bits 0)
        (word-bits-left 0)
        (next 0)
        (next-bits 0)
        (result
         #?+hlmem/character<=FFFF (make-array  n-codepoints :element-type 'character
                                               :adjustable t :fill-pointer 0)
         #?-hlmem/character<=FFFF (make-string n-codepoints :element-type 'character)))

    (declare (type mem-word word next)
             (type (integer 0 #.+mem-word/bits+) word-bits word-bits-left next-bits))

    (dotimes (i n-codepoints)
      (declare (ignorable i))
      
      (loop while (< word-bits 32) ;; UTF-8 needs at most 32 bits to encode a character
         do
           (when (and (zerop next-bits)
                      (< index end-index))
             (setf next (mget-word ptr index)
                   next-bits +mem-word/bits+)
             (incf-mem-size index))

           (setf word-bits-left (- +mem-word/bits+ word-bits)
                 word      (logior word (logand +mem-word/mask+ (ash next word-bits)))
                 word-bits (min +mem-word/bits+ (+ next-bits word-bits))
                 next      (ash next (- word-bits-left))
                 next-bits (max 0 (- next-bits word-bits-left)))

           (unless (< index end-index)
             (return)))

      (multiple-value-bind (code bits) (%utf-8-word->codepoint word)

        #?+hlmem/character<=FFFF
        (multiple-value-bind (ch1 ch2) (%codepoint->utf-16 code)
          (vector-push-extend ch1 result)
          (when ch2
            (vector-push-extend ch2 result)))

        #?-hlmem/character<=FFFF
        (setf (schar result i) (%codepoint->character code))
        
        (setf word      (ash word (- bits))
              word-bits (max 0 (- word-bits bits)))))
    
    (when (>= (+ word-bits next-bits) +mem-word/bits+)
      ;; problem... we read one word too much
      (decf-mem-size index))

    (values result index)))
  

(defun mread-box/string-utf-8 (ptr index end-index)
  "Read a string from the memory starting at (PTR+INDEX) and return it.
Also return as additional value INDEX pointing to immediately after words read.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (let* ((n-codepoints (mget-uint ptr index))
         (min-n-words (mem-size+1 (ceiling n-codepoints +msizeof-word+))))
    
    (check-array-length ptr index 'string n-codepoints)
    (check-mem-length ptr index end-index min-n-words)

    (%mread-string-utf-8 ptr (mem-size+1 index) end-index n-codepoints)))
