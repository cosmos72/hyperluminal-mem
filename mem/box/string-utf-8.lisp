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

(declaim (inline box-words/string-utf-8))

(defun box-words/string-utf-8 (index string)
  "Return the number of words needed to store STRING in memory, not including BOX header."
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type string string)
           (type mem-size index))

  (let ((n-bytes 0))
    (declare (type ufixnum n-bytes))

    (macrolet
        ((%count-utf-8-bytes ()
           `(loop for ch across string
               for code = (char-code ch) do
                 (incf (the fixnum n-bytes)
                       (cond
                         ((<= code #x7F) 1)
                         ((<= code #x7FF) 2)
                         ((<= code #xFFFF) 3)
                         (t 4))))))
      (if (typep string 'simple-string)
          (%count-utf-8-bytes)
          (%count-utf-8-bytes)))

    ;; +1 to store N-CHARS prefix
    (mem-size+ index 1 (ceiling n-bytes +msizeof-word+))))


(declaim (inline %unsigned->utf-8-word %utf-8-word->unsigned))

(declaim (inline %character->utf-8-word %utf-8-word->character %mwrite-string))


(defun %unsigned->utf-8-word (n)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type (unsigned-byte #.+character/bits+) n))

  (let ((word 0)
        (bits 0))
    (cond
      ((<= n #x7F) (setf word n
                         bits 8))
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


(declaim (inline %character->utf-8-word))

(defun %character->utf-8-word (ch)
  (declare (type character ch))

  (%unsigned->utf-8-word (char-code ch)))


(defun invalid-utf8-error (byte)
  (declare (type (unsigned-byte 8) byte))
  (error "invalid byte. UTF-8 sequence cannot start with #x~X" byte))


(defun %utf-8-word->unsigned (word)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type mem-word word))

  (let ((n 0)
        (bits 0)
        (byte0 (logand #xFF word)))

    (cond
      ((<= byte0 #x7F) (setf n byte0
                             bits 8))
      
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

    (values n bits)))
                   

(declaim (inline %utf-8-word->character))

(defun %utf-8-word->character (word)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type mem-word word))

  (multiple-value-bind (n bits) (%utf-8-word->unsigned word)
    (values (code-char (logand n +character/mask+)) bits)))




(defun %mwrite-string-utf-8 (ptr index end-index string n-chars)
  "Write characters from string STRING to the memory starting at (PTR+INDEX).
Return the number of words actually written.

ABI: characters will be stored using UTF-8 encoding."
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type maddress ptr)
           (type mem-size index end-index)
           (type string string)
           (type ufixnum n-chars))

  (let ((word 0)
        (word-bits 0)
        (word-bits-left +mem-word/bits+))
    (declare (type (integer 0 #.(1- +mem-word/bits+)) word-bits)
             (type (integer 1 #.+mem-word/bits+) word-bits-left))

    (macrolet
        ((%mwrite-utf-8-words (char-func)
           `(dotimes (i n-chars)
              (let ((ch (,char-func string i)))
                (multiple-value-bind (next next-bits) (%character->utf-8-word ch)

                  (declare (type mem-word word next)
                           (type (integer 0 32) next-bits))

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

                  (incf word-bits next-bits))))))

      (if (typep string 'simple-string)
          (%mwrite-utf-8-words schar)
          (%mwrite-utf-8-words  char)))

    (unless (zerop word-bits)
      (check-mem-overrun ptr index end-index 1)
      (mset-word ptr index word)
      (incf-mem-size index)))
  
  index)



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

    (mset-int ptr index n-chars)
    (%mwrite-string-utf-8 ptr (mem-size+1 index) end-index string n-chars)))





(defun %mread-string-utf-8 (ptr index end-index result n-chars)
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type (and simple-string #?-hlmem/base-char/eql/character (not base-string))
                 result)
           (type ufixnum n-chars))

  (let ((word 0)
        (word-bits 0)
        (word-bits-left 0)
        (next 0)
        (next-bits 0))
    (declare (type mem-word word next)
             (type (integer 0 #.+mem-word/bits+) word-bits word-bits-left next-bits))

    (dotimes (i n-chars)
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

      (multiple-value-bind (ch ch-bits) (%utf-8-word->character word)
              
        (setf (schar result i) ch
              word      (ash word (- ch-bits))
              word-bits (max 0 (- word-bits ch-bits)))))

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
  
  (let* ((n-chars (mget-uint ptr index))
         (min-n-words (mem-size+1 (ceiling n-chars +msizeof-word+))))
    
    (check-array-length ptr index 'string n-chars)
    (check-mem-length ptr index end-index min-n-words)

    (let ((result (make-string n-chars :element-type 'character)))

      (%mread-string-utf-8 ptr (mem-size+1 index) end-index result n-chars))))
