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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              boxed BASE-STRING                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-#?-syntax)

(deftype ascii-string ()
  #?+hlmem/base-char>=ascii 'base-string
  #?-hlmem/base-char>=ascii 'string)


(deftype ascii-char ()
  #?+hlmem/base-char>=ascii 'base-char
  #?-hlmem/base-char>=ascii 'character)


(declaim (inline box-words/ascii-string))

(defun box-words/ascii-string (index string)
  "Return the number of words needed to store ascii-string STRING in mmap memory,
not including BOX header words."
  (declare (type ascii-string string)
           (type mem-size index))
  ;; 1-word length prefix, and round up required bytes to a whole word
  (mem-size+ index 1 (ceiling (length string) +msizeof-word+)))


(declaim (inline %mwrite-ascii-string))

(defun %mwrite-ascii-string (ptr index string n-chars)
  "Write the first N-CHARS single-byte characters of STRING into the memory starting at (PTR+INDEX). Return T."
  (declare (optimize (speed 3) (debug 1) (safety 0))

           (type maddress ptr)
           (type mem-size index)
           (type ascii-string string)
           (type ufixnum n-chars))
 
  (let* ((n-chars-remainder (nth-value 1 (truncate n-chars +msizeof-word+)))
         (n-chars-truncate  (- n-chars n-chars-remainder)))
    (declare (type ufixnum n-chars-remainder n-chars-truncate))

    (macrolet ((ascii-char-to-word (char-func i)
                 `(the mem-byte
                       (char-code (,char-func string (the fixnum ,i)))))
               
               (ascii-chars-to-word (char-func i)
                 `(logior
                   ,@(loop for j below +msizeof-word+ collect
                          `(the mem-word
                                (ash (ascii-char-to-word ,char-func (+ ,i ,j))
                                     ,(* j +mem-byte/bits+))))))

               (loop-write (char-func)
                  (with-gensyms (i word)
                    `(progn
                       (let ((, i 0))
                         (declare (type ufixnum ,i))
                         (loop while (< ,i n-chars-truncate)
                            do
                              (let ((,word (ascii-chars-to-word ,char-func ,i)))
                                (declare (type mem-word ,word))
                                (mset-word ptr index ,word)
                                (incf-mem-size index)
                                (incf ,i +msizeof-word+))))
                       
                       (unless (zerop n-chars-remainder)
                         (let ((,word 0))
                           (declare (type mem-word ,word))
                           (loop for ,i from 0 below n-chars-remainder do
                                (setf
                                 ,word
                                 (logior ,word
                                         (the mem-word
                                              (ash (ascii-char-to-word
                                                    ,char-func (+ ,i n-chars-truncate))
                                                   (* ,i +mem-byte/bits+))))))
                           (mset-word ptr index ,word)))))))
        
      (if (typep string 'simple-string)
          (loop-write schar)
          (loop-write  char))))
  t)



(defun mwrite-box/ascii-string (ptr index end-index string)
  "Write STRING into the memory starting at (+ PTR INDEX)
and return the number of words written. Assumes BOX header is already written.

ABI: writes characters count as mem-int, followed by array of characters each occupying one byte"
  (declare (type maddress ptr)
           (type mem-size index)
           (type ascii-string string))

  (let* ((n-chars (length string))
         (n-words (mem-size+1 (ceiling n-chars +msizeof-word+))))
    
    (check-mem-overrun ptr index end-index n-words)

    (mset-int ptr index n-chars)
    (%mwrite-ascii-string ptr (mem-size+1 index) string n-chars)

    (mem-size+ index n-words)))



(declaim (inline %mread-ascii-string))

(defun %mread-ascii-string (ptr index n-chars)
  "Read (END-START) single-byte characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING. Return RESULT-STRING and number of words
actually read as multiple values.

ABI: characters are read from memory using the compact, single-byte representation.
For this reason only codes in the range 0 ... +most-positive-byte+ can be read
\(typically 0 ... 255)"
  (declare (optimize (speed 3) (debug 1) (safety 0))

           (type maddress ptr)
           (type mem-size index)
           (type ufixnum n-chars))

  (let* ((n-chars-remainder (nth-value 1 (truncate n-chars +msizeof-word+)))
         (n-chars-truncate  (- n-chars n-chars-remainder))
         (result (make-string n-chars :element-type
                              #?+hlmem/base-char>=ascii 'base-char
                              #?-hlmem/base-char>=ascii 'character)))
    
    (declare (type ufixnum n-chars-remainder n-chars-truncate))

    (macrolet ((word-to-ascii-char (word)
                 `(the ascii-char
                       (code-char (logand ,word +ascii-char/mask+))))
               
               (word-to-ascii-chars (word char-func i)
                 `(progn
                   ,@(loop for j below +msizeof-word+ collect
                          `(setf (,char-func result (+ ,i ,j))
                                 (word-to-ascii-char
                                  (ash ,word (- (* ,j +mem-byte/bits+))))))))


               (loop-read (char-func)
                  (with-gensyms (i word)
                    `(progn
                       (let ((, i 0))
                         (declare (type ufixnum ,i))
                         (loop while (< ,i n-chars-truncate)
                            do
                              (let ((,word (mget-word ptr index)))
                                (declare (type mem-word ,word))
                                (incf-mem-size index)
                                (word-to-ascii-chars ,word ,char-func ,i)
                                (incf ,i +msizeof-word+))))

                       (unless (zerop n-chars-remainder)
                         (let ((,word (mget-word ptr index)))
                           (declare (type mem-word ,word))
                           (loop for ,i from 0 below n-chars-remainder do
                                (setf (,char-func result (+ ,i n-chars-truncate))
                                      (word-to-ascii-char ,word))
                                (setf ,word (ash ,word (- +mem-byte/bits+))))))))))
      (loop-read schar))

    result))





      


(defun mread-box/ascii-string (ptr index end-index)
  "Read a boxed ascii-string from the memory starting at (PTR+INDEX) and return it.
Also return number of words actually read as addition value.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (let* ((n-chars (mget-int ptr index))
         (n-words (mem-size+1 (ceiling n-chars +msizeof-word+))))

    (check-array-length ptr index 'ascii-string n-chars)
    (check-mem-length ptr index end-index n-words)

    (values (%mread-ascii-string ptr (mem-size+1 index) n-chars)
            (mem-size+ index n-words))))


      
