;; -*- lisp -*-

;; This file is part of hyperluminal-DB.
;; Copyright (c) 2013 Massimiliano Ghilardi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


(in-package :hyperluminal-mem)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              boxed BASE-STRING                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-#?-syntax)

(declaim (inline box-words/base-string))

(defun box-words/base-string (string index)
  "Return the number of words needed to store base-string STRING in mmap memory,
not including BOX header words."
  (declare (type base-string string)
           (type mem-size index))
  ;; 1-word length prefix, and round up required bytes to a whole word
  (mem-size+ index 1 (ceiling (length string) +msizeof-word+)))


(declaim (inline %mwrite-base-string))

(defun %mwrite-base-string (ptr index string n-chars)
  "Write the first N-CHARS single-byte characters of STRING into the memory starting at (PTR+INDEX). Return T."
  (declare (type maddress ptr)
           (type mem-size index)
           (type base-string string)
           (type ufixnum n-chars))

  (let ((offset (the mem-word (* index +msizeof-word+))))

    (macrolet ((loop-write (char-func ptr offset string n-chars)
                  (with-gensym i
                    `(loop for ,i from 0 below ,n-chars do
                          (%mset-t (the (unsigned-byte #.+mem-byte/bits+)
                                     (char-code
                                      (,char-func ,string ,i)))
                                   :byte ,ptr (the mem-word (+ ,offset ,i)))))))
    
      (if (typep string 'simple-string)
          (loop-write schar ptr offset string n-chars)
          (loop-write char ptr offset string n-chars))))
  
  t)



(defun mwrite-box/base-string (ptr index end-index string)
  "Write STRING into the memory starting at (+ PTR INDEX)
and return the number of words written. Assumes BOX header is already written.

ABI: writes characters count as mem-int, followed by array of characters each occupying one byte"
  (declare (type maddress ptr)
           (type mem-size index)
           (type base-string string))

  (let* ((n-chars (length string))
         (n-words (mem-size+1 (ceiling n-chars +msizeof-word+))))
    
    (check-mem-overrun ptr index end-index n-words)

    (mset-int ptr index n-chars)
    (%mwrite-base-string ptr (mem-size+1 index) string n-chars)

    (mem-size+ index n-words)))



(declaim (inline %mread-base-string))

(defun %mread-base-string (ptr index result-string n-chars)
  "Read (END-START) single-byte characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING. Return RESULT-STRING and number of words
actually read as multiple values.

ABI: characters are read from memory using the compact, single-byte representation.
For this reason only codes in the range 0 ... +most-positive-byte+ can be read
\(typically 0 ... 255)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type simple-base-string result-string)
           (type ufixnum n-chars))

  (let ((offset (the mem-word (* index +msizeof-word+))))

    (loop for i from 0 below n-chars do
         (setf (schar result-string i)
               (code-char
                (the (unsigned-byte #.+mem-byte/bits+)
                  (%mget-t :byte ptr (the mem-word (+ offset i)))))))))


(defun mread-box/base-string (ptr index end-index)
  "Read a boxed base-string from the memory starting at (PTR+INDEX) and return it.
Also return number of words actually read as addition value.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (let* ((n-chars (mget-int ptr index))
         (n-words (mem-size+1 (ceiling n-chars +msizeof-word+))))

    (check-array-length ptr index 'base-string n-chars)
    (check-mem-length ptr index end-index n-words)

    (let ((string (make-string n-chars :element-type 'base-char)))

      (%mread-base-string ptr (mem-size+1 index) string n-chars)

      (values string (mem-size+ index n-words)))))
      
