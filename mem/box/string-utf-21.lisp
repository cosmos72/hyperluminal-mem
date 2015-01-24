;; -*- lisp -*-

;; This file is part of Hyperluminal-MEM.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; boxed STRING alternative implementation. Stores 21-bit Unicode as-is    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-#?-syntax)


(defun box-words/string-utf-21 (string index)
  "Return the number of words needed to store STRING in memory, not including BOX header."
  (declare (type string string)
           (type mem-size index))
  ;; 1-word length prefix, and round up required bytes to a whole word
  (mem-size+ index 1 (ceiling (length string) +characters-per-word+)))



(defmacro %bulk-pack-string-utf-21 (char-func string pos)
  `(logior
    ,@(loop for i from 0 below +characters-per-word+
         collect `(the mem-word
                    (ash (char-code (,char-func ,string (+ ,i ,pos)))
                         (* ,i +character/bits+))))))


(defmacro %tail-pack-string-utf-21 (char-func string pos n-chars)
  (with-gensyms (word i)
    `(let ((,word 0))
       (declare (type mem-word ,word))

       (dotimes (,i ,n-chars)
         (setf ,word (logior ,word
                             (the mem-word
                               (ash (char-code (the character (,char-func ,string (+ ,i ,pos))))
                                    (the ufixnum (* ,i +character/bits+)))))))
       ,word)))

(defmacro %%mwrite-string-utf-21 (char-func ptr index string bulk-n-words tail-n-chars)
  (with-gensyms (char-i bulk-n-chars)
    `(let ((,bulk-n-chars (* ,bulk-n-words +characters-per-word+)))
       (loop
          for ,char-i from 0 below ,bulk-n-chars by +characters-per-word+ do
            (mset-word ,ptr ,index
                       (%bulk-pack-string-utf-21 ,char-func ,string ,char-i))
            (incf-mem-size ,index)
            
          finally
            (unless (zerop ,tail-n-chars)
              (mset-word ,ptr ,index
                         (%tail-pack-string-utf-21 ,char-func ,string ,char-i ,tail-n-chars))
              (incf-mem-size ,index))
            (return ,index)))))


(defun %mwrite-string-utf-21 (ptr index string n-chars)
  "Write characters from string STRING to the memory starting at (PTR+INDEX).
Return the number of words actually written.

ABI: characters will be stored by packing as many as possible into words."
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type maddress ptr)
           (type mem-size index)
           (type string string)
           (type ufixnum n-chars))

  (multiple-value-bind (bulk-n-words tail-n-chars) (truncate n-chars +characters-per-word+)

      (typecase string
        (simple-base-string
         (%%mwrite-string-utf-21 schar ptr index string bulk-n-words tail-n-chars))
        (simple-string
         (%%mwrite-string-utf-21 schar ptr index string bulk-n-words tail-n-chars))
        (otherwise
         (%%mwrite-string-utf-21 char ptr index string bulk-n-words tail-n-chars)))))




(defun mwrite-box/string-utf-21 (ptr index end-index string)
  "write STRING into the memory starting at (+ PTR INDEX).
Assumes BOX header is already written.

ABI: writes string length as mem-int, followed by packed array of characters
\(each character occupies 21 bits)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type string string))

  (let* ((n-chars (length string))
         (n-words (mem-size+1 (ceiling n-chars +characters-per-word+))))
    
    (check-mem-overrun ptr index end-index n-words)

    (mset-int ptr index n-chars)
    (%mwrite-string-utf-21 ptr (mem-size+1 index) string n-chars)))



(defmacro %bulk-unpack-string-utf-21 (word string pos)
  "Unpack characters from WORD and stores them in STRING.
Note: increments POS!"
  `(progn
     ,@(loop for i from 0 below +characters-per-word+
          collect `(setf (schar ,string ,pos) (code-char (logand +character/mask+ ,word))
                         ,@(unless (= (1+ i)  +characters-per-word+)
                                   `(word (the mem-word (ash ,word #.(- +character/bits+)))))
                         ,pos  (the ufixnum (1+ ,pos))))))


(defun %mread-string-utf-21 (ptr index n-chars)
  "Read N-CHAR packed characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING.
Return RESULT and number of words actually read as multiple values."
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type maddress ptr)
           (type mem-size index)
           (type ufixnum n-chars))

  (multiple-value-bind (bulk-n-words tail-n-chars) (truncate n-chars +characters-per-word+)

    (let ((char-i 0) ;; incremented by (%bulk-unpack-string-utf-21)
          (bulk-end (mem-size+ index bulk-n-words))
          (result (make-string n-chars :element-type 'character)))

      (declare (type ufixnum char-i)
               (type mem-size bulk-end))

      (loop while (< index bulk-end)
         do
           (let ((word (mget-word ptr index)))
             (%bulk-unpack-string-utf-21 word result char-i) ;; increments char-i
             (incf-mem-size index)))

      (unless (zerop tail-n-chars)
        (let ((word (mget-word ptr bulk-end)))
          (loop while (< char-i n-chars)
             do (setf (schar result char-i) (code-char (logand +character/mask+ word))
                      word   (the mem-word (ash word #.(- +character/bits+)))
                      char-i (the ufixnum (1+ char-i))))
          (incf-mem-size index)))

      (values result index))))



(defun mread-box/string-utf-21 (ptr index end-index)
  "Read a string from the memory starting at (PTR+INDEX) and return it.
Also return number of words actually read as addition value.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (let* ((n-chars (mget-int/value ptr index))
         (n-words (mem-size+1 (ceiling n-chars +characters-per-word+))))
    
    (check-array-length ptr index 'string n-chars)
    (check-mem-length ptr index end-index n-words)

    (%mread-string-utf-21 ptr (mem-size+1 index) n-chars)))
