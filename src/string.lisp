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


(in-package :hyperluminal-db)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              boxed BASE-STRING                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-#?-syntax)

(defun base-string-words (string)
  "Return the number of words needed to store base-string STRING in memory."
  (declare (type base-string string))
  (ceiling (length string) +msizeof-word+)) ;; round up


(defun box-words/base-string (string)
  "Return the number of words needed to store a BOX containing base-string STRING in memory."
  (declare (type base-string string))
  (mem-size+ +mem-box/header-words+ 1 (base-string-words string)))


(defun mwrite-base-string (ptr index string n-chars)
  "Write characters from STRING into the memory starting at (PTR+INDEX).
Characters are written using the compact, single-byte representation.
For this reason the codes of all characters to be stored must be in the range
0 ... +most-positive-byte+ (typically 0 ... 255)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type base-string string)
           (type ufixnum n-chars))

  (let ((offset (the mem-word (* index +msizeof-word+))))

    (macrolet ((loop-write (char-func ptr offset string n-chars)
                 `(loop for i from 0 below ,n-chars do
                       (%mset-t (the (unsigned-byte #.+mem-byte/bits+)
                                  (char-code
                                   (,char-func ,string i)))
                                :byte ,ptr (the mem-word (+ ,offset i))))))

      (if (typep string 'simple-string)
          (loop-write schar ptr offset string n-chars)
          (loop-write char ptr offset string n-chars)))))


(defun mwrite-box/base-string (ptr index n-words string)
  "Reuse the memory block starting at (+ PTR INDEX)
and write STRING into it.

ABI: string is stored as box prefix, followed by characters count and array of characters"
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type base-string string))

  (setf index
        (mwrite-box/header ptr index n-words +mem-box-base-string+))

  (let ((n-chars (length string)))
    
    (mset-int ptr index n-chars)
    (mwrite-base-string ptr (mem-size+1 index) string n-chars)
    t))


(defun mread-base-string (ptr index result-string n-chars)
  "Read (END-START) single-byte characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING.  Return RESULT-STRING.
Characters are read from memory using the compact, single-byte representation.
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
                  (%mget-t :byte ptr (the mem-word (+ offset i))))))))
  result-string)



(defun mread-box/base-string (ptr index)
  "Read a boxed base-string from the memory starting at (PTR+INDEX).
Return the base-string"
  (declare (type maddress ptr)
           (type mem-size index))
  
  ;; skip the box header
  (incf-mem-size index +mem-box/header-words+)

  (let* ((n-chars (mget-int ptr index))
         (string (make-string n-chars :element-type 'base-char)))

    (mread-base-string ptr (mem-size+1 index) string n-chars)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed    STRING                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-words (string)
  "Return the number of words needed to store STRING in memory."
  (declare (type string string))
  (ceiling (length string) +characters-per-word+)) ;; round up


(defun box-words/string (string)
  "Return the number of words needed to store a BOX containing STRING in memory."
  (declare (type string string))
  (mem-size+ +mem-box/header-words+ 1 (string-words string)))


(defmacro %bulk-pack-string (char-func string pos)
  `(logior
    ,@(loop for i from 0 below +characters-per-word+
         collect `(the mem-word
                    (ash (char-code (,char-func ,string (+ ,i ,pos)))
                         (* ,i +character/bits+))))))


(defmacro %tail-pack-string (char-func string pos n-chars)
  (with-gensyms (word i)
    `(let ((,word 0))
       (declare (type mem-word ,word))

       (dotimes (,i ,n-chars)
         (setf ,word (logior ,word
                             (the mem-word
                               (ash (char-code (the character (,char-func ,string (+ ,i ,pos))))
                                    (the fixnum (* ,i +character/bits+)))))))
       ,word)))

(defmacro %mwrite-string (char-func ptr index string bulk-n-words tail-n-chars)
  (with-gensyms (char-i word-i)
    `(loop
        for ,char-i from 0 by +characters-per-word+
        for ,word-i from 0 below ,bulk-n-words
        do (mset-word ,ptr (mem-size+ ,index ,word-i)
                      (%bulk-pack-string ,char-func ,string ,char-i))
        finally
          (unless (zerop ,tail-n-chars)
            (mset-word ,ptr (mem-size+ ,index ,bulk-n-words)
                       (%tail-pack-string ,char-func ,string ,char-i ,tail-n-chars))))))


(defun mwrite-string (ptr index string n-chars)
  "Write characters from string STRING to the memory starting at (PTR+INDEX).
Characters will be stored by packing as many as possible into words."
  (declare (type maddress ptr)
           (type mem-size index)
           (type string string)
           (type ufixnum n-chars))

  (multiple-value-bind (bulk-n-words tail-n-chars) (truncate n-chars +characters-per-word+)

      (typecase string
        (simple-base-string
         (%mwrite-string schar ptr index string bulk-n-words tail-n-chars))
        (simple-string
         (%mwrite-string schar ptr index string bulk-n-words tail-n-chars))
        (otherwise
         (%mwrite-string char ptr index string bulk-n-words tail-n-chars))))
  t)




(defun mwrite-box/string (ptr index n-words string)
  "Reuse the memory block starting at (+ PTR INDEX)
and write STRING into it.

ABI: string is stored as box prefix, followed by characters count and array of characters"
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type string string))

  (setf index
        (mwrite-box/header ptr index n-words +mem-box-string+))

  (let ((n-chars (length string)))
    
    (mset-int ptr index n-chars)
    (mwrite-string ptr (mem-size+1 index) string n-chars)
    t))



(defmacro %bulk-unpack-string (word string pos)
  "Unpack characters from WORD and stores them in STRING.
Note: increments POS!"
  `(progn
     ,@(loop for i from 0 below +characters-per-word+
          collect `(setf (schar ,string ,pos) (code-char (logand +character/mask+ ,word))
                         ,@(unless (= (1+ i)  +characters-per-word+)
                                   `(word (the mem-word (ash ,word #.(- +character/bits+)))))
                         ,pos  (the ufixnum (1+ ,pos))))))


(defun mread-string (ptr index result-string n-chars)
  "Read characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING. Return RESULT-STRING."
  (declare (type maddress ptr)
           (type mem-size index)
           (type (and simple-string #?-hldb/base-char/eql/character (not base-string)) result-string)
           (type ufixnum n-chars))

  (multiple-value-bind (bulk-n-words tail-n-chars) (truncate n-chars +characters-per-word+)

    (let ((char-i 0) ;; incremented by (%bulk-unpack-string)
          (bulk-end (mem-size+ index bulk-n-words)))

      (declare (type ufixnum char-i)
               (type mem-size bulk-end))

      (loop while (< index bulk-end)
         do
           (let ((word (mget-word ptr index)))
             (%bulk-unpack-string word result-string char-i) ;; increments char-i
             (incf (the mem-size index))))

      (unless (zerop tail-n-chars)
        (let ((word (mget-word ptr bulk-end)))
          (loop while (< char-i n-chars)
             do (setf (schar result-string char-i) (code-char (logand +character/mask+ word))
                      word   (the mem-word (ash word #.(- +character/bits+)))
                      char-i (the fixnum (1+ char-i))))))))

  result-string)



(defun mread-box/string (ptr index)
  "Read a boxed string from the memory starting at (PTR+INDEX).
Return the string"
  (declare (type maddress ptr)
           (type mem-size index))
  
  ;; skip the box header
  (incf-mem-size index +mem-box/header-words+)

  (let* ((n-chars (mget-int ptr index))
         (string (make-string n-chars)))

    (mread-string ptr (mem-size+1 index) string n-chars)))



