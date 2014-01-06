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



(defun base-string-words (string)
  "Return the number of words needed to store base-string STRING in memory."
  (declare (type base-string string))
  (truncate (+ (length string) +msizeof-word+ -1) ;; round up
            +msizeof-word+))


(defun box-words/base-string (string)
  "Return the number of words needed to store a BOX containing base-string STRING in memory."
  (declare (type base-string string))
  (mem-size+ +mem-box/header-words+ 1 (base-string-words string)))


(defun mwrite-base-string (ptr index string &optional (start 0) (end (length string)))
  "Write (END-START) single-byte characters from STRING into the memory starting at (PTR+INDEX).
Characters are written using the compact, single-byte representation.
For this reason the codes of all characters to be stored must be in the range
0 ... +most-positive-byte+ (typically 0 ... 255)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type base-string string)
           (type ufixnum start end))

  (let ((offset (the mem-word (* index +msizeof-word+))))

    (macrolet ((loop-write (char-func ptr offset string start end)
                 `(loop for i from ,start below ,end do
                       (%mset-t (the (unsigned-byte #.+mem-byte/bits+)
                                  (char-code
                                   (,char-func ,string i)))
                                :byte ,ptr (the mem-word (+ ,offset i))))))

      (if (typep string 'simple-string)
          (loop-write schar ptr offset string start end)
          (loop-write char ptr offset string start end)))))


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
    (mwrite-base-string ptr (mem-size+1 index) string 0 n-chars)
    t))


(defun mread-base-string (ptr index result-string &optional (start 0) (end (length result-string)))
  "Read (END-START) single-byte characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING.  Return RESULT-STRING.
Characters are read from memory using the compact, single-byte representation.
For this reason only codes in the range 0 ... +most-positive-byte+ can be read
\(typically 0 ... 255)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type simple-base-string result-string)
           (type ufixnum start end))

  (let ((offset (the mem-word (* index +msizeof-word+))))

    (loop for i from start below end do
         (setf (schar result-string i)
               (code-char
                (the (unsigned-byte #.+mem-byte/bits+)
                  (%mget-t :byte ptr (mem-size+ offset i)))))))
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

    (mread-base-string ptr (mem-size+1 index) string 0 n-chars)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed    STRING                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-words (string)
  "Return the number of words needed to store STRING in memory."
  (declare (type string string))
  (length string))


(defun box-words/string (string)
  "Return the number of words needed to store a BOX containing STRING in memory."
  (declare (type string string))
  (mem-size+ +mem-box/header-words+ 1 (string-words string)))


(defun mwrite-string (ptr index string &optional (start 0) (end (length string)))
  "Write (END-START) characters from string STRING to the memory starting at (PTR+INDEX).
Characters will be stored using the general-purpose representation.

TODO: pack as many characters as possible in each CPU word"
  (declare (type maddress ptr)
           (type mem-size index)
           (type string string)
           (type ufixnum start end))

  (if (typep string 'simple-string)
      (loop for i from start below end do
           (mset-character ptr (mem-size+ index i) (schar string i)))

      (loop for i from start below end do
           (mset-character ptr (mem-size+ index i) (char string i)))))


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
    (mwrite-string ptr (mem-size+1 index) string 0 n-chars)
    t))



(defun mread-string (ptr index result-string &optional (start 0) (end (length result-string)))
  "Read (END-START) characters from the memory starting at (PTR+INDEX)
and write them into RESULT-STRING. Return RESULT-STRING.
Characters will be read using the general-purpose representation."
  (declare (type maddress ptr)
           (type mem-size index)
           (type base-string result-string)
           (type ufixnum start end))

  (loop for i from start below end do
       (setf (schar result-string i)
             (mget-character ptr (mem-size+ index i))))
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

    (mread-string ptr (mem-size+1 index) string 0 n-chars)))



