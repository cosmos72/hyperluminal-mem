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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed bit-vector                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-words/bit-vector (vector index)
  "Return the number of words needed to store bit-vector VALUE in mmap memory,
not including BOX header."
  (declare (type bit-vector vector)
           (type mem-size index))
  ;; 1-word length prefix, and round up required bytes to a whole word
  (mem-size+ index 1 (ceiling (length vector) +mem-word/bits+)))
  

(defmacro %bit-vector-extract-byte (vector position)
  "Warning: evaluates VECTOR multiple times"
  (with-gensyms (pos)
    `(let ((,pos ,position))
       (logior
        ,@(loop for i from 0 below +mem-byte/bits+
             collect `(ash (aref ,vector (the ufixnum (+ ,pos ,i))) ,i))))))


(defmacro %bit-vector-insert-byte (vector position byte)
  "Warning: evaluates VECTOR multiple times. updates BYTE"
  (with-gensyms (pos)
    `(let ((,pos ,position))
       ,@(loop for i from 0 below +mem-byte/bits+
            collect `(setf (aref ,vector (the ufixnum (+ ,pos ,i))) (logand ,byte 1)
                           ,byte (the mem-word (ash ,byte -1)))))))
                            


(defmacro %bit-vector-extract-word (vector position)
  "Warning: evaluates VECTOR multiple times"
  (with-gensyms (start end i word)
    `(let* ((,start (the ufixnum ,position))
            (,end (the ufixnum (+ ,start +mem-word/bits+)))
            (,word 0))
       (declare (type mem-word ,word))
       (loop for ,i from ,start below ,end by +mem-byte/bits+ do
            (setf ,word (logior ,word (the mem-word
                                        (ash (%bit-vector-extract-byte ,vector ,i) ,i)))))
       ,word)))


(defmacro %bit-vector-insert-word (vector position word)
  "Warning: evaluates VECTOR multiple times."
  (with-gensyms (start end w i)
    `(let* ((,start (the ufixnum ,position))
            (,end (the ufixnum (+ ,start +mem-word/bits+)))
            (,w ,word))
       (declare (type mem-word ,w))

       (loop for ,i from ,start below ,end by +mem-byte/bits+ do
            (%bit-vector-insert-byte ,vector ,i ,w)))))




(defmacro %mwrite-bit-vector (ptr index vector n-bits)
  "Warning: evaluates PTR, INDEX and VECTOR multiple times. Updates INDEX."
  ;; split N-BITS into a whole number of words and a remainder
  ;; works also if +msizeof-word+ is not a power of two.
  (with-gensyms (n end tail i word bit)
    `(let* ((,n    (the ufixnum ,n-bits))
            (,tail (the ufixnum (mod ,n +mem-word/bits+)))
            (,end  (the ufixnum (- ,n ,tail))))
       (loop for ,i from 0 below ,end by +mem-word/bits+
          do
            (mset-word ,ptr (incf-mem-size ,index)
                       (%bit-vector-extract-word ,vector ,i)))
       (unless (zerop ,tail)
         (let ((,word 0))
           (declare (type mem-word ,word))
           (loop for ,i from 0 below ,tail
              for ,bit = (aref ,vector (the ufixnum (+ ,i ,end)))
              do
                (setf ,word (logior ,word (the mem-word (ash ,bit ,i)))))
           (mset-word ,ptr (incf-mem-size ,index) ,word))))))


(defun %mread-bit-vector (ptr index vector n-bits)
  (declare (type maddress ptr)
           (type mem-size index)
           (type simple-bit-vector vector)
           (type ufixnum n-bits))

  ;; split N-BITS into a whole number of words and a remainder
  ;; works also if +msizeof-word+ is not a power of two.
  (let* ((tail (the ufixnum (mod n-bits +mem-word/bits+)))
         (end  (the ufixnum (- n-bits tail))))
    (loop for i from 0 below end by +mem-word/bits+
       do
         (let ((word (mget-word ptr (incf-mem-size index))))
           (%bit-vector-insert-word vector i word)))
    (unless (zerop tail)
      (let ((word (mget-word ptr (incf-mem-size index))))
        (loop for i from 0 below tail
           for bit = (logand word 1)
           do
             (setf (aref vector (the ufixnum (+ i end))) bit
                   word (the mem-word (ash word -1))))))
    (mem-size+1 index)))



(defun mwrite-box/bit-vector (ptr index end-index vector)
  "Write bit-vector VECTOR into the memory starting at (+ PTR INDEX)
and return the number of words written. Assumes BOX header is already written.

ABI: writes element count as mem-int, followed by sequence of bits"
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type bit-vector vector))

  (let* ((n-bits (length vector))
         (n-words (mem-size+1 (ceiling n-bits +mem-word/bits+))))
    
    (check-mem-overrun ptr index end-index n-words)

    (mset-int ptr index n-bits)

    (if (typep vector 'simple-bit-vector)
        (%mwrite-bit-vector ptr index vector n-bits)
        (%mwrite-bit-vector ptr index vector n-bits))

    (mem-size+1 index)))



(defun mread-box/bit-vector (ptr index end-index)
  "Read a bit-vector from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (let* ((n-bits (mget-int ptr index))
         (n-words (mem-size+1 (ceiling n-bits +mem-word/bits+))))

    (check-array-length ptr index 'bit-vector n-bits)
    (check-mem-length ptr index end-index n-words)

    (let ((vector (make-array n-bits :element-type 'bit)))
      (values
       vector
       (%mread-bit-vector ptr index vector n-bits)))))
