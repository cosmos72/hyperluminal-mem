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

(def-suite memcpy-suite :in suite)
(in-suite memcpy-suite)

(defun set-words (ptr index end-index start-value)
  (declare (type maddress ptr)
	   (mem-size index end-index)
	   (mem-word start-value))
  (let ((value start-value))
    (declare (type mem-word value))
    (loop while (< index end-index) do
         (hlmem::mset-word ptr index value)
         (incf value)
         (incf index))))

(defun check-words (ptr index end-index start-value)
  (declare (type maddress ptr)
	   (mem-size index end-index)
	   (mem-word start-value))
  (let ((value start-value))
    (declare (type mem-word value))
    (loop
       while (< index end-index) do
         (let ((word (hlmem::mget-word ptr index)))
           (is (= word value)
               "word at ~S contains #x~X instead of #x~X" index word value)
           (incf value)
           (incf index)))))

(defun memcpy-test (&optional (n-words 16))
  (declare (type mem-size n-words))
  
  (with-mem-words (src n-words)
    (with-mem-words (dst n-words)

      (dotimes (algo #-abcl 3 #+abcl 2)
        (let* ((seed1 (the mem-word (logxor #x4321 (* algo #x0F0F))))
               (seed2 (the mem-word (logand hlmem::+most-positive-word+
                                            (ash (lognot seed1) -1)))))
          (set-words   src 0 n-words seed1)
          (check-words src 0 n-words seed1)
          (set-words   dst 0 n-words seed2)
          (check-words dst 0 n-words seed2)

          (case algo
            (0 (dotimes (i n-words)
                 (hlmem::mset-word dst i (hlmem::mget-word src i))))
            (1 (memcpy-words dst 0 src 0 n-words))
            #-abcl
            (2 (osicat-posix:memcpy dst src (the mem-word (* n-words +msizeof-word+)))))
          
          (check-words src 0 n-words seed1)
          (check-words dst 0 n-words seed1))))))


(def-test memcpy (:compile-at :definition-time)
  (loop for n-words in '(0 1 2 3 4 5 6 7 8 9 15 16 17 31 32 33 63 64 65) do
    (memcpy-test n-words)))
