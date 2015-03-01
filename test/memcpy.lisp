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

(defun check-words (ptr index end-index expected-word)
  (declare (type maddress ptr)
	   (mem-size index end-index)
	   (mem-word expected-word))
  (loop while (< index end-index) do
       (let ((word (hlmem::mget-word ptr index)))
	 (is (= word expected-word)
	     (format nil "word at ~S contains #x~X instead of #x~X"
		     index word expected-word))
	 (incf index))))

(defun memcpy-test (&optional (n-words 16))
  (declare (type mem-size n-words))
  
  (with-mem-words (src n-words)
    (with-mem-words (dst n-words)
      (memset-words src 0 n-words #x1234)
      (check-words  src 0 n-words #x1234)

      (dotimes (algo #-abcl 3 #+abcl 2)
	(memset-words dst 0 n-words algo)
	(check-words  dst 0 n-words algo)

	(case algo
	  (0 (loop for i from 1 below (1- n-words)
		do (hlmem::mset-word dst i (hlmem::mget-word src i))))
	  (1 (memcpy-words dst 1 src 0 (- n-words 2)))
	  #-abcl
	  (2 (osicat-posix:memcpy (cffi-sys:inc-pointer dst +msizeof-word+)
				  src (* (- n-words 2) +msizeof-word+))))
	(check-words dst 0            1            algo)
	(check-words dst 1            (1- n-words) #x1234)
	(check-words dst (1- n-words) n-words      algo)))))


(def-test memcpy (:compile-at :definition-time)
  (memcpy-test ))
