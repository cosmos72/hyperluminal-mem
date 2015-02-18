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


(in-package :hyperluminal-mem.test)

(defun memcpy-test (n-words &optional (n-loops (truncate (ash 1 30) n-words)))
  (declare (type mem-size n-words)
           (type fixnum n-loops))
  
  (with-mem-words (src n-words)
    (with-mem-words (dst n-words)
      (memset-words src 0 n-words #x1234)
      (memset-words dst 0 n-words #x4321)
      (time
       (dotimes (i n-loops)
         (osicat-posix:memcpy dst src (* n-words +msizeof-word+))))
      (time
       (dotimes (i n-loops)
         (memcpy-words dst 0 src 0 n-words))))))
