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


(in-package :hyperluminal-mem-lang)

;; support inverted endianity.

;; clisp 2.49 has too old asdf, cannot load swap-bytes
#+clisp
(progn
  (declaim (inline swap-bytes/2))
  (defun swap-bytes/2 (n)
    (declare (type (unsigned-byte 16) n))
    (logior (ash (logand #x00FF n) 8)
            (ash                n -8)))

  (declaim (inline swap-bytes/4))
  (defun swap-bytes/4 (n)
    (declare (type (unsigned-byte 32) n))
    #+#.(cl:if (cl:> (cl:integer-length cl:most-positive-fixnum) 32) '(:and) '(:or))
    (logior (ash (logand #x000000FF n) 24)
            (ash (logand #x0000FF00 n) 8)
            (ash (logand #x00FF0000 n) -8)
            (ash                    n -24))
    #-#.(cl:if (cl:> (cl:integer-length cl:most-positive-fixnum) 32) '(:and) '(:or))
    (logior (swap-bytes/2 (ash n -16))
            (ash (swap-bytes/2 (logand n #xFFFF)) 16)))
            
  (declaim (inline swap-bytes/8))
  (defun swap-bytes/8 (n)
    (declare (type (unsigned-byte 64) n))
    (logior (swap-bytes/4 (ash n -32))
            (ash (swap-bytes/4 (logand n #xFFFFFFFF)) 32))))
            

            
;; ABCL has built-in support in java.nio.ByteBuffer
#-(or abcl clisp)
(progn
  (declaim (inline swap-bytes/2))
  (defun swap-bytes/2 (n)
    (swap-bytes:swap-bytes-16 n))

  (declaim (inline swap-bytes/4))
  (defun swap-bytes/4 (n)
    (swap-bytes:swap-bytes-32 n))

  (declaim (inline swap-bytes/8))
  (defun swap-bytes/8 (n)
    (swap-bytes:swap-bytes-64 n)))


#-abcl
(defun find-swap-bytes/n (size)
  (ecase size
    (1 'identity)
    (2 'swap-bytes/2)
    (4 'swap-bytes/4)
    (8 'swap-bytes/8)))
    
    
      
