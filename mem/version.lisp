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


(define-constant-once +hlmem-version+ '(0 5 2))

(define-constant-once +hlmem-abi-version+ '(0 1 0))


(defun hlmem-version ()
  "Return HYPERLUMINAL-MEM version, in the form '(major minor patch)
as for example '(0 4 0)"
  +hlmem-version+)


(defun hlmem-abi-version ()
  "Return HYPERLUMINAL-MEM ABI version, in the form '(major minor patch)
as for example '(0 1 0)"
  +hlmem-abi-version+)
