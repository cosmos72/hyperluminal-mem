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


;;;; * HYPERLUMINAL-ABCL

(in-package :cl-user)


(defpackage #:hyperluminal-mem-abcl
  (:nicknames #:hlm-abcl)
  (:use #:cl #:hyperluminal-mem-lang #:hyperluminal-mem-ffi)
  (:import-from #:stmx.lang
                #:eval-always  #:with-gensym  #:with-gensyms)

  (:export #:fast-memcpy/4 #:fast-memset/4
           #:fast-memcpy/8 #:fast-memset/8))
