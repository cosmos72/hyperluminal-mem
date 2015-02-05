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


;;;; * HYPERLUMINAL-SBCL

(in-package :cl-user)


(defpackage #:hyperluminal-sbcl
  (:nicknames #:hl-sbcl)
  (:use #:cl #:hyperluminal-lang #:hyperluminal-ffi)
  (:import-from #:stmx.lang
                #:eval-always  #:with-gensym  #:with-gensyms)

  (:export #:fast-sap #:sap=>fast-sap #:fast-sap=>sap #:+fixnum-zero-mask+1+
           #||#     #:fast-mread/4          #:fast-mwrite/4 #+x86    #:fast-mword/4=>fixnum
	   #+x86-64 #:fast-mread/8 #+x86-64 #:fast-mwrite/8 #+x86-64 #:fast-mword/8=>fixnum))


(defpackage #:hyperluminal-asm
  (:nicknames #:hl-asm)
  (:use #:hyperluminal-sbcl)
  (:export #:fast-sap #:sap=>fast-sap #:fast-sap=>sap #:+fixnum-zero-mask+1+
           #||#     #:fast-mread/4          #:fast-mwrite/4 ;;#+x86    #:fast-mword/4=>fixnum
	   #+x86-64 #:fast-mread/8 #+x86-64 #:fast-mwrite/8 ;;#+x86-64 #:fast-mword/8=>fixnum
           ))
