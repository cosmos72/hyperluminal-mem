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

  #+arm
  (:export #:fast-sap  #:sap=>fast-sap #:fast-sap=>sap
	   #:fast-mread/4 #:fast-mwrite/4 #:fast-mword/4=>fixnum)

  #+x86
  (:export #:fast-sap  #:sap=>fast-sap #:fast-sap=>sap
           #:fast-sap+ #:fast-sap<
	   #:fast-mread/4 #:fast-mwrite/4 #:fast-mword/4=>fixnum)

  #+x86-64
  (:export #:fast-sap  #:sap=>fast-sap #:fast-sap=>sap
           #:fast-sap+ #:fast-sap<
	   #:fast-mread/4 #:fast-mwrite/4 #:fast-mword/4=>fixnum
	   #:fast-mread/8 #:fast-mwrite/8 #:fast-mword/8=>fixnum))


