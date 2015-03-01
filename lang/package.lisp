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


;;;; * HYPERLUMINAL-MEM

(in-package :cl-user)

(defpackage #:hyperluminal-mem-lang

  (:nicknames #:hlm-lang)

  (:use #:cl)

  (:import-from #:stmx.lang
                #:with-gensyms #:when-bind)
  
  (:export #:eval-compile-constant #:check-compile-constant
	   #:or-func    #:and-func
	   #:stringify  #:concat-symbols
           #:get-symbol #:get-fbound-symbol #:have-symbol?
	   #:check-vector-index

	   #:fixnum*        #:fixnum+       #:fixnum-
           #:find-swap-bytes/n
           #:swap-bytes/2   #:swap-bytes/4  #:swap-bytes/8))
    
    

	   


