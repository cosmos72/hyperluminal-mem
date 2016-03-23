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


;;;; * HYPERLUMINAL-MEM-TREE

(in-package :cl-user)

(stmx.lang:enable-#?-syntax)

(defpackage #:hyperluminal-mem-tree

  (:nicknames #:hlm-tree)

  (:use #:cl)

  (:import-from #:stmx.lang

                #:eval-always  #:enable-#?-syntax #:get-feature
                #:set-feature  #:set-features

                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen    #:aif)

  (:import-from #:stmx.util
                
                #:fixnum< #:fixnum> #:fixnum=)

                
  (:export #:b+tree))
