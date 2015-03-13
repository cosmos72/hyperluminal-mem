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

(defpackage #:hyperluminal-mem-ffi

  (:nicknames #:hlm-ffi)

  (:use #:cl)

  (:import-from #:stmx.lang

                #:eval-always  #:enable-#?-syntax

                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen    #:aif )

  (:export #:ffi-mem-get     #:ffi-mem-set
           #:ffi-mem-alloc   #:ffi-mem-free    #:ffi-endianity
           #:with-ffi-mem    #:with-vector-mem
           #:ffi-sizeof      #:ffi-native-type-name
           #:+null-pointer+  #:null-pointer?     #:ffi-address

           #:ffi-defstruct
           
           #:os-getpagesize #:fd #:+bad-fd+ #:bad-fd?
           #:os-open-fd     #:os-close-fd   #:os-stat-fd-size #:os-truncate-fd
           #:os-mmap-fd     #:os-munmap-ptr #:os-msync-ptr))

