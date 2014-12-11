;; -*- lisp -*-

;; This file is part of hyperluminal-DB.
;; Copyright (c) 2013 Massimiliano Ghilardi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;;;; * HYPERLUMINAL-DB

(in-package :cl-user)

(defpackage #:hyperluminal-ffi

  (:nicknames #:hlffi)

  (:use #:cl)

  (:import-from #:stmx.lang

                #:eval-always  #:enable-#?-syntax

                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen    #:aif 
                #:log.debug    #:log.trace     #:log.make-logger)

  (:export #:ffi-mem-get     #:ffi-mem-set
           #:with-ffi-mem    #+sbcl #:with-simple-array-mem
           #:ffi-sizeof      #:ffi-native-type-name
           #:+null-pointer+  #:null-pointer?     #:maddress

           #:os-getpagesize #:fd #:+bad-fd+ #:bad-fd?
           #:os-open-fd-rw  #:os-close-fd   #:os-stat-fd-size #:os-truncate-fd
           #:os-mmap-fd-rw  #:os-munmap-ptr #:os-msync-ptr))

