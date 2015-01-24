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

(defpackage #:hyperluminal-db

  (:nicknames #:hl-db #:hldb)

  (:use #:cl #:hyperluminal-ffi)

  (:import-from #:stmx.lang

                #:enable-#?-syntax  #:eval-always  #:set-feature 

                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen    #:aif 
                #:log.debug    #:log.trace     #:log.make-logger)

  (:import-from #:hyperluminal-ffi

                #:os-getpagesize #:ffi-defstruct)
                
  (:import-from #:hyperluminal-mem

                #:+null-pointer+         #:+mem-unallocated+
                #:+most-positive-size+   #:+msizeof-word+
                #:+mem-box/min-words+    #:+mem-box/max-words+
                #:+mem-box/header-words+ #:+mem-box/min-payload-words+

                #:+native-word-type+   #:msizeof    #:mget-t    #:mset-t
                #:mem-word
                
                #:mem-size #:mem-size+ #:mem-size+1 #:mem-size- #:mem-size-1
                #:incf-mem-size        #:mem-size*
                #:mget-value #:mset-fulltag-and-value

                #:mread-magic #:mwrite-magic #:!mread #:!mwrite #:!mzero-words

                #:box-pointer->size          #:size->box-pointer
                #:box         #:make-box     #:box-n-words
                #:reuse-box   #:box-realloc  #:box-index  #:box-value)

  (:export      #:hldb-version #:hldb-abi-version
                #:hldb-open    #:hldb-close))
                
