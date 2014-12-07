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

(defpackage #:hyperluminal-mem

  (:nicknames #:hlmem)

  (:use #:cl #:hyperluminal-ffi)

  (:import-from #:stmx.lang

                #:eval-always  #:enable-#?-syntax

                #:set-feature  #:set-features #:default-feature #:default-features
                #:get-feature  #:all-features?

                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen    #:aif 
                #:log.debug    #:log.trace     #:log.make-logger)

  (:import-from #:stmx
                #:+unbound-tvar+)

  (:import-from #:stmx.util
                
                #:fixnum< #:fixnum> #:fixnum=

                #:_ #:tcell #:tcons #:tlist #:tstack #:tfifo

                #:tmap #:rbmap #:gmap #:gmap-pred #:gmap-count #:get-gmap #:set-gmap #:do-gmap

                #:thash-table #:ghash-table #:ghash-table-test #:ghash-table-hash
                #:ghash-table-count #:set-ghash #:do-ghash)
                

  (:export #:hlmem-version #:hlmem-abi-version
           
           #:maddress     #:mem-word       #:mem-size    #:+msizeof-word+
           #:malloc       #:malloc-words   #:mfree       #:with-mem-words
           #:mread-magic  #:mwrite-magic

           #:mget-unboxed #:mset-unboxed   #:mset-fulltag-and-value
           #:msize        #:mwrite         #:mread
           #:msize*       #:mwrite*        #:with-mread*

           #:box          #:make-box       #:reuse-box
           #:mread-box    #:mwrite-box

           #:msize-object       #:mwrite-object        #:mread-object
           #:msize-object-slots #:mwrite-object-slots  #:mread-object-slots
           #:mlist-object-slots #:mwrite-object-slot   #:mwrite-slot        
           #:mread-object-slots-using-names
                                                  
           #:decl-msize-class   #:decl-mwrite-class    #:decl-mread-class
           #:decl-mserializable-class

           #:!mdump #:!mdump-bytes #:!mdump-bytes-reverse #:!memset-words))
