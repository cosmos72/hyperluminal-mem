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

(stmx.lang:enable-#?-syntax)

(defpackage #:hyperluminal-mem

  (:nicknames #:hlm-mem #:hlmem)

  (:use #:cl #:hyperluminal-mem-lang #:hyperluminal-mem-ffi)

  (:import-from #:stmx.lang

                #:eval-always  #:enable-#?-syntax #:get-feature
                #:set-feature  #:set-features


                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen    #:aif)

  (:import-from #:stmx
                #:+unbound-tvar+)

  (:import-from #:stmx.util
                
                #:fixnum< #:fixnum> #:fixnum=

                #:_ #:tcell #:tcons #:tlist #:tstack #:tfifo

                #:tmap #:rbmap #:gmap #:gmap-pred #:gmap-count
                #:get-gmap #:set-gmap #:do-gmap

                #:thash-table #:ghash-table
                #:ghash-table-count #:get-ghash #:set-ghash #:do-ghash

                ;; ghash-table-test and ghash-table-hash require STMX >= 2.0.1
                #?+(symbol :stmx.util :ghash-table-test) #:ghash-table-test
                #?+(symbol :stmx.util :ghash-table-hash) #:ghash-table-hash)

                

  (:export #:hlmem-version #:hlmem-abi-version
	   
           #:maddress     #:mem-word       #:mem-size    #:+msizeof-word+
           #:mread-magic  #:mwrite-magic   

           #:mget-unboxed #:mset-unboxed   #:mset-fulltag-and-value
           #:msize        #:mwrite         #:mread
           #:msize*       #:mwrite*        #:with-mread*

           #:msize-object       #:mwrite-object        #:mread-object
           #:msize-object-slots #:mwrite-object-slots  #:mread-object-slots
           #:mlist-object-slots #:mwrite-object-slot   #:mwrite-slot        
           #:mread-object-slots-using-names

           #:with-mem-bytes     #:with-mem-words       #:with-vector-mem
           #:malloc             #:malloc-words         #:mfree
           #:mzero-words        #:memset-words         #:memcpy-words

           #:decl-msize-class   #:decl-mwrite-class    #:decl-mread-class
           #:decl-mserializable-class

           
           #:!mdump #:!mdump-bytes #:!mdump-bytes-reverse))
