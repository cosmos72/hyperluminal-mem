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

;;;; * HYPERLUMINAL-MEM-TEST

(in-package :cl-user)

(defpackage #:hyperluminal-mem-test

  (:nicknames #:hlm-test #:hlmem-test)

  (:use #:cl
        #:fiveam
        #:stmx.util
        #:hyperluminal-mem)

  (:import-from #:stmx.lang
                #:enable-#?-syntax)

  (:import-from #:hyperluminal-mem
                #:!mdump #:+most-negative-int+ #:+most-positive-int+
                #:+stmx-unbound-tvar+  #:+stmx-empty-tcell+)

  (:export #:suite #:loop-run-tests))




(in-package :hyperluminal-mem-test)

(fiveam:def-suite suite)
