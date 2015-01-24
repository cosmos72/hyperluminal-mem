;; -*- lisp -*-

;; This file is part of Hyperluminal-MEM.
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


;;;; * HYPERLUMINAL-SBCL

(in-package :cl-user)


(defpackage #:hyperluminal-sbcl
  (:nicknames #:hl-sbcl)
  (:use #:cl #:hyperluminal-lang #:hyperluminal-ffi)
  (:import-from #:stmx.lang
                #:eval-always  #:with-gensym  #:with-gensyms)

  (:export #:fast-sap #:sap=>fast-sap #:fast-sap=>sap
	   #:+fixnum-zero-mask+1+
	   #:fast-mread/4 #:fast-mwrite/4
	   #+x86-64 #:fast-mread/8
           #+x86-64 #:fast-mwrite/8))


(defpackage #:hyperluminal-asm
  (:nicknames #:hl-asm)
  (:use #:hyperluminal-sbcl)
  (:export #:fast-sap #:sap=>fast-sap #:fast-sap=>sap
	   #:+fixnum-zero-mask+1+
	   #:fast-mread/4 #:fast-mwrite/4
	   #+x86-64 #:fast-mread/8
           #+x86-64 #:fast-mwrite/8))
