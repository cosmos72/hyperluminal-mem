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

  (:nicknames #:hldb #:hl)

  (:use #:cl)

  (:import-from #:stmx.lang

                #:eval-always

                #:enable-#?-syntax
                #:set-feature  #:set-features #:default-feature #:default-features
                #:get-feature  #:all-features?

                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:eval-always  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen #:aif 
                #:log.debug    #:log.trace     #:log.make-logger)

  (:import-from #:stmx
                #:+unbound-tvar+)

  (:export #:+null-pointer+ #:+bad-fd+
           #:open-store #:close-store
           #:mget-unboxed #:mset-unboxed
           #:mread #:mwrite))


