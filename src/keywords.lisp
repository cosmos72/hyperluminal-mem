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


(in-package :hyperluminal-db)


(define-constant-once +keywords-vector+ #(
 :compile-toplevel :load-toplevel :execute ;; eval-when options
 :inherited :external :internal ;; intern options
 :element-type :initial-element :initial-contents :adjustable :fill-pointer :displaced-to :displaced-index-offset ;; make-array options
 :test :size :rehash-size :rehash-threshold ;; make-hash-table options
 :case :common :local ;; pathname-* functions options
 :wild :newest :unspecific :oldest :previous :installed ;; used inside pathname components
 ))


(defconstant +mem-keywords/first+ 1024 "first value used for predefined KEYWORDS")
(defconstant +mem-keywords/last+ (+ +mem-keywords/first+ (length +keywords-vector+) -1) "last value used for predefined KEYWORDS")

(define-constant-once +keywords-table+  (symbols-to-table +cl-symbols-vector+ +mem-keywords/first+))


(defconstant +mem-syms-user/first+ 2048 "first value available for user-defined symbols")


