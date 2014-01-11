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

(defmacro define-constant-once (name value &optional documentation)
  "If global constant NAME is not yet defined, define it as VALUE.
Otherwise keep its current value."
  `(defconstant ,name 
     (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when documentation `(,documentation))))


(defun symbol-concat (a b)
  "Concatenate the two symbols A and B."
  (declare (type symbol a b))
  (intern (concatenate 'string (symbol-name a) (symbol-name b))))


(defmacro check-vector-index (vector index &rest error-message-and-args)
  (with-gensyms (len idx)
    `(let* ((,len (length (the vector ,vector)))
            (,idx (the fixnum ,index)))
       (unless (<= 0 ,idx ,len)
         ,(if error-message-and-args
              `(error ,@error-message-and-args)
              `(error "out of range index ~S: vector has ~S elements" ,idx ,len))))))
