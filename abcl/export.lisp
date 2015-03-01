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


;;;; * HYPERLUMINAL-ABCL

(in-package :cl-user)


(macrolet ((define-package/reexport (package-name (&key reexport-from) &rest body)
	     (let ((reexport (find-package reexport-from)))
	       `(defpackage ,package-name
		  ,@body
		  (:use ,(package-name reexport))
		  (:export
		   ,@(loop for s being the external-symbols of (find-package reexport-from)
			collect (symbol-name s)))))))

  (define-package/reexport #:hyperluminal-mem-asm
      (:reexport-from #:hlm-abcl)
    (:nicknames #:hlm-asm)))
