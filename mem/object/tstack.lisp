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


(in-package :hyperluminal-mem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read and write STMX.UTIL:TSTACK                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; short version :)
(decl-mserializable-class tstack
                          :slots (stmx.util::top)
                          :new-instance (tstack))

;; medium version... for comparison
#|
(undecl-mlist-class-direct-slots 'tstack)
(decl-mlist-class-slots tstack :slots (stmx.util::top))
(decl-msize-class       tstack :slots (stmx.util::top))
(decl-mwrite-class      tstack :slots (stmx.util::top))
(decl-mread-class       tstack :slots (stmx.util::top) :new-instance (tstack))
|#


;; and long version too.
#|
(undecl-mlist-class-direct-slots 'tstack)

(defmethod mlist-class-slots ((class (eql 'tstack)))
  "Optional method, invoked only at compile time by DECL-M...-CLASS macros"
  '(stmx.util::top))

(defmethod msize-object ((obj tstack) index)
  (declare (type mem-size index))

  (msize index (_ obj top)))


(defmethod mwrite-object ((obj tstack) ptr index end-index)
  (declare (type mem-size index end-index))

  (mwrite ptr index end-index (_ obj top)))


(defmethod mread-object ((type (eql 'tstack)) ptr index end-index &key)
  (declare (type mem-size index end-index))

  (multiple-value-bind (top index) (mread ptr index end-index)
    (let ((obj (tstack)))
      (setf (_ obj top) top)
      (values obj index))))
|#
