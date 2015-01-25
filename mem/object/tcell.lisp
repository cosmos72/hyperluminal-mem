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
;;;;   read and write STMX.UTIL:TCELL                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; short version :)
(decl-mserializable-class tcell
                          :slots (stmx.util::value)
                          :new-instance (tcell))


;; medium version... for comparison
#|
(undecl-mlist-class-direct-slots 'tcell)
(decl-mlist-class-slots tcell :slots (stmx.util::value))
(decl-msize-class       tcell :slots (stmx.util::value))
(decl-mwrite-class      tcell :slots (stmx.util::value))
(decl-mread-class       tcell :slots (stmx.util::value) :new-instance (tcell))
|#


;; and long version too.
#|
;; remove the optional method MLIST-CLASS-DIRECT-SLOTS,
;; invoked only at compile time by DECL-M...-CLASS macros
(undecl-mlist-class-direct-slots 'tcell)

(defmethod mlist-class-slots ((class (eql 'tcell)))
  "Optional method, invoked only at compile time by DECL-M...-CLASS macros"
  '(stmx.util::value))

(defmethod msize-object ((c tcell) index)
  (declare (type mem-size index))

  (msize index (_ c value)))


(defmethod mwrite-object ((c tcell) ptr index end-index)
  (declare (type mem-size index end-index))

  (mwrite ptr index end-index (_ c value)))


(defmethod mread-object ((type (eql 'tcell)) ptr index end-index &key)
  (declare (type mem-size index end-index))

  (multiple-value-bind (value index) (mread ptr index end-index)
    (values (tcell value) index)))
|#
