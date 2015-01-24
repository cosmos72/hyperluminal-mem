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
