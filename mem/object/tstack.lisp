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

  (msize (_ obj top) index))


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
