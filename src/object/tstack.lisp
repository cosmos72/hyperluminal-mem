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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read and write STMX.UTIL:TSTACK                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod msize-object ((obj tstack) msize-func index)
  (declare (type function msize-func)
           (type mem-size index))

  (call-msize (msize-func index) (_ obj top)))


(defmethod mwrite-object ((obj tstack) mwrite-func ptr index end-index)
  (declare (type function mwrite-func)
           (type mem-size index end-index))

  (call-mwrite (mwrite-func ptr index end-index) (_ obj top)))


(defmethod mread-object ((type (eql 'tstack)) mread-func ptr index end-index &key)
  (declare (type function mread-func)
           (type mem-size index end-index))

  (multiple-bind-mread (index top) (mread-func ptr index end-index)
    (let ((obj (tstack)))
      (setf (_ obj top) top)
      (values obj index))))

