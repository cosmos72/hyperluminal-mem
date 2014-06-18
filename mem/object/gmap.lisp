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


(in-package :hyperluminal-mem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read and write sorted map STMX.UTIL:RBMAP                             ;;;;
;;;;   and its transactional version STMX.UTIL:TMAP                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod msize-object ((obj gmap) index)
  (declare (type mem-size index))

  (setf index (msize* index (gmap-pred obj) (gmap-count obj)))
  (do-gmap (key value) obj
    (setf index (msize* index key value)))
  index)


(defmethod mwrite-object ((obj gmap) ptr index end-index)
  (declare (type mem-size index end-index))

  (setf index (mwrite* ptr index end-index (gmap-pred obj) (gmap-count obj)))
  (do-gmap (key value) obj
    (setf index (mwrite* ptr index end-index key value)))
  index)


(defmethod mread-object ((obj gmap) ptr index end-index &key)
  "Warning: this method expects the caller to have already read the serialized
:PRED argument and instantiated a GMAP or a subclass"
  (declare (type mem-size index end-index))

  (multiple-value-bind (size index) (mread ptr index end-index)
    (check-type size mem-uint)
    (dotimes (i size)
      (with-mread* (key value new-index) (ptr index end-index)
        (set-gmap obj key value)
        (setf index new-index)))
  
    (values obj index)))



;; we currently do NOT allow deserializing arbitrary functions as RBMAP predicates:
;; it would allow a malicious remote user to execute arbitrary code!
(define-constant-once +gmap-trusted-pred-list+ '(< fixnum< char< string<))

(defun mread-object/gmap (type ptr index end-index)
  (declare (type symbol type)
           (type maddress ptr)
           (type mem-size index end-index))

  (multiple-value-bind (pred index) (mread ptr index end-index)

    (unless (member pred +gmap-trusted-pred-list+)
      (error "HYPERLUMINAL-DB: refusing to use untrusted ~S ~S value ~S,
expecting one of the trusted values ~S" type :pred pred +gmap-trusted-pred-list+))

    (mread-object (new type :pred pred) ptr index end-index)))


               


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read RBMAP                                                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod mread-object ((type (eql 'rbmap)) ptr index end-index &key)
  (declare (type mem-size index end-index))

  (mread-object/gmap type ptr index end-index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read TMAP                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mread-object ((type (eql 'tmap)) ptr index end-index &key)
  (declare (type mem-size index end-index))

  (mread-object/gmap type ptr index end-index))

