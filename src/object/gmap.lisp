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
;;;;   read and write sorted map STMX.UTIL:RBMAP                             ;;;;
;;;;   and its transactional version STMX.UTIL:TMAP                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod mdetect-object-size ((m gmap) mdetect-size-func index)
  (declare (type function mdetect-size-func)
           (type mem-size index))

  (call-mdetect-size (gmap-pred m) (gmap-count m))

  (do-gmap (key value) m
    (call-mdetect-size key value))
  index)


(defmethod mwrite-object ((m gmap) mwrite-func ptr index end-index)
  (declare (type function mwrite-func)
           (type mem-size index end-index))

  (call-mwrite (gmap-pred m) (gmap-count m))
  (do-gmap (key value) m
    (call-mwrite key value))
  index)


(defmethod mread-object ((m gmap) mread-func ptr index end-index &key)
  "Warning: this method expects the caller to have already read the serialized :PRED argument and instantiated a GMAP or a subclass"
  (declare (type function mread-func)
           (type mem-size index end-index))

  (with-mread (size)
    (check-type size mem-uint)
    (dotimes (i size)
      (with-mread (key value)
        (set-gmap m key value))))
  
  (values m index))



;; we currently do NOT allow deserializing arbitrary functions as RBMAP predicates:
;; it would allow a malicious remote user to execute arbitrary code!
(define-constant-once +gmap-trusted-pred-list+ '(< fixnum< char< string<))

(defun mread-object/gmap (type mread-func ptr index end-index)
  (declare (type symbol type)
           (type function mread-func)
           (type mem-size index end-index))

  (with-mread (pred)
    (unless (member pred +gmap-trusted-pred-list+)
      (error "HYPERLUMINAL-DB: refusing to use untrusted ~S ~S value ~S,
expecting one of the trusted values ~S" type :pred pred +gmap-trusted-pred-list+))
               
    (mread-object (new type :pred pred) mread-func ptr index end-index)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read RBMAP                                                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod mread-object ((type (eql 'rbmap)) mread-func ptr index end-index &key)
  (declare (type function mread-func)
           (type mem-size index end-index))

  (mread-object/gmap type mread-func ptr index end-index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read TMAP                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mread-object ((type (eql 'tmap)) mread-func ptr index end-index &key)
  (declare (type function mread-func)
           (type mem-size index end-index))

  (mread-object/gmap type mread-func ptr index end-index))

