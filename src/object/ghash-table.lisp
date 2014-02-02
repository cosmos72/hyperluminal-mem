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
;;;;   read and write hash table STMX.UTIL:GHASH-TABLE                       ;;;;
;;;;   and its transactional version STMX.UTIL:THASH-TABLE                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod mdetect-object-size ((h ghash-table) mdetect-size-func index)
  (declare (type function mdetect-size-func)
           (type mem-size index))

  (call-mdetect-size (ghash-table-test h)
                     (ghash-table-hash h)
                     (ghash-table-count h))

  (do-ghash (key value) h
    (call-mdetect-size key value))
  index)


(defmethod mwrite-object ((h ghash-table) mwrite-func ptr index end-index)
  (declare (type function mwrite-func)
           (type mem-size index end-index))

  (call-mwrite (ghash-table-test h)
               (ghash-table-hash h)
               (ghash-table-count h))
  (do-ghash (key value) h
    (call-mwrite key value))
  index)


(defmethod mread-object ((h ghash-table) mread-func ptr index end-index
                         &key size &allow-other-keys)
  "Warning: this method expects the caller to have already read the serialized
:TEST, :HASH and :SIZE values and instantiated a GHASH-TABLE or a subclass"
  (declare (type function mread-func)
           (type mem-size index end-index))

  (dotimes (i size)
    (with-mread (key value)
      (set-ghash h key value)))
  
  (values h index))



;; we currently do NOT allow deserializing arbitrary functions as GHASH-TABLE predicates:
;; it would allow a malicious remote user to execute arbitrary code!
(define-constant-once +ghash-table-trusted-test-list+
    '(eq eql equal equalp = fixnum= char= string-equal))


(define-constant-once +ghash-table-trusted-hash-list+
    '(sxhash identity))


(defun mread-object/ghash-table (type mread-func ptr index end-index)
  (declare (type symbol type)
           (type function mread-func)
           (type mem-size index end-index))

  (with-mread (test hash size)
    (unless (member test +ghash-table-trusted-test-list+)
      (error "HYPERLUMINAL-DB: refusing to use untrusted ~S ~S value ~S,
expecting one of the trusted values ~S" type :test test +ghash-table-trusted-test-list+))
    (unless (member hash +ghash-table-trusted-hash-list+)
      (error "HYPERLUMINAL-DB: refusing to use untrusted ~S ~S value ~S,
expecting one of the trusted values ~S" type :hash hash +ghash-table-trusted-hash-list+))
               
    (check-type size mem-uint)

    (mread-object (new type :test test :hash hash :initial-capacity size)
                  mread-func ptr index end-index :size size)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read GHASH-TABLE                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod mread-object ((type (eql 'ghash-table)) mread-func ptr index end-index &key)
  (declare (type function mread-func)
           (type mem-size index end-index))

  (mread-object/ghash-table type mread-func ptr index end-index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read THASH-TABLE                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mread-object ((type (eql 'thash-table)) mread-func ptr index end-index &key)
  (declare (type function mread-func)
           (type mem-size index end-index))

  (mread-object/ghash-table type mread-func ptr index end-index))

