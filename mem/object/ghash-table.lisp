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
;;;;   read and write hash table STMX.UTIL:GHASH-TABLE                       ;;;;
;;;;   and its transactional version STMX.UTIL:THASH-TABLE                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod msize-object ((obj ghash-table) index)
  (declare (type mem-size index))

  (setf index (msize* index
                      (ghash-table-test obj)
                      (ghash-table-hash obj)
                      (ghash-table-count obj)))
  (do-ghash (key value) obj
    (setf index (msize* index key value)))
  index)


(defmethod mwrite-object ((obj ghash-table) ptr index end-index)
  (declare (type mem-size index end-index))

  (setf index (mwrite* ptr index end-index
                       (ghash-table-test obj)
                       (ghash-table-hash obj)
                       (ghash-table-count obj)))
  (do-ghash (key value) obj
    (setf index (mwrite* ptr index end-index key value)))
  index)


(defmethod mread-object ((obj ghash-table) ptr index end-index
                         &key size &allow-other-keys)
  "Warning: this method expects the caller to have already read the serialized
:TEST, :HASH and :SIZE values and instantiated a GHASH-TABLE or a subclass"
  (declare (type mem-size index end-index)
           (type mem-uint size))

  (dotimes (i size)
    (with-mread* (key value new-index) (ptr index end-index)
      (set-ghash obj key value)
      (setf index new-index)))
  
  (values obj index))



;; we currently do NOT allow deserializing arbitrary functions as GHASH-TABLE predicates:
;; it would allow a malicious remote user to execute arbitrary code!
(define-constant-once +ghash-table-trusted-test-list+
    '(eq eql equal equalp = fixnum= char= string-equal))


(define-constant-once +ghash-table-trusted-hash-list+
    '(sxhash identity))


(defun mread-object/ghash-table (type ptr index end-index)
  (declare (type symbol type)
           (type mem-size index end-index))

  (with-mread* (test hash size new-index) (ptr index end-index)
    (unless (member test +ghash-table-trusted-test-list+)
      (error "HYPERLUMINAL-DB: refusing to use untrusted ~S ~S value ~S,
expecting one of the trusted values ~S" type :test test +ghash-table-trusted-test-list+))
    (unless (member hash +ghash-table-trusted-hash-list+)
      (error "HYPERLUMINAL-DB: refusing to use untrusted ~S ~S value ~S,
expecting one of the trusted values ~S" type :hash hash +ghash-table-trusted-hash-list+))
               
    (check-type size mem-uint)

    (mread-object (new type :test test :hash hash :initial-capacity size)
                  ptr new-index end-index :size size)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read GHASH-TABLE                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod mread-object ((type (eql 'ghash-table)) ptr index end-index &key)
  (declare (type mem-size index end-index))

  (mread-object/ghash-table type ptr index end-index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read THASH-TABLE                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mread-object ((type (eql 'thash-table)) ptr index end-index &key)
  (declare (type mem-size index end-index))

  (mread-object/ghash-table type ptr index end-index))

