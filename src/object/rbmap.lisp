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
;;;;   read and write RBMAPs i.e. binary trees implemented in STMX library   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; we currently do NOT allow deserializing arbitrary functions as RBMAP predicates:
;; it would allow a malicious remote user to execute arbitrary code!
(define-constant-once +rbmap-trusted-pred-list+ '(< fixnum< char< string<))


(defmethod mread-object ((type (eql 'rbmap)) mread-func ptr index end-index)
  (declare (type function mread-func)
           (type mem-size index end-index))

  (bind-%mread (pred)
    (unless (member pred +rbmap-trusted-pred-list+)
      (error "HYPERLUMINAL-DB: refusing to use untrusted ~S ~S value ~S,
expecting one of the trusted values ~S" 'rbmap :pred pred +rbmap-trusted-pred-list+))
               
    (let ((m (new 'rbmap :pred pred)))
      
      (mread-object m mread-func ptr index end-index))))
