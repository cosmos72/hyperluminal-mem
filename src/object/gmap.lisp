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


(defmethod mread-object ((m gmap) mread-func ptr index end-index)
  "Warning: this method expects the caller to have already read the serialized :PRED argument"
  (declare (type function mread-func)
           (type mem-size index end-index))

  (with-mread (size)
    (check-type size mem-size)
    (dotimes (i size)
      (with-mread (key value)
        (set-gmap m key value))))
  
  (values m index))
