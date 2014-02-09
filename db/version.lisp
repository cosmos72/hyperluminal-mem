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


(define-constant-once +hldb-version+ hlmem::+hlmem-version+)

(define-constant-once +hldb-abi-version+ '(0 1 0))


(defun hldb-version ()
  "Return HYPERLUMINAL-DB version, in the form '(major minor patch)
as for example '(0 4 0)"
  +hldb-version+)


(defun hldb-abi-version ()
  "Return HYPERLUMINAL-DB file format and ABI version, in the form '(major minor patch)
as for example '(0 1 0)"
  +hldb-abi-version+)
