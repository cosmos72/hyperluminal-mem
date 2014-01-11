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
;;;;    boxed pathname                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-words/pathname (value)
  "Return the number of words needed to store pathname VALUE in mmap memory, not including BOX header."
  (declare (type pathname value))

  (error "TODO"))
  

(defun mwrite-box/pathname (ptr index value)
  "Reuse the memory block starting at (PTR+INDEX) and write pathname VALUE into it.
Assumes BOX header is already written."
  (declare (type maddress ptr)
           (type mem-size index)
           (type pathname value))

  (error "TODO"))


(defun mread-box/pathname (ptr index)
  "Read a pathname from the boxed memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (error "TODO"))
