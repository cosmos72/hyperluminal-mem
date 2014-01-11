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
;;;;    boxed    RATIOs                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ratio-words (n)
  "Return the number of words needed to store ratio N in mmap memory."
  (declare (type ratio n))

  (error "TODO"))


(defun box-words/ratio (n)
  "Return the number of words needed to store a BOX containing ratio N in mmap memory."
  (declare (type integer n))
  (the mem-size (mem-size+ 1 +mem-box/header-words+ (ratio-words n))))
  


(defun mwrite-box/ratio (ptr index n-words n)
  "Reuse the memory block starting at (PTR+INDEX) and write ratio N into it.

ABI: ratio is stored as box prefix, followed by numerator and denominator."
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type integer n))

  (error "TODO"))



(defun mread-box/ratio (ptr index)
  "Read a ratio from the boxed memory starting at (PTR+INDEX).
Return the ratio"
  (declare (type maddress ptr)
           (type mem-size index))
  
  (error "TODO"))


  

