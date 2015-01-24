;; -*- lisp -*-

;; This file is part of Hyperluminal-MEM.
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
;;;;    SYMBOLs                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun %package-words (pkg index)
  "Return the number of words needed to store package PKG in mmap memory."

  (if (or (eq pkg nil)
          (eq pkg +package-keyword+)
          (eq pkg +package-common-lisp+)
          (eq pkg +package-common-lisp-user+))
      ;; unboxed package reference
      (incf-mem-size index)
      ;; untagged package name: mem-int length, then utf-8 chars
      (box-words/string-utf-8 (package-name pkg) index)))


(declaim (inline %mwrite-package %mread-package))

(defun %mwrite-package (ptr index end-index pkg)
  "Write package PKG into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after written words.

ABI: package is stored as package reference if possible, otherwise as package name."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type (or null package) pkg))

  (let ((tag +mem-tag/package+)
        (val +mem-pkg/common-lisp+))
    (cond
      ((eq pkg +package-keyword+)          (setf val +mem-pkg/keyword+))
      ((eq pkg +package-common-lisp+))
      ((eq pkg +package-common-lisp-user+) (setf val +mem-pkg/common-lisp-user+))

      ((eq pkg nil)                        (setf tag +mem-tag/symbol+ 
                                                 val +mem-sym/nil+))
      (t
       (return-from %mwrite-package
         (mwrite-box/string-utf-8 ptr index end-index (package-name pkg)))))

    (mset-fulltag-and-value ptr index tag val)
    (mem-size+1 index)))


(defun %mread-package (ptr index end-index)
  "Read a package from the memory starting at (PTR+INDEX).
Return the package and INDEX pointing to immediately after words read.

ABI: package is stored as package reference if possible, otherwise as package name."
  (declare (type maddress ptr)
           (type mem-size index end-index))

  (bind-fulltag-and-value (fulltag value) (ptr index)
    (if (= +mem-tag/symbol+ fulltag)
        (values
         (ecase value
           (#.+mem-pkg/keyword+          +package-keyword+)
           (#.+mem-pkg/common-lisp+      +package-common-lisp+)
           (#.+mem-pkg/common-lisp-user+ +package-common-lisp-user+))
         (mem-size+1 index))
       
        (mread-box/string-utf-8 ptr index end-index))))


(defun box-words/symbol (sym index)
  "Return the number of words needed to store symbol SYM in mmap memory.
Does not count the space needed by BOX header."
  (declare (type symbol sym)
           (type mem-size index))

  ;; assume symbol does NOT have predefined representation

  (let1 index (%package-words (symbol-package sym) index)
    (box-words/string-utf-8   (symbol-name    sym) index)))


(defun mwrite-box/symbol (ptr index end-index sym)
  "Write symbol SYM into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after words written.

ABI: symbol is stored as package followed by symbol name.
To store symbol as unboxed reference, use MSET-UNBOXED or MWRITE."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type symbol sym))

  ;; assume symbol does NOT have predefined representation
  (setf index (%mwrite-package ptr index end-index (symbol-package sym)))

  (mwrite-box/string-utf-8 ptr index end-index (symbol-name sym)))
    


(defun mread-box/symbol (ptr index end-index)
  "Read a symbol from the memory starting at (PTR+INDEX) and return it.
Return the symbol and the INDEX pointing to immediately after the words read.
Assumes BOX header is already read.

ABI: symbol is assumed to be stored as package followed by symbol name.
To read symbol stored as unboxed reference, use MGET-UNBOXED or MREAD."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  ;; assume symbol does NOT have predefined representation

  (multiple-value-bind (pkg index) (%mread-package ptr index end-index)
    (multiple-value-bind (sym-name index) (mread-box/string-utf-8 ptr index end-index)
      (values
       (if pkg
           (intern sym-name pkg)
           (make-symbol (the simple-string sym-name)))
       index))))
