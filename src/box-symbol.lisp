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
;;;;    SYMBOLs                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun %package-words (pkg)
  "Return the number of words needed to store package PKG in mmap memory."
  (if (or (eq pkg +package-keyword+)
          (eq pkg +package-common-lisp+)
          (eq pkg +package-common-lisp-user+))
      1
      (detect-box-n-words (package-name pkg))))


(declaim (inline %mwrite-package %mread-package))

(defun %mwrite-package (ptr index end-index pkg)
  "Write package PKG into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after written words.

ABI: package is stored as package reference if possible, otherwise as package name."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type (or null package) pkg))

  (let ((val +mem-pkg/common-lisp+))
    (cond
      ((eq pkg nil)
       (mset-symbol-ref ptr index +mem-sym/nil+)
       (return-from %mwrite-package (mem-size+1 index)))

      ((eq pkg +package-keyword+)          (setf val +mem-pkg/keyword+))
      ((eq pkg +package-common-lisp+))
      ((eq pkg +package-common-lisp-user+) (setf val +mem-pkg/common-lisp-user+))

      (t
       (return-from %mwrite-package
         (mwrite ptr index end-index (package-name pkg)))))

    (mset-fulltag-and-value ptr index +mem-tag/package+ val)
    (mem-size+1 index)))


(defun %mread-package (ptr index end-index)
  "Read a package from the memory starting at (PTR+INDEX).
Return the package and INDEX pointing to immediately after words read.

ABI: package is stored as package reference if possible, otherwise as package name."
  (declare (type maddress ptr)
           (type mem-size index end-index))

  (multiple-value-bind (value index) (mread ptr index end-index)
    (values
     (cond
       ((eq value nil)                        nil)
       ((eq value +mem-pkg/keyword+)          +package-keyword+)
       ((eq value +mem-pkg/common-lisp+)      +package-common-lisp+)
       ((eq value +mem-pkg/common-lisp-user+) +package-common-lisp-user+)
       (t                                     (find-package value)))
     index)))


(defun box-words/symbol (sym)
  "Return the number of words needed to store symbol SYM in mmap memory.
Does not count the space needed by BOX header."
  (declare (type symbol sym))

  ;; assume symbol does NOT have predefined representation

  (let* ((pkg (symbol-package sym))
         (pkg-words (%package-words pkg))
         (sym-words (detect-box-n-words (symbol-name sym)))
         (words (mem-size+ pkg-words sym-words)))

    (unless (<= words +mem-box/max-payload-words+)
      (error "HYPERLUMINAL-DB: symbol too long for object store,
it requires ~S words, maximum supported is ~S words" words +mem-box/max-payload-words+))

    (the mem-size words)))



(defun mwrite-box/symbol (ptr index end-index sym)
  "Write symbol SYM into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after words written.

ABI: symbol is stored as symbol reference if possible,
otherwise as package followed by symbol name."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type symbol sym))

  ;; assume symbol does NOT have predefined representation

  (let* ((pkg (symbol-package sym))
         (new-index (%mwrite-package ptr index end-index pkg)))

    (mwrite ptr new-index end-index (symbol-name sym))))


(defun mread-box/symbol (ptr index end-index)
  "Read a symbol from the memory starting at (PTR+INDEX) and return it.
Return the symbol and the INDEX pointing to immediately after the words read.

Assumes BOX header is already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  ;; assume symbol does NOT have predefined representation

  (multiple-value-bind (pkg index) (%mread-package ptr index end-index)
    (multiple-value-bind (sym-name index) (mread ptr index end-index)
      (values
       (if pkg
           (intern sym-name pkg)
           (make-symbol (the simple-string sym-name)))
       index))))
