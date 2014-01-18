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


(let ((+package-keyword+          (find-package '#:keyword))
      (+package-common-lisp+      (find-package '#:common-lisp))
      (+package-common-lisp-user+ (find-package '#:common-lisp-user)))
      

  (defun %package-words (pkg)
    "Return the number of words needed to store package PKG in mmap memory."
    (if (or (eq pkg +package-keyword+)
            (eq pkg +package-common-lisp+)
            (eq pkg +package-common-lisp-user+))
        1
        (detect-box-n-words (package-name pkg))))


  (defun %mwrite-package (ptr index end-index pkg)
    "Write symbol SYM into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after written symbol.

ABI: package is stored as package reference if possible, otherwise as package name."
    (declare (type maddress ptr)
             (type mem-size index end-index)
             (type (or null package) pkg))

    ;; TODO FINISH THIS!
    (let ((val nil))
      (cond
        ((eq pkg +package-keyword+)          (setf val +mem-pkg/keyword+))
        ((eq pkg +package-common-lisp+)      (setf val +mem-pkg/common-lisp+))
        ((eq pkg +package-common-lisp-user+) (setf val +mem-pkg/common-lisp-user+))

        (t))))
         
    

  (defun %symbol-words (sym)
    "Return the number of words needed to store symbol SYM in mmap memory."
    (declare (type symbol sym))

    (when (or (eq sym t) (eq sym nil))
      (return-from %symbol-words 1))

    (let ((pkg (symbol-package sym)))
      (when (and (eq pkg +package-common-lisp+)
                 (gethash sym +cl-symbols-table+))
        ;; symbol has a predefined representation
        (return-from %symbol-words 1))

      (let* ((pgk-words (%package-words pkg))
             (sym-words (detect-box-n-words (symbol-name sym)))
             (words (mem-int+ 1 pgk-words sym-words)))

        (unless (<= words +mem-box/max-payload-words+)
          (error "HYPERLUMINAL-DB: symbol too long for object store,
it requires ~S words, maximum supported is ~S words" words +mem-box/max-payload-words+))

        (the mem-size words))))

      
  (defun obj-words/symbol (sym)
    "Return the number of words needed to store an object containing symbol SYM in mmap memory."
    (mem-size+ +mem-box/header-words+ (%symbol-words sym)))
  

  (defun %mwrite-symbol (ptr index end-index sym pkg)
    "Write symbol's package name followed by symbol name."
    (declare (type maddress ptr)
             (type mem-size index end-index)
             (type symbol sym)
             (type (or null package) pkg))

    (let ((new-index
           (if pkg
               (mwrite ptr (mem-size+1 index) end-index (package-name pkg))
               (progn
                 (mset-fulltag-and-value ptr index +mem-tag/symbol+ +mem-sym/nil+)
                 (mem-size+1 index)))))
      (setf new-index (mwrite ptr new-index end-index (symbol-name sym)))
      (mwrite-box/header ptr index +mem-obj/symbol+ (mem-size- new-index index))
      new-index))


  (defun mwrite-obj/symbol (ptr index end-index sym)
    "Write symbol SYM into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after written symbol.

ABI: symbol is stored as symbol reference if possible, otherwise as package followed by symbol name."
    (declare (type maddress ptr)
             (type mem-size index end-index)
             (type symbol sym))

    (let ((val +mem-sym/nil+))
      (cond
        ((eq sym nil))
        ((eq sym t)              (setf val +mem-sym/t+))
        ((eq sym +unbound-tvar+) (setf val +mem-sym/unbound+))

        (t
         (let ((pkg (symbol-package sym)))
           (when (eq pkg +package-common-lisp+)
             (when-bind index (gethash sym +cl-symbols-table+)
               ;; symbol has a predefined representation
               (mset-fulltag-and-value ptr index +mem-tag/symbol+ (the mem-pointer index))
               (return-from mwrite-obj/symbol 1)))
         
           (return-from mwrite-obj/symbol (%mwrite-symbol ptr index end-index sym pkg)))))

      (mset-fulltag-and-value ptr index +mem-tag/symbol+ val)
      (return-from mwrite-obj/symbol 1)))

  (defun %mread-symbol (ptr index end-index)
    "Read a symbol's package and name from the memory starting at (PTR+INDEX).
Return the symbol and the INDEX pointing to immediately after the symbol."
    (declare (type maddress ptr)
             (type mem-size index end-index))
    
    (multiple-value-bind (pkg-name index) (mread ptr index end-index)
      (multiple-value-bind (sym-name index) (mread ptr index end-index)
        (values
         (if pkg-name
             (intern sym-name (find-package pkg-name))
             (make-symbol sym-name))
         index))))
  
  (defun mread-obj/symbol (ptr index end-index)
    "Read a symbol from the memory starting at (PTR+INDEX).
Return the symbol and the INDEX pointing to immediately after the symbol."
    (declare (type maddress ptr)
             (type mem-size index end-index))

    (multiple-value-bind (fulltag value) (mget-fulltag-and-value ptr index)
      (if (= +mem-tag/symbol+ fulltag)
          (when (<= +mem-syms-cl/first+ value +mem-syms-cl/last+)
            (svref +cl-symbols-vector+ (- value +mem-syms-cl/first+)))
          (%mread-symbol ptr (mem-size+1 index) (min end-index (mem-size+ index (box-pointer->size value))))))))
  

