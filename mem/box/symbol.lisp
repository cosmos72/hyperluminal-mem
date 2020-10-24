;; -*- lisp -*-

;; This file is part of Hyperluminal-mem.
;; Copyright (c) 2013-2015 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package :hyperluminal-mem)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    SYMBOLs                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun %package-words (index pkg)
  "Return the number of words needed to store package PKG in mmap memory."

  (if (or (eq pkg nil)
          (eq pkg +package-keyword+)
          (eq pkg +package-common-lisp+)
          (eq pkg +package-common-lisp-user+))
      ;; unboxed package reference
      (incf-mem-size index)
      ;; untagged package name: mem-int length, then utf-8 chars
      (msize-box/string-utf-8 index (package-name pkg))))


(declaim (inline %mwrite-package %mread-package))

(defun %mwrite-package (ptr index end-index pkg)
  "Write package PKG into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after written words.

ABI: package is stored as package reference if possible, otherwise as package name."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type (or null package) pkg))

  (let ((tag +mem-tag/package+)
        (vid +mem-pkg/common-lisp+))
    (cond
      ((eq pkg +package-keyword+)          (setf vid +mem-pkg/keyword+))
      ((eq pkg +package-common-lisp+))
      ((eq pkg +package-common-lisp-user+) (setf vid +mem-pkg/common-lisp-user+))

      ((eq pkg nil)                        (setf tag +mem-tag/symbol+ 
                                                 vid +mem-sym/nil+))
      (t
       (return-from %mwrite-package
         (mwrite-box/string-utf-8 ptr index end-index (package-name pkg)))))

    (check-mem-overrun ptr index end-index 1)
    (mset-tag-and-vid ptr index tag vid)
    (mem-size+1 index)))


(defun %mread-package (ptr index end-index)
  "Read a package from the memory starting at (PTR+INDEX).
Return the package and INDEX pointing to immediately after words read.

ABI: package is stored as package reference if possible, otherwise as package name."
  (declare (type maddress ptr)
           (type mem-size index end-index))

  (check-mem-length ptr index end-index 1)
  (with-tag-and-vid (tag vid) (ptr index)
    (if (= +mem-tag/symbol+ tag)
        (values
         (ecase vid
           (#.+mem-pkg/keyword+          +package-keyword+)
           (#.+mem-pkg/common-lisp+      +package-common-lisp+)
           (#.+mem-pkg/common-lisp-user+ +package-common-lisp-user+))
         (mem-size+1 index))

        ;; actually return the package name... (find-symbol) below accepts it
        (mread-box/string-utf-8 ptr index end-index))))


(defun msize-box/symbol (index sym)
  "Return the number of words needed to store symbol SYM in mmap memory.
Does not count the space needed by BOX header."
  (declare (type symbol sym)
           (type mem-size index))

  ;; assume symbol does NOT have predefined representation

  (let ((index (%package-words index (symbol-package sym))))
    (msize-box/string-utf-8   index (symbol-name    sym))))


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
    

(defun %read-interactive-symbol ()
  (let ((io *query-io*))
    (format io  "Enter a symbol (not evaluated): ")
    (finish-output io)
    (let ((sym (read io)))
      (check-type sym symbol)
      (multiple-value-list sym))))


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
           (let ((sym (find-symbol sym-name pkg)))
             (unless sym
               (let ((pkg-name (if (typep pkg 'package) (package-name pkg) pkg)))

                 #+(and)
                 (error "Deserialization error: symbol ~A not found in package ~A" sym pkg-name)
               
                 #-(and) ;; reading from *query-io* is always safe?
                 (restart-case
                     (error "Deserialization error: symbol ~A not found in package ~A" sym pkg-name)
                   (store-symbol (new-sym)
                     :report "Supply a new symbol."
                     :interactive %read-interactive-symbol
                     (setf sym new-sym)))))
             sym)
           (make-symbol (the simple-string sym-name)))
       index))))
