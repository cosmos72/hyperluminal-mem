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


(in-package :hyperluminal-lang)

(eval-when (:compile-toplevel :load-toplevel)
  (pushnew :hyperluminal-mem *features*)

  #-(and)
  (pushnew :hyperluminal-mem/debug *features*))  


(defun eval-compile-constant (name form)
  (unless (constantp form)
    (error "~S must be a compile-time constant, found ~S" name form))
  (eval form))


(defmacro check-compile-constant (form)
  `(eval-compile-constant ',form ,form))



(defun or-func (&rest args)
  (dolist (arg args nil)
    (when arg
      (return t))))

(defun and-func (&rest args)
  (dolist (arg args t)
    (unless arg
      (return nil))))

(defun unwrap-list-1 (list)
  "If LIST contains a single element which is itself a list, return that element.
Otherwise return the whole LIST"
  (declare (type list list))
  (let ((first (first list)))
    (if (and (listp first) (null (rest list)))
	first
	list)))

(defun %to-string-list (syms-ints-and-strings-list)
  (declare (type list syms-ints-and-strings-list))
  (loop for s in (unwrap-list-1 syms-ints-and-strings-list)
     collect (etypecase s
	       (integer (format nil "~S" s))
	       (symbol (symbol-name s))
	       (string s))))
  
(defun stringify (&rest syms-ints-and-strings)
  "Concatenate the strings, integers and/or names of specified symbols.
Returns a string"
  (the string
    (apply #'concatenate 'string (%to-string-list syms-ints-and-strings))))

(defun concat-symbols (&rest syms-ints-and-strings)
  "Concatenate the strings, integers and/or names of specified symbols.
Returns a symbol interned in current package"
  (intern (apply #'stringify syms-ints-and-strings)))


(defun get-symbol (package-name symbol-name &key errorp)
  (declare (type (or symbol string package) package-name)
           (type (or symbol string)         symbol-name))
  (when (symbolp package-name) (setf package-name (string package-name)))
  (when (symbolp symbol-name)  (setf symbol-name  (string symbol-name)))
  (let ((pkg (find-package package-name)))
    (if pkg
	(multiple-value-bind (sym kind) (find-symbol symbol-name pkg)
	  (if kind
	      (values sym kind)
	      (when errorp
		(error "no symbol ~A in package ~A"
		       symbol-name package-name))))
	(when errorp 
	  (error "no package ~A" package-name)))))


  
(defun have-symbol? (package-name symbol-name)
  (declare (type (or symbol string package) package-name)
           (type (or symbol string)         symbol-name))
  (not (null (nth-value 1 (get-symbol package-name symbol-name)))))

(defun get-fbound-symbol (package-name symbol-name)
  (declare (type (or symbol string package) package-name)
           (type (or symbol string)         symbol-name))
  (multiple-value-bind (sym found) (get-symbol package-name symbol-name)
    (when (and found (fboundp sym))
      sym)))

        

  
(defmacro check-vector-index (vector index &rest error-message-and-args)
  (with-gensyms (len idx)
    `(let* ((,len (length (the vector ,vector)))
	    (,idx (the fixnum ,index)))
       (unless (<= 0 ,idx ,len)
	 ,(if error-message-and-args
	      `(error ,@error-message-and-args)
	      `(error "out of range index ~S: vector has ~S elements"
		      ,idx ,len))))))



(declaim (inline fixnum*))
(defun fixnum* (a b)
  (declare (type fixnum a b))
  (the fixnum (* a b)))

(declaim (inline fixnum+))
(defun fixnum+ (a b)
  (declare (type fixnum a b))
  (the fixnum (+ a b)))

(declaim (inline fixnum-))
(defun fixnum- (a b)
  (declare (type fixnum a b))
  (the fixnum (- a b)))
