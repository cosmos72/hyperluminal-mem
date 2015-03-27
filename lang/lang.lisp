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


(in-package :hyperluminal-mem-lang)

(eval-when (:compile-toplevel :load-toplevel)
  (pushnew :hyperluminal-mem *features*)

  #-(and)
  (pushnew :hyperluminal-mem/debug *features*)

  #+abcl ;; #?+abcl is used in mem/float.lisp
  (set-feature :abcl)

  ;; does CPU allow unaligned reads and writes of double-floats?
  ;; used in mem/float.lisp to split double-float reads and writes,
  ;; otherwise SPARC (and possibly other CPUs) will signal SIGBUS
  (set-feature :cpu/double-float/unaligned #+sparc nil))


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

(defun stringify (&rest things)
  "Print the things to a string and return it"
  (let ((s (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (*print-array*    t)
        (*print-base*     10)
        (*print-escape*   nil)
        (*print-gensym*   nil)
        (*print-pretty*   nil)
        (*print-radix*    nil)
        (*print-readably* nil))
    (dolist (thing (unwrap-list-1 things))
      (format s "~A" thing))
    s))

(defun concat-symbols (&rest things)
  "Print the things to a string, the convert the string into a symbol interned in current package.
Return the symbol"
  (intern (apply #'stringify things)))


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
