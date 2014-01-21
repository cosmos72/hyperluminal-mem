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

(eval-when (:compile-toplevel :load-toplevel)

  (pushnew :hyperluminal-db *features*)

  #-(and)
  (pushnew :hyperluminal-db/debug *features*)


  (defmacro define-constant-once (name value &optional documentation)
    "If global constant NAME is not yet defined, define it as VALUE.
Otherwise keep its current value."
    `(defconstant ,name 
       (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when documentation `(,documentation))))


  (defun concat-symbols-to-string (&rest syms)
    "Concatenate the names of specified symbols. Returns a string."
    (the string (apply #'concatenate 'string (mapcar #'symbol-name syms))))

  (defun concat-symbols (&rest syms)
    "Concatenate the names of specified symbols. Returns a symbol."
    (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))


  (defmacro check-vector-index (vector index &rest error-message-and-args)
    (with-gensyms (len idx)
      `(let* ((,len (length (the vector ,vector)))
              (,idx (the fixnum ,index)))
         (unless (<= 0 ,idx ,len)
           ,(if error-message-and-args
                `(error ,@error-message-and-args)
                `(error "out of range index ~S: vector has ~S elements" ,idx ,len))))))


  (defun find-hldb-option/string (prefix)
    (declare (type symbol prefix))
    (let* ((prefix-name (concat-symbols-to-string 'hyperluminal-db/ prefix '/))
           (prefix-len (length prefix-name)))
      (loop for f in *features*
         for fname = (if (symbolp f) (symbol-name f) "")
         when (and (> (length fname) prefix-len)
                   (string-equal fname prefix-name :end1 prefix-len))
         return (subseq fname prefix-len))))
           
  (defun find-hldb-option/integer (prefix)
    (when-bind f (find-hldb-option/string prefix)
      (parse-integer f)))

  (defun find-hldb-option/keyword (prefix)
    (when-bind f (find-hldb-option/string prefix)
      (intern f :keyword)))

  ;; choose the file format and ABI between 32 or 64 bit - and possibly more in the future.
  ;;
  ;; by default, the used file format matches Lisp idea of CFFI-SYS pointers:
  ;;    32 bit when CFFI-SYS pointers are 32 bit,
  ;;    64 bit when CFFI-SYS pointers are 64 bit,
  ;;    and so on...
  ;; This is implemented by (choose-word-type) below, that searches for
  ;; a CFFI-SYS unsigned integer type having the same size as CFFI-SYS pointers.
  ;;
  ;; It is possible to override such autodetection by adding an appropriate entry
  ;; in the global variable *FEATURES* which tells to Hyperluminal-DB the size
  ;; or the type it should use instead.
  ;;
  ;; For example, to use 64 bit (= 8 bytes) file format on 32 bit systems,
  ;; execute the following form before loading Hyperluminal-DB:
  ;;   (pushnew :hyperluminal-db/word-size/8 *features*)
  ;;
  ;; on the other hand, to use 32 bit (= 4 bytes) file format on 64 bit systems,
  ;; execute the form
  ;;   (pushnew :hyperluminal-db/word-size/4 *features*)
  ;;
  ;; in both cases, (choose-word-type) will recognize your option
  ;; and search for a CFFI unsigned integer type having the size you specified
  ;; among the following options:
  ;;   :unsigned-char
  ;;   :unsigned-short
  ;;   :unsigned-int
  ;;   :unsigned-long
  ;;   :unsigned-long-long
  ;; In case it does not find a matching type, it will raise an error.
  ;;
  ;; For the far future (which arrives surprisingly quickly in software)
  ;; where CFFI-SYS will know about further unsigned integer types,
  ;; it is also possible to explicitly specify the type to use
  ;; by executing a form like
  ;;   (pushnew :hyperluminal-db/word-type/<SOME-CFFI-SYS-TYPE> *features*)
  ;; as for example:
  ;;   (pushnew :hyperluminal-db/word-type/unsigned-long-long *features*)
  ;;
  ;; Hyperluminal-DB will honour such override, intern the type name
  ;; to convert it to a keyword, and use it.


  (defun choose-word-type ()

    ;; search for :hyperluminal-db/word-type/<SOME-CFFI-SYS-TYPE> in *features*
    (when-bind type (find-hldb-option/keyword 'word-type)
      (return-from choose-word-type type))

    ;; search for :hyperluminal-db/word-size/<INTEGER> in *features*
    (let ((size (or (find-hldb-option/integer 'word-size)
                    ;; default is pointer size
                    (cffi-sys:%foreign-type-size :pointer)))
          (types (loop for type in '(:unsigned-char :unsigned-short :unsigned-int
                                     :unsigned-long :unsigned-long-long)
                      collect (cons (cffi-sys:%foreign-type-size type) type))))

      (when-bind type (rest (assoc (the fixnum size) types))
        (return-from choose-word-type type))
          
      (error "Hyperluminal-DB: failed to find a CFFI-SYS unsigned integer type
having size = ~S. Tried the following types: ~S" size types))))


