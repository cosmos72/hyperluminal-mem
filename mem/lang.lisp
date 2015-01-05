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


(in-package :hyperluminal-mem)

(eval-when (:compile-toplevel :load-toplevel)



  (defun find-hldb-option/string (prefix)
    (declare (type symbol prefix))
    (let* ((prefix-name (stringify 'hyperluminal-db/ prefix '/))
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

  (defun choose-word-type ()
    "Choose the file format and ABI between 32 or 64 bit - and possibly more in the future.

By default, Hyperluminal-DB file format and ABI is autodetected to match
Lisp idea of CFFI-SYS pointers:
* 32 bit when CFFI-SYS pointers are 32 bit,
* 64 bit when CFFI-SYS pointers are 64 bit,
* and so on...

In other words, `mem-word` is normally autodetected to match the width
of underlying CPU registers (exposed through CFFI-SYS foreign-type :pointer)
and `+msizeof-word+` is set accordingly.

It is possible to override such autodetection by adding an appropriate entry
in the global variable `*FEATURES*` **before** compiling and loading Hyperluminal-DB.
Doing so disables autodetection and either tells Hyperluminal-DB the desired size
of `mem-word`, in alternative, the CFFI-SYS type it should use for `mem-word`.

For example, to force 64 bit (= 8 bytes) file format and ABI even on 32-bit systems,
execute the following form before compiling and loading Hyperluminal-DB:
    (pushnew :hyperluminal-db/word-size/8 *features*)

on the other hand, to force 32 bit (= 4 bytes) file format and ABI,
execute the form
    (pushnew :hyperluminal-db/word-size/4 *features*)

in both cases, the Hyperluminal-DB internal function (choose-word-type)
will recognize the override and define `mem-word` and `+msizeof-word+`
to match a CFFI-SYS unsigned integer type having the specified size
among the following candidates:
    :unsigned-char
    :unsigned-short
    :unsigned-int
    :unsigned-long
    :unsigned-long-long
In case it does not find a type with the requested size, it will raise an error.

Forcing the same value that would be autodetected is fine and harmless.
Also, the chosen type must be 32 bits wide or more, but there is no upper limit:
Hyperluminal-DB is designed to automatically support 64 bits systems,
128 bit systems, and anything else that will exist in the future.
It even supports 'unusual' configurations where the size of `mem-word`
is not a power of two (ever heard of 36-bit CPUs?).

For the far future (which arrives surprisingly quickly in software)
where CFFI-SYS will know about further unsigned integer types,
it is also possible to explicitly specify the type to use
by executing a form like
  (pushnew :hyperluminal-db/word-type/<SOME-CFFI-SYS-TYPE> *features*)
as for example:
  (pushnew :hyperluminal-db/word-type/unsigned-long-long *features*)

Hyperluminal-DB will honour such override, intern the type name
to convert it to a keyword, use it as the definition of `mem-word`,
and derive `+msizeof-word+` from it."

    ;;search for :hyperluminal-db/word-type/<SOME-CFFI-SYS-TYPE> in *features*
    (when-bind type (find-hldb-option/keyword 'word-type)
      (return-from choose-word-type (the symbol type)))

    ;; search for :hyperluminal-db/word-size/<INTEGER> in *features*
    (let ((size (or (find-hldb-option/integer 'word-size)
                    ;; default is pointer size
                    (ffi-sizeof :pointer)))
          (types (loop for type in '(:unsigned-char :unsigned-short :unsigned-int
                                     :unsigned-long :unsigned-long-long)
                      collect (cons (ffi-sizeof type) type))))

      (when-bind type (rest (assoc (the fixnum size) types))
        (return-from choose-word-type (the symbol type)))
          
      (error "Hyperluminal-DB: failed to find a CFFI-SYS unsigned integer type
having size = ~S. Tried the following types: ~S" size types))))


