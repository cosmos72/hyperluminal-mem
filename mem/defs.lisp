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

(deftype maddress () 'ffi-address)

(deftype ufixnum () '(and fixnum (integer 0)))


(eval-always
  (declaim (type keyword +chosen-word-type+))
  (defconstant +chosen-word-type+ (choose-word-type))

  (defconstant +native-word-type+ (ffi-native-type-name +chosen-word-type+))
  

  (defun parse-type (type)
    (case type
      (:sfloat :float)         ;; this is the ONLY code mapping :sfloat to a CFFI type
      (:dfloat :double)        ;; this is the ONLY code mapping :dfloat to a CFFI type
      (:byte   :unsigned-char) ;; this is the ONLY code mapping :byte to a CFFI type
      (:word   +chosen-word-type+) ;; :word is mapped to a CFFI type by (choose-word-type)
      (otherwise type))))


;; not really used, but handy
#-(and)
(eval-always
 (cffi:defctype mfloat  #.(parse-type :sfloat))
 (cffi:defctype mdouble #.(parse-type :dfloat))
 (cffi:defctype mbyte   #.(parse-type :byte))
 (cffi:defctype mword   #.(parse-type :word)))



(defmacro %msizeof (type)
  "Wrapper for (CFFI-SYS:%FOREIGN-TYPE-SIZE), interprets :SFLOAT :DFLOAT :BYTE AND :WORD"
  `(ffi-sizeof ,(if (constantp type)
                    (parse-type type)
                    `(parse-type ,type))))

(defmacro msizeof (type)
  "Wrapper for (%MSIZEOF), computes (CFFI:%FOREIGN-TYPE-SIZE) at compile time whenever possible"
  (if (constantp type)
      (%msizeof (eval type))
      `(%msizeof ,type)))



(eval-always
  (defconstant +msizeof-sfloat+  (msizeof :sfloat))
  (defconstant +msizeof-dfloat+  (msizeof :dfloat))
  (defconstant +msizeof-byte+    (msizeof :byte))
  (defconstant +msizeof-word+    (msizeof :word)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-always
  (defconstant +fast-mread-symbol+
    (concat-symbols 'fast-mread/ +msizeof-word+))
  (defconstant +fast-mwrite-symbol+
    (concat-symbols 'fast-mwrite/ +msizeof-word+)))

(eval-always
  (set-feature 'hldb/fast-mem
               (and (have-symbol? 'hl-asm +fast-mread-symbol+)
                    (have-symbol? 'hl-asm +fast-mwrite-symbol+))))
      


(defmacro with-mem-bytes ((var-name n-bytes &optional n-bytes-var) &body body)
  `(with-ffi-mem (,var-name ,n-bytes ,@(when n-bytes-var `(,n-bytes-var)))
     ,@body))


(defmacro with-mem-words ((ptr n-words &optional n-words-var)
			      &body body)
  "Bind PTR to N-WORDS words of raw memory while executing BODY.
Raw memory is automatically deallocated when BODY terminates."

  (let* ((n-const? (constantp n-words))
         (n-words  (if n-const? (eval n-words) n-words))
         (n-bytes  (if n-const?
                       (* n-words +msizeof-word+)
                       `(the mem-word (* ,(or n-words-var n-words) +msizeof-word+)))))
    
    `(let ,(when n-words-var `((,n-words-var ,n-words)))
       (with-mem-bytes (,ptr ,n-bytes)
         ,@body))))

          
