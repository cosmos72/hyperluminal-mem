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


;; ABI definition. The values are tuned for good balance
;; between performance and size of addressable store


;;  8 bits reserved for type tags on 32-bit architectures
;; 16 bits reserved for type tags on 64-bit architectures
;; ...
;; the rest of each CPU word is used for pointer index or value
(defconstant +mem-fulltag/bits+ (truncate +mem-word/bits+ 4))
(defconstant +mem-pointer/bits+ (- +mem-word/bits+ +mem-fulltag/bits+))

;; integers (actually, mem-int) are one bit less than CPU words
(defconstant +mem-int/bits+      (1-  +mem-word/bits+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NO USER-SERVICEABLE PARTS BELOW THIS LINE.                                 ;;
;; I.e. everything else is computed from the CPU architecture                 ;;
;; (see (defmacro parse-type) in mem.lisp) and the constants above            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant +pagesize+ (osicat-posix:getpagesize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; type tags and pointers ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-fulltag/shift+     +mem-pointer/bits+)
(defconstant +mem-fulltag/mask+      (1- (ash 1 +mem-fulltag/bits+)))
(defconstant +most-positive-fulltag+ +mem-fulltag/mask+)

;; reserve most significant fulltag bit to mark integers
;; fulltags larger than +most-positive-tag+ indicate an integer (actually, mem-int) value
(defconstant +mem-tag/bits+          (1- +mem-fulltag/bits+))
(defconstant +mem-tag/mask+          (1- (ash 1 +mem-tag/bits+)))
(defconstant +most-positive-tag+      +mem-tag/mask+)

;; pointer offsets stored in MMAP area. To better use the available bits,
;; they are in units of the type pointed to, i.e. increasing them by 1
;; makes them point to the next object of the same type
(defconstant +mem-pointer/shift+     0)
(defconstant +mem-pointer/mask+      (1- (ash 1 +mem-pointer/bits+)))
(defconstant +most-positive-pointer+ +mem-pointer/mask+)

;; pointer offsets used internally by HYPERLUMINAL-DB. They are in units of a CPU word,
;; so to convert from mem-pointer to mem-size you must multiply by the number of words
;; required to store the object pointed to.
(defconstant +mem-size/bits+         (- +mem-word/bits+ (integer-length (1- +msizeof-word+))))
(defconstant +mem-size/mask+         (1- (ash 1 +mem-size/bits+)))
(defconstant +most-positive-size+    +mem-size/mask+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    integers, i.e. mem-int    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-int/mask+          (1- (ash 1 +mem-int/bits+)))
;; bits in a word that exceed a mem-int. if all set to one,
;; it means the rest of the word must be interpreted as a mem-int
(defconstant +mem-int/flag+          (- +mem-word/mask+ +mem-int/mask+))

;; values are the lowest bits in a mem-int
(defconstant +mem-int/value-bits+    (1-  +mem-int/bits+))
(defconstant +mem-int/value-mask+    (ash +mem-int/mask+ -1))
;; sign is the top bit in a mem-int
(defconstant +mem-int/sign-bits+     1)
(defconstant +mem-int/sign-shift+    +mem-int/value-bits+)
(defconstant +mem-int/sign-mask+     (1+ +mem-int/value-mask+))

(defconstant +most-positive-int+     (ash +mem-int/mask+ -1))
(defconstant +most-negative-int+     (lognot +most-positive-int+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        boxed values          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-box/min-words+    4 "boxed values are allocated
    in multiples of 4 CPU-words. This value must be a power of two.")
     
(defconstant +mem-box/max-words+    (* +most-positive-pointer+ +mem-box/min-words+))
(defconstant +mem-box/header-words+ 1 "boxed values have a 1 CPU-word header")
(defconstant +mem-box/payload-words+ (- +mem-box/min-words+ +mem-box/header-words+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   bignums, i.e. mem-bignum   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-bignum/max-words+ (min +most-positive-int+
                                         (- +mem-box/max-words+ +mem-box/header-words+ 1))
  "Maximum number of CPU words in a mem-bignum.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; single-float and double-foat ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-sfloat/bits+ (* +msizeof-sfloat+ +mem-byte/bits+))
(defconstant +mem-dfloat/bits+ (* +msizeof-dfloat+ +mem-byte/bits+))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (unless (zerop (logand +mem-box/min-words+ (1- +mem-box/min-words+)))
    (error "+mem-box/min-words+ is ~S, instead it must be a power of two!" +mem-box/min-words+))

  (defun %mem-float/inline? (type)
    (declare (type (member :float :double :sfloat :dfloat type)))
    (let ((size (msizeof type)))
      (<= (* size +mem-byte/bits+) +mem-pointer/bits+))))

(defmacro mem-float/inline? (type)
  (if (keywordp type)
      (%mem-float/inline? type)
      `(%mem-float/inline? ,type)))

(defconstant +mem-sfloat/inline?+ (mem-float/inline? :sfloat))
(defconstant +mem-dfloat/inline?+ (mem-float/inline? :dfloat))

(eval-always
 (set-feature 'hldb/sfloat/inline +mem-sfloat/inline?+)
 (set-feature 'hldb/dfloat/inline +mem-dfloat/inline?+))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      user-defined types      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-object/max-slots+  +most-positive-size+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hard-coded constants. DO NOT CHANGE!                                       ;;
;; They are read from and written to the store, so they are part of the ABI   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-unallocated+   0)
(defconstant +mem-unbound+       1 "persistent representation of unbound slot")
(defconstant +mem-nil+           2 "persistent representation of NIL")
(defconstant +mem-t+             3 "persistent representation of T")

(defconstant +mem-tag/int+              -1 "unboxed mem-int. It is a special case")
(defconstant +mem-tag/symbol+            0 "unboxed symbol or keyword")
(defconstant +mem-tag/character+         1 "unboxed character")
(defconstant +mem-tag/sfloat+            2 "unboxed single-float")
(defconstant +mem-tag/dfloat+            3 "unboxed double-float")
(defconstant +mem-tag/box+               4 "boxed value")

(defconstant +mem-box/unallocated+       0 "box is unallocated")

(defconstant +mem-box/bignum+            4 "box is a bignum") ;; intentionally eql +mem-tag/box+
(defconstant +mem-box/ratio+             5 "box is a ratio")
(defconstant +mem-box/sfloat+            6 "box is a single-float")
(defconstant +mem-box/dfloat+            7 "box is a double-float")
(defconstant +mem-box/complex-sfloat+    8 "box is a complex of single-floats")
(defconstant +mem-box/complex-dfloat+    9 "box is a complex of double-floats")
(defconstant +mem-box/complex-rational+ 10 "box is a complex of rationals")
(defconstant +mem-box/pathname+         11 "box is a pathname")
(defconstant +mem-box/hash-table+       12 "box is a hash-table")
(defconstant +mem-box/list+             13 "box is a cons or list")
(defconstant +mem-box/array+            14 "box is a N-dimensional array")
(defconstant +mem-box/vector+           15 "box is a 1-dimensional array, i.e. a vector")
(defconstant +mem-box/string+           16 "box is a string, i.e. a (vector character)")
(defconstant +mem-box/base-string+      17 "box is a base-string, i.e. a (vector base-char)")
(defconstant +mem-box/bit-vector+       18 "box is a bit-vector, i.e. a (vector bit)")

(defconstant +mem-box/first+            +mem-box/bignum+)
(defconstant +mem-box/last+             +mem-box/bit-vector+)

(defconstant +mem-user/first+           20 "first type tag available for user-defined types")
(defconstant +mem-user/last+            +mem-tag/mask+)



(declaim (type vector +mem-boxed-type-syms+))

(define-constant-once +mem-boxed-type-syms+
    #(bignum ratio sfloat dfloat
      complex-sfloat complex-dfloat complex-rational
      pathname hash-table list
      array vector string base-string bit-vector))


(deftype mem-box-type ()
  "Valid range for boxed-type tags"
  `(integer ,+mem-box/first+ ,+mem-box/last+))

