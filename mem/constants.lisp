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

(enable-#?-syntax)

;; ABI definition. The values are tuned for good balance
;; between performance and size of addressable store


;; 5 or 6 bits reserved for type tags,
;; the rest of each CPU word is used for tagged values i.e. vids
(eval-always
  (defconstant +mem-tag/bits+
    (if (<= +mem-word/bits+ 32) 5 6)))

(eval-always
  (defconstant +mem-vid/bits+ (- +mem-word/bits+ +mem-tag/bits+)))

;; integers (actually, mem-int) are one bit less than CPU words
;; EXCEPTION:
;; when using 32-bit ABI, they are two bits less than CPU words
;; in order to match the fixnums of 32-bit SBCL and CCL 
(eval-always
  (defconstant +mem-int/bits+
    (- +mem-word/bits+ (if (<= +mem-word/bits+ 32) 2 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NO USER-SERVICEABLE PARTS BELOW THIS LINE.                                 ;;
;; I.e. everything else is computed from the CPU architecture                 ;;
;; (see (defmacro parse-type) in mem.lisp) and the constants above            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-always
  ;; sanity checks

  (when (< +mem-tag/bits+ 5)
    (error "HYPERLUMINAL-MEM compile error.
  Invalid ~S = ~S, must be at least 5.
  Please fix the customization in \"constants.lisp\" before recompiling."
           '+mem-tag/bits+ +mem-tag/bits+))

  (unless (< +mem-int/bits+ +mem-word/bits+)
    (error "HYPERLUMINAL-MEM compile error.
  Invalid ~S = ~S, must be smaller than ~S = ~S.
  Please fix the customization in \"constants.lisp\" before recompiling."
           '+mem-int/bits+ +mem-int/bits+ '+mem-word/bits+ +mem-word/bits+))

  (when (and (< +mem-tag/bits+ 6)
             (= +mem-int/bits+ (1- +mem-word/bits+)))
    (error "HYPERLUMINAL-MEM compile error.
  Invalid combination ~S = ~S, ~S = ~S.
  When ~S is (1- ~S) as in this case, ~S must be at least 6.
  Please fix the customization in \"constants.lisp\" before recompiling."
           '+mem-int/bits+ +mem-int/bits+ '+mem-tag/bits+ +mem-tag/bits+
           '+mem-int/bits+ '+mem-word/bits+ '+mem-tag/bits+)))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    integers, i.e. mem-int    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-always
  (defconstant +mem-int/mask+          (1- (ash 1 +mem-int/bits+))))
(eval-always
  ;; bits in a word that exceed a mem-int. if all set to one,
  ;; it means the rest of the word must be interpreted as a mem-int
  (defconstant +mem-int/flag+          (- +mem-word/mask+ +mem-int/mask+)))

(eval-always
  ;; values are the lowest bits in a mem-int
  (defconstant +mem-int/value-bits+    (1-  +mem-int/bits+)))
(eval-always
  (defconstant +mem-int/value-mask+    (ash +mem-int/mask+ -1)))
(eval-always
  ;; sign is the top bit in a mem-int
  (defconstant +mem-int/sign-bits+     1))
(eval-always
  (defconstant +mem-int/sign-shift+    +mem-int/value-bits+))
(eval-always
  (defconstant +mem-int/sign-mask+     (1+ +mem-int/value-mask+)))

(eval-always
  (defconstant +most-positive-int+     (ash +mem-int/mask+ -1)))
(eval-always
  (defconstant +most-negative-int+     (lognot +most-positive-int+)))

(eval-always
  (defconstant +mem-int>fixnum+        (< +most-negative-int+ most-negative-fixnum
                                          most-positive-fixnum +most-positive-int+)))
(eval-always
  (defconstant +mem-int=fixnum+        (and (= most-negative-fixnum +most-negative-int+)
                                            (= most-positive-fixnum +most-positive-int+))))

(eval-always
  (set-features `(hlmem/mem-int>fixnum ,+mem-int>fixnum+)
                `(hlmem/mem-int=fixnum ,+mem-int=fixnum+)))
                
                

(declaim (inline mem-int=integer-type mem-int>integer-type))

(defun mem-int=integer-type (type)
  (declare (type (or symbol cons) type))
  (or
   #?+hlmem/mem-int=fixnum (eq type 'fixnum)
   (and (consp type)
        (eq  'signed-byte (first type))
        (eql +mem-int/bits+ (second type)))))
        

(defun mem-int>integer-type (type)
  (declare (type (or symbol cons) type))
  (or
   #?+hlmem/mem-int>fixnum (eq type 'fixnum)
   (and (consp type)
        (member (first type) '(signed-byte unsigned-byte))
        (> +mem-int/bits+ (the fixnum (second type))))))

    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; type tags and vids ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-always
  (defconstant +mem-tag/shift+     (- +mem-word/bits+ +mem-tag/bits+)))
(eval-always
  (defconstant +mem-tag/mask+      (1- (ash 1 +mem-tag/bits+))))

;; reserve the top fulltag values to mark integers:
;; fulltags larger than +most-positive-tag+ indicate an integer (actually, mem-int) value
(eval-always
  (defconstant +most-positive-tag+ (1- (ash +mem-int/flag+ (- +mem-tag/shift+)))))


;; serialized tagged values. To better use the available bits,
;; in case they represent raw memory pointer offsets,
;; they are in units of the type pointed to, i.e. increasing them by 1
;; makes them point to the next object of the same type
(eval-always
  (defconstant +mem-vid/shift+     0))
(eval-always
  (defconstant +mem-vid/mask+      (1- (ash 1 +mem-vid/bits+))))
(eval-always
  (defconstant +most-positive-vid+ +mem-vid/mask+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mem-size, i.e. raw memory lengths and offsets ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mem-size are used internally by HYPERLUMINAL-MEM for raw memory lengths and pointer offsets.
;; They are in units of a CPU word, so to convert from mem-vid to mem-size
;; you must multiply by the number of words required to store the object pointed to.
(eval-always
  (defconstant +mem-size/bits+
    ;; optimization for 32-bit SBCL and CCL, where fixnum is (signed-byte 30)
    ;; and mem-size would be (unsigned-byte 30), causing a lot of bignum allocations:
    ;; define mem-size as (unsigned-byte 29), so that it fits a fixnum
    (let ((bits (- +mem-word/bits+ (integer-length (1- +msizeof-word+))))
          (ufixnum-bits (integer-length most-positive-fixnum)))
      (if (<= 1 (- bits ufixnum-bits) 2)
          ufixnum-bits
          bits))))
          
(eval-always
  (defconstant +mem-size/mask+         (1- (ash 1 +mem-size/bits+))))
(eval-always
  (defconstant +most-positive-size+    +mem-size/mask+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    ratios, i.e. mem-ratio    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-ratio/bits+             +mem-vid/bits+)
(defconstant +mem-ratio/denominator/bits+ (ash +mem-ratio/bits+ -1))
(defconstant +mem-ratio/denominator/mask+ (1- (ash 1 +mem-ratio/denominator/bits+)))
(defconstant +mem-ratio/numerator/bits+   (- +mem-vid/bits+ +mem-ratio/denominator/bits+))
(defconstant +mem-ratio/numerator/mask+   (1- (ash 1 +mem-ratio/numerator/bits+)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; single-float and double-foat ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-always
  (defconstant +mem-sfloat/bits+ (* +msizeof-sfloat+ +mem-byte/bits+)))
(eval-always
  (defconstant +mem-dfloat/bits+ (* +msizeof-dfloat+ +mem-byte/bits+)))

(eval-always

  (defun %mem-float/inline? (type)
    (declare (type (member :float :double :sfloat :dfloat type)))
    (let ((size (msizeof type)))
      (<= (* size +mem-byte/bits+) +mem-vid/bits+)))

  (defmacro mem-float/inline? (type)
    (if (keywordp type)
        (%mem-float/inline? type)
        `(%mem-float/inline? ,type))))

(eval-always
  (defconstant +mem-sfloat/inline?+ (mem-float/inline? :sfloat))
  (defconstant +mem-dfloat/inline?+ (mem-float/inline? :dfloat)))

(eval-always
 (set-feature 'hlmem/sfloat/inline +mem-sfloat/inline?+)
 (set-feature 'hlmem/dfloat/inline +mem-dfloat/inline?+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        boxed values          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-always
  (defconstant +mem-box/min-words+    4 "boxed values are allocated
    in multiples of 4 CPU-words. This value must be a power of two."))

(eval-always
  (defconstant +mem-box/max-words+    (min
                                       +most-positive-size+
                                       (* (1+ +most-positive-vid+) +mem-box/min-words+)))
  (defconstant +mem-box/header-words+ 1 "boxed values have a 1 CPU-word header"))

(eval-always
  (defconstant +mem-box/min-payload-words+ (- +mem-box/min-words+ +mem-box/header-words+))
  (defconstant +mem-box/max-payload-words+ (- +mem-box/max-words+ +mem-box/header-words+)))

(eval-always
  (unless (zerop (logand +mem-box/min-words+ (1- +mem-box/min-words+)))
    (error "+mem-box/min-words+ is ~S, instead it must be a power of two!" +mem-box/min-words+))

  (set-feature 'hlmem/box/header-words +mem-box/header-words+)
  (set-feature 'hlmem/box/min-words    +mem-box/min-words+))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   bignums, i.e. mem-bignum   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-bignum/max-words+ (min +most-positive-int+
					 ;; -1 for length header
                                         (1- +mem-box/max-payload-words+))
  "Maximum number of CPU words in a mem-bignum.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      user-defined types      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-object/max-slots+  +most-positive-size+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hard-coded constants. DO NOT CHANGE!                                       ;;
;; They are read from and written to the store, so they are part of the ABI   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +mem-unallocated+          0 "unallocated memory")

(defconstant +mem-sym/nil+              0 "persistent representation of NIL")
(defconstant +mem-sym/t+                1 "persistent representation of T")
(defconstant +mem-sym/unbound+          2 "persistent representation of unbound slot")


(defconstant +mem-tag/int+              -1 "unboxed mem-int i.e. small integer. Special case")

(defconstant +mem-tag/ref+               0 "unboxed reference to symbol, keyword, package, class...")
(defconstant +mem-tag/symbol+            +mem-tag/ref+ "alias for +MEM-TAG/REF+")
(defconstant +mem-tag/package+           +mem-tag/ref+ "alias for +MEM-TAG/REF+")

(defconstant +mem-tag/character+         1 "unboxed character")
(defconstant +mem-tag/ratio+             2 "unboxed, unsigned ratio. must be even.")
(defconstant +mem-tag/neg-ratio+         3 "unboxed, negative ratio. must be (1+ +mem-tag/ratio+)")
(defconstant +mem-tag/sfloat+            4 "unboxed single-float")
(defconstant +mem-tag/dfloat+            5 "unboxed double-float")
(defconstant +mem-tag/box+               6 "boxed value")

(defconstant +mem-box/unallocated+       0 "box is unallocated")

(eval-always
  (defconstant +mem-box/bignum+          6 "box is a bignum")) ;; intentionally eql +mem-tag/box+
(defconstant +mem-box/ratio+             7 "box is a ratio")
(defconstant +mem-box/sfloat+            8 "box is a single-float")
(defconstant +mem-box/dfloat+            9 "box is a double-float")
(defconstant +mem-box/complex-sfloat+   10 "box is a complex of single-floats")
(defconstant +mem-box/complex-dfloat+   11 "box is a complex of double-floats")
(defconstant +mem-box/complex-rational+ 12 "box is a complex of rationals")
(defconstant +mem-box/pathname+         13 "box is a pathname")
(defconstant +mem-box/hash-table+       14 "box is a hash-table")
(defconstant +mem-box/list+             15 "box is a cons or list")
(defconstant +mem-box/array+            16 "box is a N-dimensional array")
(defconstant +mem-box/vector+           17 "box is a 1-dimensional array, i.e. a vector")

(defconstant +mem-box/string-utf-21+    18 "box is a string, i.e. a (vector character)")
(defconstant +mem-box/string-utf-8+     19 "box is a string, i.e. a (vector character)")
(defconstant +mem-box/string+           +mem-box/string-utf-8+ "default string format is UTF-8")

(defconstant +mem-box/ascii-string+     20 "box is an ASCII string")
(defconstant +mem-box/bit-vector+       21 "box is a bit-vector, i.e. a (vector bit)")
(eval-always
  (defconstant +mem-box/symbol+           22 "object is a symbol or keyword")
  (defconstant +mem-box/first+            +mem-box/bignum+)
  (defconstant +mem-box/last+             +mem-box/symbol+)

  (let ((obj-tag (1+ +mem-box/symbol+)))
    (when (< +most-positive-tag+ obj-tag)
      (error "HYPERLUMINAL-MEM compile error:
  ~S is too small! found ~S, minimum supported is ~S
  Please increase ~S before recompiling"
             '+most-positive-tag+ +most-positive-tag+ obj-tag +mem-tag/bits+))

    (defconstant +mem-obj+ obj-tag "type tag for user-defined objects or structs")))



(declaim (type vector +mem-boxed-type-syms+))
(define-constant-once +mem-boxed-type-syms+
    ;; these MUST match the constants defined above!!
    #(bignum ratio sfloat dfloat
      complex-sfloat complex-dfloat complex-rational
      pathname hash-table list
      array vector string-utf-21 string-utf-8 ascii-string bit-vector symbol))


