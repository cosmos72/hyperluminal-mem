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

;; if available, use fast-sap< and fast-sap+
(eval-always
  (let* ((fast-sap<    (get-fbound-symbol 'hl-asm 'fast-sap<))
         (fast-sap+    (get-fbound-symbol 'hl-asm 'fast-sap+))
         (fast-sap*    (the boolean (and fast-sap< fast-sap+ t))))

    (set-feature 'hlmem/fast-sap* fast-sap*)
    (if fast-sap*
        (progn
          (defmacro fast-sap< (x y)
            `(,fast-sap< ,x ,y))
          (defmacro fast-sap+ (ptr index &key (scale +msizeof-word+) (offset 0))
            `(,fast-sap+ ,ptr ,index :scale ,scale :offset ,offset)))
        ;; sanity
        (progn
          (fmakunbound 'fast-sap<)
          (fmakunbound 'fast-sap+)))))


;; if available, use fast-mread and fast-mwrite
(eval-always
  (let* ((fast-mread   (get-fbound-symbol 'hl-asm (stringify 'fast-mread/  +msizeof-word+)))
         (fast-mwrite  (get-fbound-symbol 'hl-asm (stringify 'fast-mwrite/ +msizeof-word+)))
         (fast-mem     (the boolean (and fast-mread fast-mwrite t))))

    (set-feature 'hlmem/fast-mem fast-mem)
    (if fast-mem
        (progn
          (defmacro fast-mget-word (ptr index &key (scale +msizeof-word+) (offset 0))
            `(,fast-mread ,ptr ,index :scale ,scale :offset ,offset))
          (defmacro fast-mset-word (value ptr index &key (scale +msizeof-word+) (offset 0))
            "Warning: returns no values"
            `(,fast-mwrite ,value ,ptr ,index :scale ,scale :offset ,offset)))
        ;; sanity
        (progn
          (fmakunbound 'fast-mget-word)
          (fmakunbound 'fast-mset-word)))))


;; use the fastest available implementation of mword=>mem-int
(eval-always
  (let ((sym (get-fbound-symbol 'hl-asm (stringify 'fast-mword/ +msizeof-word+ '=>fixnum))))
    
    ;; hl-asm:fast-mword=>fixnum is usable for mword=>mem-int
    ;; only if mem-int equals fixnum
    (set-feature 'hlmem/mword=>mem-int
                 ;; we store sym in the features!
                 (if (get-feature 'hlmem/mem-int=fixnum) sym nil))))
     

#?+(or hlmem/fast-mem hlmem/fast-sap*)
(deftype fast-sap () 'hl-asm:fast-sap)
      
#?+hlmem/fast-mem
(progn
  (defmacro sap=>fast-sap (x)
    `(hl-asm:sap=>fast-sap ,x))
  (defmacro fast-sap=>sap (x)
    `(hl-asm:fast-sap=>sap ,x)))


#?+hlmem/fast-sap*
(defmacro incf-fast-sap (place index &key (scale +msizeof-word+) (offset 0))
  (multiple-value-bind (temps vals stores store-form get-form)
      (get-setf-expansion place)
    `(let* (,@(loop for temp in temps
                 for val in vals
                 collect `(,temp ,val))
            (,(first stores) (fast-sap+ ,get-form ,index :scale ,scale :offset ,offset)))
       ,store-form)))


