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
     

;; if available, use fast-memcpy
(eval-always
  (let ((fast-memcpy  (get-fbound-symbol 'hl-asm (stringify 'fast-memcpy/  +msizeof-word+))))

    (set-feature 'hlmem/fast-memcpy fast-memcpy)
    (if fast-memcpy
        (defmacro fast-memcpy-words (dst dst-index src src-index n-words
                                     &key
                                       (dst-scale +msizeof-word+) (dst-offset 0)
                                       (src-scale +msizeof-word+) (src-offset 0))
          `(progn
             (,fast-memcpy ,dst ,dst-index ,src ,src-index ,n-words
                           :dst-scale ,dst-scale :dst-offset ,dst-offset
                           :src-scale ,src-scale :src-offset ,src-offset)
             nil))
        ;; sanity
        (fmakunbound 'fast-memcpy-words))))


;; if available, use fast-memset
(eval-always
  (let ((fast-memset  (get-fbound-symbol 'hl-asm (stringify 'fast-memset/ +msizeof-word+))))

    (set-feature 'hlmem/fast-memset fast-memset)
    (if fast-memset
        (defmacro fast-memset-words (ptr index n-words fill-word
                                     &key (scale +msizeof-word+) (offset 0))
          `(progn
             (,fast-memset ,ptr ,index ,n-words ,fill-word :scale ,scale :offset ,offset)
             nil))
        ;; sanity
        (fmakunbound 'fast-memset-words))))


#?+(or hlmem/fast-mem hlmem/fast-memcpy hlmem/fast-memset)
(deftype fast-sap () 'hl-asm:fast-sap)
      
#?+(or hlmem/fast-mem hlmem/fast-memcpy hlmem/fast-memset)
(progn
  (defmacro sap=>fast-sap (x)
    `(hl-asm:sap=>fast-sap ,x))
  (defmacro fast-sap=>sap (x)
    `(hl-asm:fast-sap=>sap ,x)))