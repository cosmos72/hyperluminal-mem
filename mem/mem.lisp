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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; if available, use fast-mread and fast-mwrite
;;
;; fast-mword=>mem-int must be defined later (in int.lisp)
;; because it needs #?+hlmem/mem-int=fixnum computed in constants.lisp
(eval-always
  (let* ((fast-mread   (get-fbound-symbol 'hlm-asm (stringify 'fast-mread/  +msizeof-word+)))
         (fast-mwrite  (get-fbound-symbol 'hlm-asm (stringify 'fast-mwrite/ +msizeof-word+)))
         (fast-mem     (and fast-mread fast-mwrite)))

    (set-feature :hlmem/fast-mem (not (null fast-mem)))
    (if fast-mem
        (progn
          (defmacro fast-mget-word/native-endianity (ptr index
                                                     &key (scale +msizeof-word+) (offset 0))
            `(,fast-mread ,ptr ,index :scale ,scale :offset ,offset))
          (defmacro fast-mset-word/native-endianity (value ptr index
                                                     &key (scale +msizeof-word+) (offset 0))
            "Warning: returns no values"
            `(,fast-mwrite ,value ,ptr ,index :scale ,scale :offset ,offset))

          ;; honour +mem/chosen-endianity+ !
          (defmacro fast-mget-word (ptr index &key (scale +msizeof-word+) (offset 0))
            `(maybe-invert-endianity/integer :word
              (,fast-mread ,ptr ,index :scale ,scale :offset ,offset)))
          (defmacro fast-mset-word (value ptr index &key (scale +msizeof-word+) (offset 0))
            "Warning: returns no values"
            `(,fast-mwrite (maybe-invert-endianity/integer :word ,value)
                           ,ptr ,index :scale ,scale :offset ,offset)))
        ;; sanity
        (progn
          (fmakunbound 'fast-mget-word/native-endianity)
          (fmakunbound 'fast-mset-word/native-endianity)
          (fmakunbound 'fast-mget-word)
          (fmakunbound 'fast-mset-word)))))


;; if available, use fast-memcpy
(eval-always
  (let ((fast-memcpy  (get-fbound-symbol 'hlm-asm (stringify 'fast-memcpy/  +msizeof-word+))))

    (set-feature :hlmem/fast-memcpy (not (null fast-memcpy)))
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
  (let ((fast-memset  (get-fbound-symbol 'hlm-asm (stringify 'fast-memset/ +msizeof-word+))))

    (set-feature :hlmem/fast-memset (not (null fast-memset)))
    (if fast-memset
        (defmacro fast-memset-words (ptr index n-words fill-word
                                     &key (scale +msizeof-word+) (offset 0))
          `(progn
             ;; honour +mem/chosen-endianity+ !
             (,fast-memset ,ptr ,index ,n-words
                           (maybe-invert-endianity/integer :word ,fill-word)
                           :scale ,scale :offset ,offset)
             nil))
        ;; sanity
        (fmakunbound 'fast-memset-words))))



#?+(or hlmem/fast-mem hlmem/fast-memcpy hlmem/fast-memset)
(let ((fast-sym (get-symbol 'hlm-asm (stringify 'fast-sap/ +msizeof-word+)
			    :errorp t))
      (to-fast-sym (get-symbol 'hlm-asm (stringify 'sap=>fast-sap/ +msizeof-word+)
			       :errorp t)))
  (deftype fast-sap () fast-sym)
  (defmacro sap=>fast-sap (x)
    `(,to-fast-sym ,x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-always
  (defmacro mget-t/native-endianity (type ptr word-index)
    #?+hlmem/fast-mem
    (when (eq +chosen-word-type+ (parse-type type))
      (return-from mget-t/native-endianity
        `(fast-mget-word/native-endianity (sap=>fast-sap ,ptr) ,word-index)))
    ;; common case
    `(%mget-t ,type ,ptr (the #+sbcl mem-word #-sbcl t
                              (* ,word-index +msizeof-word+))))
  (defmacro mset-t/native-endianity (value type ptr word-index)
    #?+hlmem/fast-mem
    (when (eq +chosen-word-type+ (parse-type type))
      (return-from mset-t/native-endianity
        `(fast-mset-word/native-endianity ,value (sap=>fast-sap ,ptr) ,word-index)))
    ;; common case
    `(%mset-t ,value ,type ,ptr (the #+sbcl mem-word #-sbcl t
                                     (* ,word-index +msizeof-word+))))

  (defmacro mget-t (type ptr word-index)
    (let ((type (parse-type type))
          (byte-offset `(the #+sbcl mem-word #-sbcl t
                             (* ,word-index +msizeof-word+))))
      (case type
        (#.+sfloat-type+ `(mget-sfloat ,ptr ,byte-offset))
        (#.+dfloat-type+ `(mget-dfloat ,ptr ,byte-offset))
        (otherwise
         `(maybe-invert-endianity/integer
           ,type (mget-t/native-endianity ,type ,ptr ,word-index))))))
  (defmacro mset-t (value type ptr word-index)
    (let ((type (parse-type type))
          (byte-offset `(the #+sbcl mem-word #-sbcl t
                             (* ,word-index +msizeof-word+))))
      (case type
        (#.+sfloat-type+ `(mset-sfloat ,value ,ptr ,byte-offset))
        (#.+dfloat-type+ `(mset-dfloat ,value ,ptr ,byte-offset))
        (otherwise
         `(mset-t/native-endianity
           (maybe-invert-endianity/integer ,type ,value) ,type ,ptr ,word-index))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-always
  (defmacro mget-word/native-endianity (ptr word-index)
    `(mget-t/native-endianity :word ,ptr ,word-index))
  (defmacro mset-word/native-endianity (ptr word-index value)
    "Warning: evaluates VALUE before the other arguments!"
    `(mset-t/native-endianity ,value :word ,ptr ,word-index))
  (defsetf mget-word/native-endianity mset-word/native-endianity)

  (defmacro mget-word (ptr word-index)
    `(mget-t :word ,ptr ,word-index))
  (defmacro mset-word (ptr word-index value)
    "Warning: evaluates VALUE before the other arguments!"
    `(mset-t ,value :word ,ptr ,word-index))
  (defsetf mget-word mset-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

