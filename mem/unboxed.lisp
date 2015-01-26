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

(eval-always
  (when (< +most-positive-pointer+ +most-positive-character+)
    
    (error "cannot compile HYPERLUMINAL-MEM: assuming ~S-bit characters (i.e. Unicode),
    cannot fit them in the ~S bits reserved by ABI." +character/bits+ +mem-pointer/bits+)))



(deftype mem-int     () '(  signed-byte #.+mem-int/bits+))
(deftype mem-uint    () '(unsigned-byte #.+mem-int/value-bits+))

(deftype mem-word    () '(unsigned-byte #.+mem-word/bits+))
;; only used internally
(deftype mem-byte    () '(unsigned-byte #.+mem-byte/bits+))

(deftype mem-fulltag () '(unsigned-byte #.+mem-fulltag/bits+))
(deftype mem-tag     () '(unsigned-byte #.+mem-tag/bits+))

;; pointer offsets stored in MMAP area. To better use the available bits,
;; they are in units of the type pointed to, i.e. increasing them by 1
;; makes them point to the next object of the same type
(deftype mem-pointer () '(unsigned-byte #.+mem-pointer/bits+))

;; pointer offsets used internally by HYPERLUMINAL-MEM. They are in units of a CPU word,
;; so to convert from mem-pointer to mem-size you must multiply by the number of words
;; required to store the object pointed to.
(deftype mem-size    () '(unsigned-byte #.(- +mem-word/bits+ (integer-length (1- +msizeof-word+)))))


(deftype mem-unboxed-except-ratio-symbol () 
  "Union of all types (except ratio and symbol) that can be stored as unboxed in memory store"
  '(or mem-int character boolean
    #?+hldb/sfloat/inline single-float
    #?+hldb/dfloat/inline double-float))

(deftype mem-unboxed-except-ratio () 
  "Union of all types (except ratio) that can be stored as unboxed in memory store"
  '(or mem-int character boolean symbol
    #?+hldb/sfloat/inline single-float
    #?+hldb/dfloat/inline double-float))



(defun !mdump (stream ptr &optional (start-index 0) (end-index (1+ start-index)))
  "mdump is only used for debugging. it assumes sizeof(byte) == 1"
  (declare (type maddress ptr)
           (type mem-size start-index end-index))
  (loop
     for start-byte = (* +msizeof-word+ start-index) then end-byte
     for end-byte   = (+ +msizeof-word+ start-byte)
     while (<= end-byte (* +msizeof-word+ end-index))
     do
       (#.(if +mem/little-endian+
              '!mdump-bytes-reverse
              '!mdump-bytes)
          stream ptr start-byte end-byte)
       (format stream " ")))


(declaim (inline malloc-words))

(defun malloc-words (n-words)
  "Allocate N-WORDS words of raw memory and return it just like MALLOC.
Usually more handy than MALLOC since almost all Hyperluminal-MEM functions
count and expect memory lengths in words, not in bytes."
  (declare (type mem-size n-words))
  (malloc (* n-words +msizeof-word+)))
  

(declaim (inline mem-int+ mem-int-))

(defun mem-int+ (a &optional (b 0) (c 0))
  (declare (type mem-int a b c))
  (the mem-int (+ a b c)))

(defun mem-int- (a b)
  (declare (type mem-int a b))
  (the mem-int (- a b)))


(declaim (inline mem-size+ mem-size+1 mem-size+2 mem-size- mem-size-1 mem-size*))

(defun mem-size+ (a &optional (b 0) (c 0))
  (declare (type mem-size a b c))
  (the mem-size (+ a b c)))

(defun mem-size+1 (a)
  (mem-size+ a 1))

(defun mem-size+2 (a)
  (mem-size+ a 2))

(defun mem-size- (a b)
  (declare (type mem-size a b))
  (the mem-size (- a b)))

(defun mem-size-1 (a)
  (mem-size- a 1))


(defun mem-size* (a b)
  (declare (type mem-size a b))
  (the mem-size (* a b)))



(defmacro incf-mem-size (place &optional (delta 1))
  `(incf (the mem-size ,place) (the mem-size ,delta)))

(defmacro decf-mem-size (place &optional (delta 1))
  `(decf (the mem-size ,place) (the mem-size ,delta)))

    
  

(defmacro %to-fulltag (word)
  `(logand +mem-fulltag/mask+ (ash ,word ,(- +mem-fulltag/shift+))))

(defmacro %to-value (word)
  `(logand +mem-pointer/mask+ (ash ,word ,(- +mem-pointer/shift+))))

(defmacro %to-fulltag-and-value (val)
  (let ((word (gensym "WORD-")))
    `(let ((,word ,val))
       (values
        (%to-fulltag ,word)
        (%to-value   ,word)))))
       

(declaim (inline mem-invalid-index? mget-fulltag mget-value mget-fulltag-and-value))

(defun mem-invalid-index? (ptr index)
  (declare (ignore ptr)
           (type mem-size index))
  (zerop index))

(defun mget-fulltag (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-fulltag (mget-word ptr index)))

(defun mget-value (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-value (mget-word ptr index)))

(defun mget-fulltag-and-value (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-fulltag-and-value (mget-word ptr index)))

(defmacro bind-fulltag-and-value ((fulltag value) (ptr index) &body body)
  (let ((word (gensym "WORD-")))
    `(let* ((,word    (mget-word ,ptr ,index))
            (,fulltag (%to-fulltag ,word))
            (,value   (%to-value ,word)))
       ,@body)))



(declaim (inline mset-fulltag-and-value))
(defun mset-fulltag-and-value (ptr index fulltag value)
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-fulltag fulltag)
           (type mem-pointer value))

  (mset-word ptr index (logior
                        (ash fulltag +mem-fulltag/shift+)
                        (ash value   +mem-pointer/shift+)))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mset-int))
(defun mset-int (ptr index value)
  "Write mem-int VALUE into the memory at (PTR+INDEX)"
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-int value))

  (mset-word ptr index
	     (logior +mem-int/flag+
		     (logand +mem-int/mask+ value)))
  t)


(defmacro %to-int/sign (word)
  `(logand +mem-int/sign-mask+ ,word))

(defmacro %to-int/value (word)
  `(logand +mem-int/value-mask+ ,word))

(defmacro %to-int (word)
  (with-gensyms (word_ value sign)
    `(let* ((,word_ ,word)
            (,value (%to-int/value ,word_))
            (,sign  (%to-int/sign  ,word_)))
       (the mem-int (- ,value ,sign)))))
  

(declaim (inline mget-int/value mget-int))
(defun mget-int (ptr index)
  "Return the mem-int stored at (PTR+INDEX)"
  (declare (type maddress ptr)
           (type mem-size index))

  (%to-int (mget-word ptr index)))


(defsetf mget-int mset-int)


(defun mget-int/value (ptr index)
  "Return the two's complement value of mem-int stored at (PTR+INDEX),
ignoring any sign bit"
  (declare (type maddress ptr)
           (type mem-size index))

  (the mem-uint (%to-int/value (mget-word ptr index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mset-character mget-character))

(defun mset-character (ptr index value)
  (declare (type maddress ptr)
           (type mem-size index)
           (type character value))

  (mset-fulltag-and-value ptr index +mem-tag/character+ (char-code value)))


(defun mget-character (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))

  (code-char (%to-value (mget-word ptr index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mset-ratio mget-ratio))

(defmacro %ratio-to-word (numerator denominator)
  `(logior #.(ash +mem-tag/ratio+ +mem-fulltag/shift+)
           ;; keep one extra bit from numerator - captures its sign!
           (ash (logand ,numerator #.(1+ (* 2 +mem-ratio/numerator/mask+)))
                +mem-ratio/denominator/bits+)
           (1- ,denominator)))
     
  
(defun mset-ratio (ptr index ratio)
  (declare (type maddress ptr)
           (type mem-size index)
           (type ratio ratio))

  (let ((numerator (numerator ratio))
        (denominator (denominator ratio)))

    (declare (type (integer #.(lognot +mem-ratio/numerator/mask+) #.+mem-ratio/numerator/bits+)
                   numerator)
             (type (integer 1 #.(1+ +mem-ratio/denominator/mask+)) denominator))

    (mset-word ptr index (%ratio-to-word numerator denominator))
    t))


(defmacro %word-to-ratio (word)
  (with-gensyms (word_ denominator fulltag-and-numerator numerator sign-bit)
    `(let* ((,word_ ,word)
            (,denominator (1+ (logand ,word_ +mem-ratio/denominator/mask+)))
            (,fulltag-and-numerator (ash ,word_ #.(- +mem-ratio/denominator/bits+)))
            (,numerator   (logand ,fulltag-and-numerator +mem-ratio/numerator/mask+))
            (,sign-bit    (logand ,fulltag-and-numerator #.(1+ +mem-ratio/numerator/mask+))))
       (/ (- ,numerator ,sign-bit) ,denominator))))


(defun mget-ratio (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%word-to-ratio (mget-word ptr index)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mset-symbol-ref mget-symbol-ref))

(defun mset-symbol-ref (ptr index symbol-ref)
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-pointer symbol-ref))

  (mset-fulltag-and-value ptr index +mem-tag/symbol+ symbol-ref))


(defun mget-symbol-ref (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))

  (mget-value ptr index))


           


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro mget-float-0 (type ptr index)
  (declare (type (member :float :double :sfloat :dfloat) type))
  `(%mget-t ,type ,ptr (logand +mem-word/mask+ (* ,index +msizeof-word+))))

(defmacro mget-float-N (type ptr index)
  (declare (type (member :float :double :sfloat :dfloat) type))
  `(%mget-t ,type ,ptr (logand +mem-word/mask+
                               (+ ,(- +msizeof-word+ (msizeof type))
                                  (logand +mem-word/mask+
                                          (* ,index +msizeof-word+))))))

(defmacro mset-float-0 (type ptr index value)
  (declare (type (member :float :double :sfloat :dfloat) type))
  `(%mset-t ,value ,type ,ptr (logand +mem-word/mask+ (* ,index +msizeof-word+))))

(defmacro mset-float-N (type ptr index value)
  (declare (type (member :float :double :sfloat :dfloat) type))
  `(%mset-t ,value ,type ,ptr (logand +mem-word/mask+
                                       (+ ,(- +msizeof-word+ (msizeof type))
                                          (logand +mem-word/mask+
                                                  (* ,index +msizeof-word+))))))

(defmacro mget-float/inline (type ptr index)
  (declare (type (member :float :double :sfloat :dfloat) type))
  (if (mem-float/inline? type)
      (if +mem/little-endian+
          `(mget-float-0 ,type ,ptr ,index)
          `(mget-float-N ,type ,ptr ,index))
      `(error "HYPERLUMINAL-MEM: cannot use inline ~As on this architecture" ,(cffi-type-name type))))

(defmacro mset-float/inline (type ptr index value)
  (declare (type (member :float :double :sfloat :dfloat) type))
  (if (mem-float/inline? type)
      (if +mem/little-endian+
          `(mset-float-0 ,type ,ptr ,index ,value)
          `(mset-float-N ,type ,ptr ,index ,value))
      `(error "HYPERLUMINAL-MEM: cannot use inline ~As on this architecture" ,(cffi-type-name type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline is-unboxed?))

(defun is-unboxed? (value)
  "Return T if VALUE can be written to mmap memory as an unboxed value."
  (if (or (typep value 'mem-unboxed-except-ratio-symbol)
          ;; (eq value nil) ;; redundant
          ;; (eq value t) ;; redundant
          (eq value +unbound-tvar+)
          
          (and (typep value 'ratio)
               (let ((numerator (numerator value))
                     (denominator (denominator value)))
                 (and (typep numerator 'mem-int)
                      (typep denominator 'mem-int)
                      (<= #.(lognot +mem-ratio/numerator/mask+) numerator +mem-ratio/numerator/mask+)
                      (<= 1 denominator #.(1+ +mem-ratio/denominator/mask+)))))
          
          (gethash value +symbols-table+))
      t
      nil))
          

(defun !get-unbound-tvar ()
   +unbound-tvar+)

(defun !is-unbound-tvar? (value)
  (eq value +unbound-tvar+))


(define-constant-once +mem-unboxed-types+
    '(mem-int character base-char boolean
      #?+hldb/sfloat/inline single-float
      #?+hldb/dfloat/inline double-float))


(declaim (inline mset-unboxed))

(defun mset-unboxed (ptr index value)
  "Try to write an unboxed value to memory store. Supported types are:
boolean, unbound slots, character and medium-size integers
\(on 64bit architectures can also write single-floats).

Return T on success, or NIL if VALUE is a pointer or must be boxed."
  (declare (type maddress ptr)
           (type mem-size index))

  (let ((tag +mem-tag/symbol+)
        (val +mem-sym/nil+))

    (declare (type mem-tag tag)
             (type mem-pointer val))

    (cond
      ((typep value 'mem-int)
       (return-from mset-unboxed (mset-int ptr index value)))

      ;; value is a character?
      ((characterp value) (setf tag +mem-tag/character+
                                val (char-code value)))
      ;; value is NIL ?
      ((eq value nil))

      ;; value is T ?
      ((eq value t)       (setf val +mem-sym/t+))

      ;; value is +unbound-tvar+ ?
      ((eq value +unbound-tvar+) (setf val +mem-sym/unbound+))

      ;; value is a ratio?
      ((typep value 'ratio)
       (let ((numerator (numerator value))
             (denominator (denominator value)))

         (when (and (typep numerator 'mem-int)
                    (typep denominator 'mem-int)
                    (<= #.(lognot +mem-ratio/numerator/mask+) numerator +mem-ratio/numerator/mask+)
                    (<= 1 denominator #.(1+ +mem-ratio/denominator/mask+)))

           (mset-word ptr index (%ratio-to-word numerator denominator))
           (return-from mset-unboxed t))
         
         (return-from mset-unboxed nil)))

             
      ;; value is a single-float?
      #?+hldb/sfloat/inline
      ((typep value 'single-float)
       (mset-fulltag-and-value ptr index +mem-tag/sfloat+ 0)
       (mset-float/inline :sfloat ptr index value)
       (return-from mset-unboxed t))

      ;; value is a double-float?
      #?+hldb/dfloat/inline
      ((typep value 'double-float)
       (mset-fulltag-and-value ptr index +mem-tag/dfloat+ 0)
       (mset-float/inline :dfloat ptr index value)
       (return-from mset-unboxed t))

      (t 
       ;; value is a predefined symbol?
       (if-bind ref-val (gethash value +symbols-table+)
         (setf val ref-val)
         ;; default case: value cannot be be stored as unboxed type, return NIL
         ;; TODO: handle pointers
         (return-from mset-unboxed nil))))

    (mset-fulltag-and-value ptr index tag val)))

     
(declaim (inline mget-unboxed))

(defun mget-unboxed (ptr index)
  "Try to read an unboxed value (boolean, unbound slot, character or mem-int)
from memory store (on 64 bit architectures, also single-floats are unboxed)
and return it.

If memory contains a pointer or a boxed value, return their value and fulltag
as multiple values."
  (declare (type maddress ptr)
           (type mem-size index))

  (let ((word (mget-word ptr index)))

    ;; found a mem-int?
    (if (zerop #+sbcl (logand word +mem-int/flag+) ;; sbcl optimizes this (word is not a fixnum)
               #-sbcl (ash word (- +mem-int/bits+)))

        ;; not a mem-int
        (let ((fulltag (%to-fulltag word))
              (value   (%to-value   word)))

          (case fulltag
            (#.+mem-tag/symbol+ ;; found a symbol

             (case word
               (#.+mem-sym/nil+     nil)
               (#.+mem-sym/t+       t)
               (#.+mem-sym/unbound+ +unbound-tvar+) ;; unbound slot
               (otherwise
                (if (<= +mem-syms/first+ value +mem-syms/last+)
                    ;; predefined symbol
                    (svref +symbols-vector+ (- value +mem-syms/first+))
                    ;; user-defined symbol... not yet implemented
                    (values value fulltag)))))

            (#.+mem-tag/character+ ;; found a character
             (code-char (logand value +character/mask+)))

            ((#.+mem-tag/ratio+ #.+mem-tag/neg-ratio+) ;; found a ratio
             (%word-to-ratio word))

            #?+hldb/sfloat/inline
            (#.+mem-tag/sfloat+ ;; found a single-float
             (mget-float/inline :sfloat ptr index))

            #?+hldb/dfloat/inline
            (#.+mem-tag/dfloat+ ;; found a double-float
             (mget-float/inline :dfloat ptr index))

            (otherwise ;; found a boxed value or a pointer
             (values value fulltag))))

        ;; found a mem-int
        (%to-int word))))



  
