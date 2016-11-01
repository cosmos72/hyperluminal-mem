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
  (when (< +most-positive-vid+ +most-positive-character+)
    
    (error "cannot compile HYPERLUMINAL-MEM: assuming ~S-bit characters (i.e. Unicode),
    cannot fit them in the ~S bits reserved by ABI." +character/bits+ +mem-vid/bits+)))


;; only used internally
(deftype mem-byte    () `(unsigned-byte ,+mem-byte/bits+))

(deftype mem-vid     () `(integer 0 ,+most-positive-vid+))
(deftype mem-tag     () `(integer 0 ,+most-positive-tag+))

(deftype mem-box-type ()
  "Valid range for boxed-type tags"
  `(integer ,+mem-box/first+ ,+mem-box/last+))

(deftype mem-obj-type ()
  "Valid range for boxed-type or object tags"
  `(integer ,+mem-box/first+ ,+mem-obj+))


;; pointer offsets used internally by HYPERLUMINAL-MEM. They are in units of a CPU word,
(deftype mem-size    ()
  "Type for offsets and sizes of serialized data. It is in units of a mem-word,
i.e. 1 means one mem-word."
  `(unsigned-byte ,+mem-size/bits+))
                            
                              


(deftype mem-unboxed-except-ratio-symbol () 
  "Union of all types (except ratio and symbol) that can be stored as unboxed in memory store"
  '(or mem-int character boolean
    #?+hlmem/sfloat/inline single-float
    #?+hlmem/dfloat/inline double-float))

(deftype mem-unboxed-except-ratio () 
  "Union of all types (except ratio) that can be stored as unboxed in memory store"
  '(or mem-int character boolean symbol
    #?+hlmem/sfloat/inline single-float
    #?+hlmem/dfloat/inline double-float))



(declaim (inline mem-int+ mem-int-))

(defun mem-int+ (a &optional (b 0) (c 0))
  (declare (type mem-int a b c))
  (the mem-int (+ a b c)))

(defun mem-int- (a b)
  (declare (type mem-int a b))
  (the mem-int (- a b)))


(declaim (inline mem-size+))
(defun mem-size+ (a &optional (b 0) (c 0))
  (declare (type mem-size a b c))
  (the mem-size (+ a b c)))

(declaim (inline mem-size+1))
(defun mem-size+1 (a)
  (mem-size+ a 1))

(declaim (inline mem-size+2))
(defun mem-size+2 (a)
  (mem-size+ a 2))

(declaim (inline mem-size-))
(defun mem-size- (a b)
  (declare (type mem-size a b))
  (the mem-size (- a b)))

(declaim (inline mem-size-1))
(defun mem-size-1 (a)
  (mem-size- a 1))

(declaim (inline mem-size*))
(defun mem-size* (a b)
  (declare (type mem-size a b))
  (the mem-size (* a b)))



(defmacro incf-mem-size (place &optional (delta 1))
  `(incf (the mem-size ,place) (the mem-size ,delta)))

(defmacro decf-mem-size (place &optional (delta 1))
  `(decf (the mem-size ,place) (the mem-size ,delta)))

    
  
(defun !mdump (stream ptr &optional (start-index 0) (end-index (1+ start-index)))
  "dump the contents of raw memory. useful for debugging"
  (declare (type maddress ptr)
           (type mem-size start-index end-index))
  (loop while (< start-index end-index)
     do
       (format stream #.(format nil "~~~A,'0X " (* 2 +msizeof-word+))
               (mget-word ptr start-index))
       (incf-mem-size start-index)))


;; function versions of mget-word and mset-word macros, only used for debugging.
;; Useful because SBCL VOPs hlm-sbcl:%fast-mread/N and hlm-sbcl:%fast-mwrite/N
;; cannot be invoked from interpreted code (i.e. REPL).
;; They depend on mem-size, so they can be defined only down here.

(declaim (inline mread-word))
(defun mread-word (ptr word-index)
  (declare (optimize (space 0) (compilation-speed 0) (safety 0) (debug 1) (speed 3))
           (type maddress ptr)
           (type mem-size word-index))
  (the mem-word (mget-word ptr word-index)))

(declaim (inline mwrite-word))
(defun mwrite-word (ptr word-index value)
  (declare (optimize (space 0) (compilation-speed 0) (safety 0) (debug 1) (speed 3))
           (type maddress ptr)
           (type mem-size word-index)
           (type mem-word value))
  (mset-word ptr word-index value)
  ;; fast-mset-word returns no values!
  value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %to-tag (word)
  `(logand +mem-tag/mask+ (ash ,word ,(- +mem-tag/shift+))))

(defmacro %to-vid (word)
  `(logand +mem-vid/mask+ (ash ,word ,(- +mem-vid/shift+))))

(defmacro %to-tag-and-vid (val)
  (let ((word (gensym "WORD-")))
    `(let ((,word ,val))
       (values
        (%to-tag ,word)
        (%to-vid ,word)))))
       

(declaim (inline mem-invalid-index?))
(defun mem-invalid-index? (ptr index)
  (declare (ignore ptr)
           (type mem-size index))
  (zerop index))

(declaim (inline mget-tag))
(defun mget-tag (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-tag (mget-word ptr index)))

(declaim (inline mget-vid))
(defun mget-vid (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-vid (mget-word ptr index)))

(declaim (inline mget-tag-and-vid))
(defun mget-tag-and-vid (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%to-tag-and-vid (mget-word ptr index)))

(defmacro with-tag-and-vid ((tag vid) (ptr index) &body body)
  (let ((word (gensym "WORD-")))
    `(let* ((,word (mget-word ,ptr ,index))
            (,tag  (%to-tag ,word))
            (,vid  (%to-vid ,word)))
       ,@body)))



(declaim (inline mset-tag-and-vid))
(defun mset-tag-and-vid (ptr index tag vid)
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-tag tag)
           (type mem-vid vid))

  (mset-word ptr index (logior
                        (ash tag +mem-tag/shift+)
                        (ash vid +mem-vid/shift+)))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mset-character))
(defun mset-character (ptr index value)
  (declare (type maddress ptr)
           (type mem-size index)
           (type character value))

  (mset-tag-and-vid ptr index +mem-tag/character+ (char-code value)))


(defmacro %to-character (word)
  `(logand +character/mask+ (ash ,word ,(- +mem-vid/shift+))))

(declaim (inline mget-character))
(defun mget-character (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))

  (code-char (%to-character (mget-word ptr index))))


(defsetf mget-character mset-character)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %ratio-to-word (numerator denominator)
  `(logior #.(ash +mem-tag/ratio+ +mem-tag/shift+)
           ;; keep one extra bit from numerator - captures its sign!
           (ash (logand ,numerator #.(1+ (* 2 +mem-ratio/numerator/mask+)))
                +mem-ratio/denominator/bits+)
           (1- ,denominator)))
     
  
(declaim (inline mset-ratio))
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

(declaim (inline mget-ratio))
(defun mget-ratio (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (%word-to-ratio (mget-word ptr index)))

(defsetf mget-ratio mset-ratio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline mset-symbol-ref))
(defun mset-symbol-ref (ptr index symbol-ref)
  (declare (type maddress ptr)
           (type mem-size index)
           (type mem-vid symbol-ref))

  (mset-tag-and-vid ptr index +mem-tag/symbol+ symbol-ref))

(declaim (inline mget-symbol-ref))
(defun mget-symbol-ref (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))

  (mget-vid ptr index))

(defsetf mget-symbol-ref mset-symbol-ref)
           


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
      (if (eql +mem/chosen-endianity+ :little-endian)
          `(mget-float-0 ,type ,ptr ,index)
          `(mget-float-N ,type ,ptr ,index))
      `(error "HYPERLUMINAL-MEM: cannot use inline ~As on this architecture" ,(cffi-type-name type))))

(defmacro mset-float/inline (type ptr index value)
  (declare (type (member :float :double :sfloat :dfloat) type))
  (if (mem-float/inline? type)
      (if (eql +mem/chosen-endianity+ :little-endian)
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
          ;; (eq value +stmx-unbound-tvar+) ;; redundant
          
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
      #?+hlmem/sfloat/inline single-float
      #?+hlmem/dfloat/inline double-float))


(declaim (inline mset-unboxed))
(defun mset-unboxed (ptr index value)
  "Try to write an unboxed value to memory store. Supported types are:
boolean, unbound slots, character and medium-size integers
\(on 64bit architectures can also write single-floats).

Return T on success, or NIL if VALUE is a pointer or must be boxed."
  (declare (type maddress ptr)
           (type mem-size index)
           (optimize (safety 0) (speed 3)))

  (let ((tag +mem-tag/symbol+)
        (vid +mem-sym/nil+))

    (declare (type mem-tag tag)
             (type mem-vid vid))

    (cond
      ;; value is a mem-int?
      ;; if mem-int is larger than fixnum, check value against fixnum first
      ;; because it's faster and more used
      #?+(or hlmem/mem-int>fixnum hlmem/mem-int=fixnum)
      ((typep value 'fixnum)
       (return-from mset-unboxed (mset-int ptr index (the fixnum value))))

      ;; value is a character?
      ((characterp value) (setf tag +mem-tag/character+
                                vid (char-code value)))
      ;; value is NIL ?
      ((eq value nil))

      ;; value is T ?
      ((eq value t)       (setf vid +mem-sym/t+))

      ;; value is +unbound-tvar+ ?
      ((eq value +unbound-tvar+) (setf vid +mem-sym/unbound+))

      ;; value is a mem-int?
      ;; if mem-int is different from fixnum, check value against mem-int
      ;; after fixnum, because it's slower
      #?-hlmem/mem-int=fixnum
      ((typep value 'mem-int)
       (return-from mset-unboxed (mset-int ptr index value)))
       

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
      #?+hlmem/sfloat/inline
      ((typep value 'single-float)
       (mset-tag-and-vid ptr index +mem-tag/sfloat+ 0)
       (mset-float/inline :sfloat ptr index value)
       (return-from mset-unboxed t))

      ;; value is a double-float?
      #?+hlmem/dfloat/inline
      ((typep value 'double-float)
       (mset-tag-and-vid ptr index +mem-tag/dfloat+ 0)
       (mset-float/inline :dfloat ptr index value)
       (return-from mset-unboxed t))

      (t 
       ;; value is a predefined symbol?
       (let ((ref-vid (gethash value +symbols-table+)))
         (if ref-vid
             (setf vid ref-vid)
             ;; default case: value cannot be be stored as unboxed type, return NIL
             ;; TODO: handle pointers
             (return-from mset-unboxed nil)))))

    (mset-tag-and-vid ptr index tag vid)))


(defmacro %word-is-mem-int (word)
  (if (= +mem-word/bits+ (1+ +mem-int/bits+))
      `(not
        (zerop #+sbcl (logand ,word +mem-int/flag+) ;; sbcl optimizes this (word is not a fixnum)
               #-sbcl (ash ,word ,(- +mem-int/bits+))))
      #+sbcl
      `(= +mem-int/flag+ (logand ,word +mem-int/flag+))
      #-sbcl
      `(= ,(ash +mem-int/flag+ (- +mem-int/bits+))
           (ash ,word ,(- +mem-int/bits+)))))
      
        
(declaim (inline mget-unboxed))
(defun mget-unboxed (ptr index)
  "Try to read an unboxed value (boolean, unbound slot, character or mem-int)
from memory store (on 64 bit architectures, also single-floats are unboxed)
and return it.

If memory contains a pointer or a boxed value, return their value and fulltag
as multiple values."
  (declare (type maddress ptr)
           (type mem-size index)
           (optimize (safety 0) (speed 3)))

  (let ((word (mget-word ptr index)))

    ;; found a mem-int?
    (when (%word-is-mem-int word)
      ;; found a mem-int
      (return-from mget-unboxed (mword=>mem-int word)))


    ;; not a mem-int
    (let ((tag (%to-tag word))
          (vid (%to-vid word)))

      (case tag
        (#.+mem-tag/symbol+ ;; found a symbol

         (case word
           (#.+mem-sym/nil+     nil)
           (#.+mem-sym/t+       t)
           (#.+mem-sym/unbound+ +unbound-tvar+) ;; unbound slot
           (otherwise
            (if (<= +mem-syms/first+ vid +mem-syms/last+)
                ;; predefined symbol
                (svref +symbols-vector+ (- vid +mem-syms/first+))
                ;; user-defined symbol... not yet implemented
                (values vid tag)))))

        (#.+mem-tag/character+ ;; found a character
         (code-char (logand vid +character/mask+)))

        ((#.+mem-tag/ratio+ #.+mem-tag/neg-ratio+) ;; found a ratio
         (%word-to-ratio word))

        #?+hlmem/sfloat/inline
        (#.+mem-tag/sfloat+ ;; found a single-float
         (mget-float/inline :sfloat ptr index))

        #?+hlmem/dfloat/inline
        (#.+mem-tag/dfloat+ ;; found a double-float
         (mget-float/inline :dfloat ptr index))

        (otherwise ;; found a boxed value or a pointer
         (values vid tag))))))




  
