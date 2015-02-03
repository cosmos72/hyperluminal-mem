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


;;;; boxed values, i.e. mem-box, are variable-length mmap areas
;;;; used to store all kind of CL built-in types that do not fit a single CPU word:
;;;; bignums, ratios, single-floats and double-floats, complexes,
;;;; pathnames, cons cells and lists, vectors, arrays, strings and hash-tables.
;;;;
;;;; mem-boxes are allocates in multiples of 4 (actually +mem-box/min-words+) CPU words,
;;;; and they contain a 2 CPU-word header followed by type-specific payload:
;;;;
;;;;   word 0: tag = type. it uses a different coding than pointer tags (see +mem-box/...+ constants)
;;;;           value = pointer to owner. used by GC.
;;;;
;;;;   word 1: tag = available for type-specific data, for example sign bits
;;;;           value = number of allocated words / 4. also counts the header (i.e. words 0 and 1)
;;;;
;;;;   word 2... : payload. depends on type


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline box-pointer->size))
(defun box-pointer->size (value)
  (declare (type mem-vid value))
  (the mem-size (* value +mem-box/min-words+)))

(declaim (inline size->box-pointer))
(defun size->box-pointer (index)
  (declare (type mem-size index))
  (the mem-vid (nth-value 0 (truncate index +mem-box/min-words+))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline box-index   (setf box-index)
                 box-n-words (setf box-n-words)
                 box-value   (setf box-value)
                 box-next    (setf box-next)
                 reuse-box))


;; wrapper for values that cannot be stored as unboxed

#|
(declaim (inline %make-box setf-box-value-index-n-words))
(defstruct (box (:constructor %make-box))
  (index   0 :type mem-size)
  (n-words 0 :type mem-size)
  (value   nil))


(defun make-box (index n-words &optional value)
  "Create a new box to wrap VALUE. Assumes VALUE will be stored at INDEX in memory store."
  (declare (type mem-size index n-words))
  (%make-box :index index :n-words n-words :value value))

(defun reuse-box (box index n-words value)
  (setf (box-value   box) value
        (box-index   box) index
        (box-n-words box) n-words)
  box)
|#

(deftype box () 'cons)

(defun make-box (index n-words &optional value)
  "Create a new box to wrap VALUE. Assumes VALUE will be stored at INDEX in memory store."
  (declare (type mem-size index n-words))
  `(,value ,index . ,n-words))

(defun box-value (box)
  (declare (type box box))
  (first box))

(defun (setf box-value) (value box)
  (declare (type box box))
  (setf (first box) value))

(defun box-index (box)
  (declare (type box box))
  (the mem-size (second box)))

(defun (setf box-index) (index box)
  (declare (type box box)
           (type mem-size index))
  (setf (second box) index))

(defun box-n-words (box)
  (declare (type box box))
  (the mem-size (rest (rest box))))

(defun (setf box-n-words) (n-words box)
  (declare (type box box)
           (type mem-size n-words))
  (setf (rest (rest box)) n-words))

(defun reuse-box (box index n-words value)
  "Set BOX slots to specified values. Return BOX."
  (declare (type box box)
           (type mem-size index n-words))
  (setf (box-value box) value)
  (let ((tail (rest box)))
    (setf (first tail)  index
          (rest  tail)  n-words))
  box)
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mwrite-box/header))
(defun mwrite-box/header (ptr index boxed-type n-words)
  "Write to mmap area the header common to all boxed values.
Return INDEX pointing to box payload"
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type mem-tag boxed-type))

  (mset-tag-and-vid ptr index boxed-type (size->box-pointer n-words))
  (incf (the mem-size index)))


(declaim (inline mwrite-box/box-header))
(defun mwrite-box/box-header (ptr box boxed-type)
  "Write to mmap area the header common to all boxed values.
Return INDEX pointing to box payload"
  (declare (type maddress ptr)
           (type box box)
           (type mem-tag boxed-type))

  (mwrite-box/header ptr (box-index box) boxed-type (box-n-words box)))


(declaim (inline mread-box/header))
(defun mread-box/header (ptr index)
  "Read from mmap area the header common to all boxed values.
Return BOXED-TYPE and N-WORDS as multiple values"
  (declare (type maddress ptr)
           (type mem-size index))

  (with-tag-and-vid (boxed-type allocated-words/4) (ptr index)
    (values
     boxed-type
     (box-pointer->size allocated-words/4))))


(declaim (inline mread-box/box-header))
(defun mread-box/box-header (ptr index)
  "Read from mmap area the header common to all boxed values.
Return BOX and BOXED-TYPE as multiple values"
  (declare (type maddress ptr)
           (type mem-size index))

  (with-tag-and-vid (boxed-type allocated-words/4) (ptr index)
    (values
     (make-box index (box-pointer->size allocated-words/4))
     boxed-type)))


(defun mwrite-box/box (ptr value &optional box)
  "Write a boxed value into the object store, (re)allocating space if needed.
Return the written box."
  (declare (type maddress ptr)
           (type (or null box) box))

  (let* ((boxed-type (mdetect-box-type     value))
         (n-words    (msize-box-rounded-up value boxed-type))
         (allocated-n-words (if box (box-n-words box) 0)))
    
    (if (and (<= n-words allocated-n-words)
             (>= n-words (ash allocated-n-words -1)))
        ;; reuse the existing memory
        (setf n-words allocated-n-words)
        ;; we must (re)allocate memory
        (setf box (box-realloc ptr box n-words)
              ;; ABSOLUTELY NECESSARY! read back actually allocated
	      ;; n-words (usually rounded up somewhat)
              n-words (box-n-words box)))

    (setf (box-value box) value)
    (let ((index (box-index box)))
      (mwrite-box ptr index (mem-size+ index n-words) value boxed-type))
    box))


(defun mread-box/box (ptr index box)
  "Read a boxed value from the memory starting at (PTR+INDEX).
Reuse box and return the boxed value."
  (declare (type maddress ptr)
           (type mem-size index)
           (type box box))

  (let1 end-index (mem-size+ index (box-n-words box))
    (multiple-value-bind (value n-words) (mread-box ptr index end-index)
      (reuse-box box index n-words value))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-type-error (ptr index boxed-type)
  (error "the object at address ~S + ~S declares to have type ~S,
which is not in the valid range ~S..~S for ~S"
         ptr index boxed-type +mem-box/first+ +mem-box/last+ 'boxed-type))

(defun mem-length-error (ptr index end-index n-words)
  (declare (type integer index end-index))
  (let ((available-words (- end-index index)))
    (error "the object at address ~S + ~S declares to be ~S word~P long,
but only ~S word~P available at that location"
           ptr index n-words n-words available-words available-words)))

(defun mem-overrun-error (ptr index end-index n-words)
  (declare (type integer index end-index))
  (let ((available-words (- end-index index)))
    (error "attempt to write ~S word~P at address ~S + ~S,
but only ~S word~P available at that location"
           n-words n-words ptr index available-words available-words)))

(defun array-rank-error (ptr index array-type rank max-rank)
  (declare (type symbol array-type)
           (type integer index rank max-rank))
  (error "the ~S at address ~S + ~S declares to have
~S dimensions, which is outside the supported rank range 0...~S"
         array-type ptr index rank max-rank))

(defun array-length-error (ptr index array-type length max-length)
  (declare (type symbol array-type)
           (type integer index length max-length))
  (error "the ~S at address ~S + ~S declares to contain
~S elements, which is outside the supported length range 0...~S"
         array-type ptr index length max-length))


(defmacro check-box-type (ptr index boxed-type)
  (with-gensyms (ptr_ index_ boxed-type_)
    `(let ((,ptr_        ,ptr)
           (,index_      ,index)
           (,boxed-type_ ,boxed-type))
       (unless (typep ,boxed-type_ 'mem-box-type)
         (box-type-error ,ptr_ ,index_ ,boxed-type_)))))


(defmacro check-mem-length (ptr index end-index n-words)
  (with-gensyms (ptr_ index_ end-index_ n-words_)
    `(let ((,ptr_       ,ptr)
           (,index_     ,index)
           (,end-index_ ,end-index)
           (,n-words_   ,n-words))
       (unless (<= ,n-words_ (mem-size- ,end-index_ ,index_))
         (mem-length-error ,ptr_ ,index_ ,end-index_ ,n-words_)))))


(defmacro check-mem-overrun (ptr index end-index n-words)
  (with-gensyms (ptr_ index_ end-index_ n-words_)
    `(let ((,ptr_       ,ptr)
           (,index_     ,index)
           (,end-index_ ,end-index)
           (,n-words_   ,n-words))
       (unless (<= ,n-words_ (mem-size- ,end-index_ ,index_))
         (mem-overrun-error ,ptr_ ,index_ ,end-index_ ,n-words_)))))


(defmacro check-array-rank (ptr index array-type rank)
  (with-gensyms (r max-rank)
    `(let ((,r ,rank)
           (,max-rank #.(min array-rank-limit most-positive-fixnum)))
       (unless (<= 0 ,r ,max-rank)
         (array-rank-error ,ptr ,index ,array-type ,r ,max-rank)))))


(defmacro check-array-length (ptr index array-type length)
  (with-gensyms (len max-len)
    `(let ((,len    ,length)
           (,max-len #.(min array-dimension-limit most-positive-fixnum)))
       (unless (<= 0 ,len ,max-len)
         (array-length-error ,ptr ,index ,array-type ,len ,max-len)))))


;; kind of forward declaration for msize-box, mwrite-box, mread-box2 and mdetect-box-type,
;; needed here but defined later, in boxed.lisp
(declaim (ftype (function (mem-size t &optional mem-box-type)
                          (values mem-size &optional))
                msize-box)

         (ftype (function (maddress mem-size mem-size t mem-box-type)
			  (values mem-size &optional))
                mwrite-box)
         
         (ftype (function (maddress mem-size mem-size mem-box-type)
			  (values t mem-size &optional))
                mread-box2)

         (ftype (function (t)
                          (values (or null mem-box-type) &optional))
                mdetect-box-type)

	 (notinline msize-box mwrite-box mread-box2 mdetect-box-type))


;; kind of forward declaration for msize-obj, mwrite-obj and mread-obj
;; needed here but defined later, in struct.lisp
(declaim (ftype (function (mem-size t &optional)
                          (values mem-size &optional))
                msize-obj)

         (ftype (function (maddress mem-size mem-size t)
			  (values mem-size &optional))
                mwrite-obj)
         
         (ftype (function (maddress mem-size mem-size)
			  (values t mem-size &optional))
                mread-obj2)
         
	 (notinline msize-obj mwrite-obj mread-obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; main API functions accepting both boxed and unboxed values              ;;;;
;;;; we define them early, to inline them in the rest of the code            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (ftype (function (mem-size t &key (:type (or null mem-box-type)))
                          (values mem-size &optional))
		msize)
	 (inline msize))

;; note: unlike MSIZE-OBJECT, VALUE is last argument
(defun msize (index value &key (type nil))
  "Compute and return the number of CPU words needed to store VALUE.
If VALUE can be stored unboxed, returns 1. Otherwise forwards the call
to MSIZE-BOX or, for user-defined types, to MSIZE-OBJECT.
Does NOT round up the returned value to a multiple of +MEM-BOX/MIN-WORDS+"
  (declare (type mem-size index)
           (type (or null mem-box-type) type))

  (if (is-unboxed? value)
      (mem-size+1 index)
      (if-bind box-type (or type (mdetect-box-type value))
        (msize-box index value box-type)
        (msize-obj index value))))




(declaim (ftype (function (maddress mem-size mem-size t &key (:type (or null mem-box-type)))
                          (values mem-size &optional))
		mwrite)
	 (inline mwrite))


;; note: unlike MWRITE-OBJECT, VALUE is last argument
(defun mwrite (ptr index end-index value &key (type nil))
  "Write a value (boxed, unboxed or object) into the memory starting at (PTR+INDEX).
Return the INDEX pointing to immediately after the value just written.

WARNING: enough memory must be already allocated at (PTR+INDEX) !!!"
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type t value)
           (type (or null mem-box-type) type))

  (check-mem-overrun ptr index end-index 1)

  (if (mset-unboxed ptr index value)
      (mem-size+1 index)
      (if-bind box-type (or type (mdetect-box-type value))
          (mwrite-box ptr index end-index value box-type)
          (mwrite-obj ptr index end-index value))))



(declaim (ftype (function (maddress mem-size mem-size)
                          (values t mem-size &optional))
		mread)
	 (inline mread))


;; note: unlike MREAD-OBJECT, there is no TYPE argument
(defun mread (ptr index end-index)
  "Read a value (boxed, unboxed or object) from the memory starting at (PTR+INDEX).
Return multiple values:
1) the value 
2) the INDEX pointing to immediately after the value just read"
  (declare (type maddress ptr)
           (type mem-size index end-index))

  (check-mem-length ptr index end-index 1)

  (multiple-value-bind (value boxed-type) (mget-unboxed ptr index)
    (unless boxed-type
      (return-from mread (values value (mem-size+1 index))))

    (let* ((n-words (box-pointer->size value))
           (end-box (mem-size+ index n-words))
           (end-index (min end-index end-box)))
      (if (<= (the fixnum boxed-type) +mem-box/last+)
          (mread-box2 ptr index end-index boxed-type)
          (mread-obj ptr index end-index)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun !mwrite (ptr index value)
  "Used only for debugging."
  (declare (type maddress ptr)
           (type mem-size index))

  (mwrite ptr index +mem-box/max-words+ value))

(defun !mread (ptr index)
  "Used only for debugging."
  (declare (type maddress ptr)
           (type mem-size index))
  (mread ptr index +mem-box/max-words+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro multiple-value-bind-chain2* ((var1 var2 &rest more-vars)
                                       (func arg1 arg2 &rest more-args) &body body)
  "Warning: this macro expands multiple references to FUNC, ARG1 and MORE-ARGS"
  (if more-vars
      (with-gensym tmp
        `(multiple-value-bind (,var1 ,tmp) (,func ,arg1 ,arg2 ,@more-args)
           (multiple-value-bind-chain2* (,var2 ,@more-vars) (,func ,arg1 ,tmp ,@more-args)
             ,@body)))
      `(multiple-value-bind (,var1 ,var2) (,func ,arg1 ,arg2 ,@more-args)
         ,@body)))


(defmacro with-mread* ((var1 var2 &rest more-vars)
                                    (ptr index end-index) &body body)
  "syntactic sugar for multiple calls to mread. Last name in MORE-VARS
will be bound to the new value of INDEX"
  (if more-vars
      (with-gensyms (ptr-var idx-var end-var)
        `(let* ((,ptr-var ,ptr)
                (,idx-var ,index)
                (,end-var ,end-index))
           (multiple-value-bind-chain2* (,var1 ,var2 ,@more-vars)
               (mread ,ptr-var ,idx-var ,end-var)
             ,@body)))
      `(multiple-value-bind (,var1 ,var2) (mread ,ptr ,index ,end-index)
         ,@body)))


(defmacro msize* (index value &rest more-values)
  (if more-values
      (with-gensym new-index
        `(let1 ,new-index (msize ,index ,value)
           (msize* ,new-index ,@more-values)))
      `(msize ,index ,value)))

  

(defmacro %mwrite* (ptr index end-index value &rest more-values)
  "Warning: this macro expands multiple references to PTR and END-INDEX"
  (if more-values
      (with-gensyms (new-index)
        `(let1 ,new-index (mwrite ,ptr ,index ,end-index ,value)
           (%mwrite* ,ptr ,new-index ,end-index ,@more-values)))
      `(mwrite ,ptr ,index ,end-index ,value)))


(defmacro mwrite* (ptr index end-index value &rest more-values)
  (if more-values
      (with-gensyms (ptr-var idx-var end-var)
        `(let* ((,ptr-var ,ptr)
                (,idx-var ,index)
                (,end-var ,end-index))
           (%mwrite* ,ptr-var ,idx-var ,end-var ,value ,@more-values)))
      `(mwrite ,ptr ,index ,end-index ,value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun !mzero-box (ptr box)
  "Fill an allocated box with zeroes."
  (declare (type maddress ptr)
           (type box box))
  (let* ((index   (box-index box))
         (n-words (box-n-words box))
         (start   index)
         (end     (mem-size+ start n-words)))

    (!mzero-words ptr start end)))



(defun !mzero-fbox (ptr box)
  "Fill a free box with zeroes."
  (declare (type maddress ptr)
           (type box box))
  (let* ((index   (box-index box))
         (n-words (box-n-words box))
         ;; free boxes are written at the end of the free mmap area they represent!
         (start   (mem-size- index (mem-size- n-words +mem-box/header-words+)))
         (end     (mem-size+ start n-words)))
    
    (!mzero-words ptr start end)))


