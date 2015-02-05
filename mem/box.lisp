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


(declaim (inline box-vid->size))
(defun box-vid->size (value)
  (declare (type mem-vid value))
  (the mem-size (* value +mem-box/min-words+)))

(declaim (inline size->box-vid))
(defun size->box-vid (index)
  (declare (type mem-size index))
  (the mem-vid (nth-value 0 (truncate index +mem-box/min-words+))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline mwrite-box/header))
(defun mwrite-box/header (ptr index boxed-type n-words)
  "Write to mmap area the header common to all boxed values.
Return INDEX pointing to box payload"
  (declare (type maddress ptr)
           (type mem-size index n-words)
           (type mem-tag boxed-type))

  (mset-tag-and-vid ptr index boxed-type (size->box-vid n-words))
  (incf (the mem-size index)))


(declaim (inline mread-box/header))
(defun mread-box/header (ptr index)
  "Read from mmap area the header common to all boxed values.
Return BOXED-TYPE and N-WORDS as multiple values"
  (declare (type maddress ptr)
           (type mem-size index))

  (with-tag-and-vid (boxed-type allocated-words/4) (ptr index)
    (values
     boxed-type
     (box-vid->size allocated-words/4))))


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

    (let* ((n-words (box-vid->size value))
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


