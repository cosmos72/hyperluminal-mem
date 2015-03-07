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


(in-package :hyperluminal-mem-ffi)

(deftype ffi-address ()
  #-abcl 'cffi-sys:foreign-pointer
  #+abcl 'java:java-object)


#+abcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct ffi-type-descriptor
    (index 0)
    (size  0)
    (name  "")
    (mask nil))

  (defmethod make-load-form ((obj ffi-type-descriptor) &optional env)
    (declare (ignorable env))
    `(make-ffi-type-descriptor :index ,(ffi-type-descriptor-index obj)
                               :size  ,(ffi-type-descriptor-size  obj)
                               :name  ,(ffi-type-descriptor-name  obj)
                               :mask  ,(ffi-type-descriptor-mask  obj))))


#+abcl
(defun %ffi-type-descriptor (type)
  (declare (type keyword type))

    (ecase type
      (:unsigned-char  #S(ffi-type-descriptor :index 0 :size 1 :name "byte"  :mask #xFF))
      (:char           #S(ffi-type-descriptor :index 0 :size 1 :name "byte"))
      (:unsigned-short #S(ffi-type-descriptor :index 1 :size 2 :name "short" :mask #xFFFF))
      (:short          #S(ffi-type-descriptor :index 1 :size 2 :name "short"))

      ;; pure Java has no pointers... this is an arbitrary choice
      ((:unsigned-int :pointer)
       #||#            #S(ffi-type-descriptor :index 2 :size 4 :name "int"   :mask #xFFFFFFFF))
      (:int            #S(ffi-type-descriptor :index 2 :size 4 :name "int"))

      ;; Java has no "long long", treat as "long"
      ((:unsigned-long :unsigned-long-long)
       #S(ffi-type-descriptor :index 3 :size 8 :name "long"  :mask #xFFFFFFFFFFFFFFFF))

      ((:long :long-long)
       #S(ffi-type-descriptor :index 3 :size 8 :name "long"))

      (:float          #S(ffi-type-descriptor :index 4 :size 4 :name "float"))
      (:double         #S(ffi-type-descriptor :index 5 :size 8 :name "double"))))


#-abcl
(declaim (inline ffi-sizeof))
  
(defun ffi-sizeof (type)
  (declare (type keyword type))
  (the fixnum
       #-abcl
       (cffi-sys:%foreign-type-size type)
  
       #+abcl
       (ffi-type-descriptor-size (%ffi-type-descriptor type))))




(defun ffi-native-type-name (type)
  (declare (type keyword type))
  #-abcl
  type

  #+abcl
  (ffi-type-descriptor-name (%ffi-type-descriptor type)))

    
  
(define-constant-once +null-pointer+ #-abcl (cffi-sys:null-pointer) #+abcl java:+null+)

(declaim (inline null-pointer?))

(defun null-pointer? (ptr)
  (declare (type ffi-address ptr))
  #-abcl (cffi-sys:null-pointer-p ptr)
  #+abcl (java:jnull-ref-p ptr))



#+abcl
(progn
  (defconstant +java-nio-byteorder-native+
    (java:jstatic "nativeOrder" "java.nio.ByteOrder"))

  (defconstant +java-nio-byteorder-little-endian+
    (java:jfield "java.nio.ByteOrder" "LITTLE_ENDIAN"))

  (defconstant +java-nio-byteorder-big-endian+
    (java:jfield "java.nio.ByteOrder" "BIG_ENDIAN"))

  (define-global *current-java-nio-byteorder*
      +java-nio-byteorder-native+
    "The byte order currently in use. Will be set by mem/mem.lisp")

  (defconstant +java-nio-bytebuffer-allocate+
    (java:jmethod "java.nio.ByteBuffer" "allocate" "int"))

  (defconstant +java-nio-bytebuffer-allocate-direct+
    (java:jmethod "java.nio.ByteBuffer" "allocateDirect" "int"))

  (defconstant +java-nio-bytebuffer-set-byteorder+
    (java:jmethod "java.nio.ByteBuffer" "order" "java.nio.ByteOrder"))

  (defconstant +java-nio-bytebuffer-get+
    (coerce
     (list
      (java:jmethod "java.nio.ByteBuffer" "get" "int")
      (java:jmethod "java.nio.ByteBuffer" "getShort" "int")
      (java:jmethod "java.nio.ByteBuffer" "getInt" "int")
      (java:jmethod "java.nio.ByteBuffer" "getLong" "int")
      (java:jmethod "java.nio.ByteBuffer" "getFloat" "int")
      (java:jmethod "java.nio.ByteBuffer" "getDouble" "int"))
     'vector))

  (defconstant +java-nio-bytebuffer-set+
    (coerce
     (list
      (java:jmethod "java.nio.ByteBuffer" "put" "int" "byte")
      (java:jmethod "java.nio.ByteBuffer" "putShort" "int" "short")
      (java:jmethod "java.nio.ByteBuffer" "putInt" "int" "int")
      (java:jmethod "java.nio.ByteBuffer" "putLong" "int" "long")
      (java:jmethod "java.nio.ByteBuffer" "putFloat" "int" "float")
      (java:jmethod "java.nio.ByteBuffer" "putDouble" "int" "double"))
     'vector))

  (defun ffi-endianity ()
    (if (java:jequal *current-java-nio-byteorder* +java-nio-byteorder-little-endian+)
        :little-endian
        :big-endian))

  (defun set-ffi-endianity (keyword)
    (declare (type (member :little-endian :big-endian) keyword))
    (setf *current-java-nio-byteorder*
          (ecase keyword
            (:little-endian +java-nio-byteorder-little-endian+)
            (:big-endian    +java-nio-byteorder-big-endian+)))
    keyword))

(defsetf ffi-endianity set-ffi-endianity)
    


(defmacro ffi-mem-get (ptr type offset)
  #-abcl
  `(cffi-sys:%mem-ref ,ptr ,type ,offset)

  #+abcl
  (if (constantp type)
      (let* ((descriptor (%ffi-type-descriptor (eval type)))
             (mask       (ffi-type-descriptor-mask descriptor))
             (method     (svref +java-nio-bytebuffer-get+
                                (ffi-type-descriptor-index descriptor))))
        (if mask
            `(logand ,mask (java:jcall ,method ,ptr ,offset))
            `(java:jcall ,method ,ptr ,offset)))
      
      (with-gensyms (descriptor mask method value)
        `(let* ((,descriptor (%ffi-type-descriptor ,type))
                (,mask       (ffi-type-descriptor-mask ,descriptor))
                (,method     (svref +java-nio-bytebuffer-get+
                                    (ffi-type-descriptor-index ,descriptor)))
                (,value      (java:jcall ,method ,ptr ,offset)))
           (if ,mask
               (logand ,mask ,value)
               ,value)))))


(defmacro ffi-mem-set (value ptr type offset)
  #-abcl
  `(cffi-sys:%mem-set ,value ,ptr ,type ,offset)

  #+abcl
  ;; no need to use (ffi-type-descriptor-mask) here:
  ;; ABCL already wraps around integer values on overflow
  ;; while converting to primitive types for (java:jcall) arguments
  (if (constantp type)
      (let* ((descriptor (%ffi-type-descriptor (eval type)))
             (method     (svref +java-nio-bytebuffer-set+
                                (ffi-type-descriptor-index descriptor))))
        (with-gensym var
          `(let ((,var ,value))
             (java:jcall ,method ,ptr ,offset ,var)
             ,var)))
                 
      (with-gensyms (descriptor method var)
        `(let* ((,var ,value)
                (,descriptor (%ffi-type-descriptor ,type))
                (,method     (svref +java-nio-bytebuffer-set+
                                    (ffi-type-descriptor-index ,descriptor))))
           (java:jcall ,method ,ptr ,offset ,var)
           ,var))))


#-abcl
(declaim (inline ffi-mem-alloc))
(defun ffi-mem-alloc (n-bytes)
  "Allocate N-BYTES of raw memory and return raw pointer to it.
The obtained memory must be freed manually: call FFI-MEM-FREE on it when no longer needed."
  #-abcl
  (cffi-sys:%foreign-alloc n-bytes)
  #+abcl
  (let ((ptr (java:jstatic +java-nio-bytebuffer-allocate+ nil n-bytes)))
    (java:jcall +java-nio-bytebuffer-set-byteorder+ ptr *current-java-nio-byteorder*)
    ptr))


(declaim (inline ffi-mem-free))
(defun ffi-mem-free (ptr)
  "Deallocate a block of raw memory previously obtained with FFI-MEM-ALLOC."
  #-abcl (declare (type cffi-sys:foreign-pointer ptr))
  #-abcl (cffi-sys:foreign-free ptr)
  #+abcl (declare (ignore ptr)))


(defmacro with-ffi-mem ((var-name n-bytes &optional n-bytes-var) &body body)
  #-abcl
  (progn
    (when (constantp n-bytes)
      (setf n-bytes (eval n-bytes))
      ;; we do not want to easily exceed the 8MB *default* maximum stack size on Linux
      ;; so we place a somewhat arbitrary limit at 960kB
      (when (and (typep n-bytes 'fixnum) (<= n-bytes (ash 960 10)))
        (return-from with-ffi-mem
          `(cffi-sys:with-foreign-pointer (,var-name ,n-bytes ,@(when n-bytes-var `(,n-bytes-var)))
             ,@body))))
    
    `(let* (,@(when n-bytes-var `((,n-bytes-var ,n-bytes)))
            (,var-name (ffi-mem-alloc ,(or n-bytes-var n-bytes))))
       (unwind-protect
            (progn ,@body)
         (ffi-mem-free ,var-name))))
  #+abcl
  `(let* (,@(when n-bytes-var `((,n-bytes-var ,n-bytes)))
          (,var-name (ffi-mem-alloc ,(or n-bytes-var n-bytes))))
     ;; ffi-mem-free does nothing on ABCL
     ,@body))


#-abcl
(defmacro with-vector-mem ((var-name vector) &body body)
  #+(and)
  `(cffi-sys:with-pointer-to-vector-data (,var-name ,vector)
     ,@body)
    
  ;; hand-crafted implementation for SBCL.
  ;; Not needed, cffi-sys implementation is both tighter and more general
  #-(and)
  (let* ((word-size #.(cffi-sys:%foreign-type-size :pointer))
         (lisp-object-address-mask (* -2 word-size))
         (array (gensym (symbol-name 'array))))
    `(let ((,array ,vector))
       (declare (type simple-array-vector ,array))
       (sb-sys:with-pinned-objects (,array)
         (let ((,var-name (cffi-sys:make-pointer 
                           (the sb-ext:word
                                (+ ,(* 2 word-size) ; skip header and array length
                                   (logand ,lisp-object-address-mask
                                           (sb-kernel:get-lisp-obj-address ,array)))))))
           ,@body)))))
