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


(in-package :hyperluminal-ffi)

(deftype maddress () #-abcl 'cffi-sys:foreign-pointer #+abcl 'java:java-object)

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
  #-abcl
  (cffi-sys:%foreign-type-size type)
  
  #+abcl
  (ffi-type-descriptor-size (%ffi-type-descriptor type)))




(defun ffi-native-type-name (type)
  (declare (type keyword type))
  #-abcl
  type

  #+abcl
  (ffi-type-descriptor-name (%ffi-type-descriptor type)))

    
  
(define-constant-once +null-pointer+ #-abcl (cffi-sys:null-pointer) #+abcl java:+null+)

(declaim (inline null-pointer?))

(defun null-pointer? (ptr)
  (declare (type maddress ptr))
  #-abcl (cffi-sys:null-pointer-p ptr)
  #+abcl (java:jnull-ref-p ptr))



#+abcl
(progn
  (defconstant +java-nio-byteorder-native+
    (java:jstatic "nativeOrder" "java.nio.ByteOrder"))

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
     'vector)))

    


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



(defmacro with-ffi-mem ((var-name bytes) &body body)
  #-abcl
  `(cffi-sys:with-foreign-pointer (,var-name ,bytes)
     ,@body)
  #+abcl
  `(let ((,var-name (java:jstatic +java-nio-bytebuffer-allocate+ nil ,bytes)))
     (java:jcall +java-nio-bytebuffer-set-byteorder+ ,var-name +java-nio-byteorder-native+)
     ,@body))


