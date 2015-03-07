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
  (defun %detect-native-endianity ()
    (with-mem-words (p 1)
      (let ((little-endian 0)
            (big-endian 0))

        (loop for i from 0 below +msizeof-word+
             for bits = (logand (1+ i) +mem-byte/mask+) do

             (setf little-endian (logior little-endian (ash bits (* i +mem-byte/bits+)))
                   big-endian    (logior bits (ash big-endian +mem-byte/bits+)))

             (mset-byte p i bits))

        (let ((endianity (%mget-t :word p 0)))
          (cond
            ((eql endianity little-endian) :little-endian)
            ((eql endianity big-endian)    :big-endian)
            (t (error "cannot build HYPERLUMINAL-MEM: unsupported architecture.
    CPU word endianity is #x~X, expecting either #x~X (little-endian) or #x~X (big-endian)"
                      endianity little-endian big-endian))))))))

          
(eval-always
  (defconstant +mem/native-endianity+ (%detect-native-endianity)))

(eval-always
  (defun choose-endianity ()
    "Choose the file format and ABI between :little-endian or :big-endian.

By default, Hyperluminal-MEM file format and ABI is autodetected to match
the endianity of CFFI-SYS raw memory, i.e. the CPU endianity.

It is possible to compile Hyperluminal-MEM for a different endianity by adding
an appropriate entry in the global variable `*FEATURES*` **before** compiling
and loading Hyperluminal-MEM.

To force little-endian ABI:
  (pushnew :hyperluminal-mem/endianity/little *features*)

To force big-endian ABI:
  (pushnew :hyperluminal-mem/endianity/big *features*)"

    ;;search for :hyperluminal-mem/endianity/{little,big,native,inverted} *features*
    (let ((endianity (find-hldb-option/keyword 'endianity)))
      (case endianity
        ((nil :native) +mem/native-endianity+)
        (:inverted  (if (eq +mem/native-endianity+ :little-endian)
                        :big-endian
                        :little-endian))
        (:little :little-endian)
        (:big    :big-endian)
        (otherwise
         (error "cannot build HYPERLUMINAL-MEM: unsupported option ~S in ~S,
  expecting one of ~S"
                (intern (concat-symbols 'hyperluminal-mem/endianity/ endianity) :keyword)
                '*features*
                '(:hyperluminal-mem/endianity/little
                  :hyperluminal-mem/endianity/big
                  :hyperluminal-mem/endianity/native
                  :hyperluminal-mem/endianity/inverted)))))))
                

(eval-always
  (defconstant +mem/chosen-endianity+ (choose-endianity))
  (set-feature :hlmem/native-endianity (eql +mem/chosen-endianity+ +mem/native-endianity+)))



(fmakunbound '%maybe-invert-endianity)
(fmakunbound 'maybe-invert-endianity)

#+abcl
(eval-always
  ;; on ABCL, we set the endianity on java.nio.ByteBuffer, used to implement raw memory:
  ;; no need for conversions in mset-t and mget-t
  (setf (ffi-endianity) +mem/chosen-endianity+)

  (defmacro maybe-invert-endianity/integer (type value)
    (declare (ignore type))
    value))


#?+hlmem/native-endianity
(progn
  (fmakunbound '%maybe-invert-endianity/integer)
  (defmacro maybe-invert-endianity/integer (type value)
    (declare (ignore type))
    value))

  
#?-hlmem/native-endianity
(progn
  (defun %maybe-invert-endianity/integer (type value)
    (let ((size (%msizeof (parse-type type))))
      (case size
        (1 value)
        (2 (swap-bytes/2 value))
        (4 (swap-bytes/4 value))
        (8 (swap-bytes/8 value))
        (otherwise
         (funcall (find-swap-bytes/n size) value)))))

  (defmacro maybe-invert-endianity/integer (type value)
    (if (constantp type)
        (let ((size (%msizeof (eval `(parse-type ,type)))))
          (case size
            (1 value)
            (2 `(swap-bytes/2 ,value))
            (4 `(swap-bytes/4 ,value))
            (8 `(swap-bytes/8 ,value))
            (otherwise
             `(,(find-swap-bytes/n size) ,value))))
        `(%maybe-invert-endianity/integer ,type ,value))))




(declaim (inline mset-sfloat))
(defun mset-sfloat (value ptr byte-offset)
  (declare (type single-float value)
           (type maddress ptr))
  #+abcl
  (%mset-t value :sfloat ptr byte-offset)

  #-abcl
  (progn
    #?+hlmem/native-endianity
    (%mset-t value :sfloat ptr byte-offset)
    
    #?-hlmem/native-endianity
    (progn
      #-sbcl (%mset-t value :sfloat ptr byte-offset)
      (let* ((bits     
              #+sbcl (the (unsigned-byte 32)
                          (logand #.+mem-sfloat/mask+
                                  (sb-kernel:single-float-bits value)))
              #-sbcl (%mget-t :sfloat-word ptr byte-offset))
             (xbits (maybe-invert-endianity/integer :sfloat-word bits)))
        (%mset-t xbits :sfloat-word ptr byte-offset)))))


(declaim (inline mget-sfloat))
(defun mget-sfloat (ptr byte-offset)
  (declare (type maddress ptr))
  #+abcl
  (%mget-t :sfloat ptr byte-offset)

  #-abcl
  (progn
    #?+hlmem/native-endianity
    (%mget-t :sfloat ptr byte-offset)
    
    #?-hlmem/native-endianity
    (let* ((xbits (%mget-t :sfloat-word ptr byte-offset))
           (bits  (maybe-invert-endianity/integer :sfloat-word xbits)))
      #+sbcl
      (sb-kernel:make-single-float (the (signed-byte 32) bits))
      #-sbcl
      (with-mem-words (tmp #.+sfloat/words+)
        (%mset-t bits :sfloat-word tmp 0)
        (%mget-t :sfloat tmp 0)))))


(declaim (inline mset-dfloat))
(defun mset-dfloat (value ptr byte-offset)
  (declare (type double-float value)
           (type maddress ptr))
  #+abcl
  (%mset-t value :dfloat ptr byte-offset)

  #-abcl
  (progn
    #?+hlmem/native-endianity
    (%mset-t value :dfloat ptr byte-offset)
    
    #?-hlmem/native-endianity
    (progn
      (%mset-t value :dfloat ptr byte-offset)
      (let* ((bits  (%mget-t :dfloat-word ptr byte-offset))
             (xbits (maybe-invert-endianity/integer :dfloat-word bits)))
        (%mset-t xbits :dfloat-word ptr byte-offset)))))


(declaim (inline mget-dfloat))
(defun mget-dfloat (ptr byte-offset)
  (declare (type maddress ptr))
  #+abcl
  (%mget-t :dfloat ptr byte-offset)

  #-abcl
  (progn
    #?+hlmem/native-endianity
    (%mget-t :dfloat ptr byte-offset)
    
    #?-hlmem/native-endianity
    (let* ((xbits (%mget-t :dfloat-word ptr byte-offset))
           (bits  (maybe-invert-endianity/integer :dfloat-word xbits)))
      #+sbcl
      (sb-kernel:make-double-float (the (signed-byte 32) (ash bits -32))
                                   (logand bits #xFFFFFFFF))
      
      #-sbcl
      (with-mem-words (tmp #.+dfloat/words+)
        (%mset-t bits :dfloat-word tmp 0)
        (%mget-t :dfloat tmp 0)))))
