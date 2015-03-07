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


;; default implementation of mget-word and mset-word
(eval-always
  (defmacro %mget-t (type ptr &optional (byte-offset 0))
    `(ffi-mem-get ,ptr ,(parse-type type) ,byte-offset))

  (defmacro %mset-t (value type ptr &optional (byte-offset 0))
    `(ffi-mem-set ,value ,ptr ,(parse-type type) ,byte-offset)))
  

(eval-always
  (defmacro mget-byte (ptr byte-index)
    "Used only by MREAD-MAGIC."
    `(%mget-t :byte ,ptr ,byte-index))

  (defmacro mset-byte (ptr byte-index value)
    "Used only by MWRITE-MAGIC, and %DETECT-ENDIANITY, !MEMSET-BYTES and !MEMCPY-BYTES.
  Warning: evaluates VALUE before the other arguments!"
    `(%mset-t ,value :byte ,ptr ,byte-index))

  (defsetf mget-byte mset-byte))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-always
  (define-condition unsupported-arch (simple-error)
    ())

  (defun cffi-type-name (sym)
    (declare (type symbol sym))
    (string-downcase (symbol-name (parse-type sym))))

  ;; (msizeof :byte) must be 1
  (when (/= +msizeof-byte+ 1)
    (error "cannot build HYPERLUMINAL-MEM: unsupported architecture.
size of ~S is ~S bytes, expecting exactly 1 byte"
           (cffi-type-name :byte) +msizeof-byte+))

  ;; we need at least a 32-bit architecture
  (when (< +msizeof-word+ 4)
    (error "cannot build HYPERLUMINAL-MEM: unsupported architecture.
size of ~S is ~S bytes, expecting at least 4 bytes"
           (cffi-type-name :byte) +msizeof-word+)))


(eval-always
  ;; determine number of bits per CPU word
  (defun %detect-bits-per-word ()
    (declare (optimize (speed 0) (safety 3))) ;; ABSOLUTELY NECESSARY!

    (let ((bits-per-word 1))

      (flet ((done (c)
               (declare (ignorable c))
               #+hyperluminal-mem/debug (log:debug c)
               (return-from %detect-bits-per-word bits-per-word)))
      
        (with-mem-words (p 1)
          (loop for i = 1 then (logior (ash i 1) 1)
             for bits = 1 then (1+ bits)
             do
               (handler-case
                   (progn
                     #+hyperluminal-mem/debug
                     (log:debug "(i #x~X) (bits ~D) ..." i bits)

                     ;; cannot use (mset-word) and (mget-word) yet,
                     ;; they contain (the mem-word ...)
                     ;; and this function is used to (deftype mem-word ...)
                     (%mset-t i :word p)
                   
                     (let ((j (%mget-t :word p)))
                       #+hyperluminal-mem/debug
                       (log:debug " read back: #x~X ..." j)
                     
                       (unless (eql i j)
                         (error "reading value '~S' stored in a CPU word returned '~S'" i j))
                   
                       #+hyperluminal-mem/debug
                       (log:debug "ok"))

                     (setf bits-per-word bits))

                 (simple-error (c)
                   (done c))
                 (type-error (c)
                   (done c))))))))
               

  (defun binary-search-pred (low high pred)
    "find the largest integer in range LO...(1- HI) that satisfies PRED.
Assumes that (funcall PRED LOw) = T and (funcall PRED HIGH) = NIL."
    (declare (type integer low high)
             (type function pred))

    (loop for delta = (- high low)
       while (> delta 1)
       do
         (let ((middle (+ low (ash delta -1))))
           (if (funcall pred middle)
               (setf low  middle)
               (setf high middle))))
    low)
         

  (defun find-most-positive-pred (pred)
    "find the largest positive integer that satisfies PRED."
    (declare (type function pred))

    (unless (funcall pred 1)
      (return-from find-most-positive-pred 0))

    (let ((n 1))
      (loop for next = (ash n 1)
         while (funcall pred next)
         do
           (setf n next))

      (binary-search-pred n (ash n 1) pred)))

  (defun %is-char-code? (code type)
    (declare (optimize (speed 0) (safety 3)) ;; better be safe here
             (type integer code)
             (type symbol type))

    (handler-case
        (let ((ch (code-char code)))
          (and (typep ch type)
               ;; ABCL code-char wraps at #xFFFF
               (eql code (char-code ch))))
      (condition () nil)))

  (defun %detect-most-positive-character ()
    (find-most-positive-pred (lambda (n) (%is-char-code? n 'character))))

  (defun %detect-most-positive-base-char ()
    (find-most-positive-pred (lambda (n) (%is-char-code? n 'base-char)))))





(eval-always
  (defconstant +mem-word/bits+      (%detect-bits-per-word)))
(eval-always
  (defconstant +mem-word/mask+      (1- (ash 1 +mem-word/bits+))))
(eval-always
  (defconstant +most-positive-word+ +mem-word/mask+))
(eval-always
  (defconstant +mem-byte/bits+     (truncate +mem-word/bits+ +msizeof-word+)))
(eval-always
  (defconstant +mem-byte/mask+     (1- (ash 1 +mem-byte/bits+))))
(eval-always
  (defconstant +most-positive-byte+ +mem-byte/mask+))

(eval-always
  (defconstant +mem-sfloat/bits+   (* +mem-byte/bits+ +msizeof-sfloat+)))
(eval-always
  (defconstant +mem-sfloat/mask+   (1- (ash 1 +mem-sfloat/bits+))))
(eval-always
  (defconstant +mem-dfloat/bits+   (* +mem-byte/bits+ +msizeof-dfloat+)))
(eval-always
  (defconstant +mem-dfloat/mask+   (1- (ash 1 +mem-dfloat/bits+))))
  


(eval-always
  (defconstant +most-positive-character+ (%detect-most-positive-character)))
(eval-always
  ;; round up characters to 21 bits (unicode)
  (defconstant +character/bits+          (max 21 (integer-length +most-positive-character+))))
(eval-always
  (defconstant +character/mask+          (1- (ash 1 +character/bits+))))
(eval-always
  (defconstant +characters-per-word+     (truncate +mem-word/bits+ +character/bits+)))

(eval-always
  (defconstant +most-positive-base-char+ (%detect-most-positive-base-char)))

;; for interoperability among different lisps, we must define serialized base-string
;; to mean the same everywhere. We decide it means "a string only containing ASCII characters,
;; i.e. only containing characters such that (<= 0 (char-code X) 127)"
(eval-always
  (defconstant +ascii-char/bits+          7))
(eval-always
  (defconstant +ascii-char/mask+          (1- (ash 1 +ascii-char/bits+))))
(eval-always
  (defconstant +most-positive-ascii-char+ +ascii-char/mask+))
(eval-always
  (set-feature :hlmem/base-char<=ascii (<= +most-positive-base-char+ +most-positive-ascii-char+))
  (set-feature :hlmem/base-char>=ascii (>= +most-positive-base-char+ +most-positive-ascii-char+)))


;; mem-word can be defined only now, it needs +mem-word/bits+
(eval-always
  (deftype mem-word    () `(unsigned-byte ,+mem-word/bits+)))



(eval-always
 ;; we need at least 32-bit words to store a useful amount of data
 (when (< +mem-word/bits+ 32)
   (error "cannot build HYPERLUMINAL-MEM: unsupported architecture.
size of CPU word is ~S bits, expecting at least 32 bits" +mem-word/bits+))

 ;; we support up to 21 bits for characters 
 (when (> +character/bits+ 21)
   (error "cannot build HYPERLUMINAL-MEM: unsupported architecture.
each CHARACTER contains ~S bits, expecting at most 21 bits" +character/bits+))

 (set-feature :hlmem/base-char=character (= +most-positive-base-char+ +most-positive-character+))

 #+sbcl
 ;; used on SBCL to access the internal representation of Lisp objects
 (defconstant +lisp-object-header-length+ (msizeof :pointer))

 #+sbcl
 ;; used on SBCL to access the internal representation of Lisp objects
 (defconstant +lisp-object-address-mask+ (* -2 +lisp-object-header-length+)))

























