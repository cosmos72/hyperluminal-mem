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


;; if available, use fast implementation of mget-word and mset-word
#?+hlmem/fast-mem
(eval-always
  (let ((pkg (find-package (symbol-name 'hl-asm))))

    (defconstant +fast-mread+  (intern (symbol-name +fast-mread-symbol+)  pkg))
    (defconstant +fast-mwrite+ (intern (symbol-name +fast-mwrite-symbol+) pkg)))


  (defmacro fast-mget-word (ptr index &key
                            (scale +msizeof-word+) (offset 0))
    `(,+fast-mread+ ,ptr ,index :scale ,scale :disp ,offset))

  (defmacro fast-mset-word (value ptr index &key
			    (scale +msizeof-word+) (offset 0))
    (with-gensym val
      `(let ((,val ,value))
	 (,+fast-mwrite+ ,val ,ptr ,index :scale ,scale :disp ,offset)
	 ,val))))


;; default implementation of mget-word and mset-word
(eval-always
  (defmacro %mget-t (type ptr &optional (byte-offset 0))
    `(ffi-mem-get ,ptr ,(parse-type type) ,byte-offset))

  (defmacro %mset-t (value type ptr &optional (byte-offset 0))
    `(ffi-mem-set ,value ,ptr ,(parse-type type) ,byte-offset)))

			    
(eval-always
  (defmacro mget-t (type ptr word-index)
    #?+hlmem/fast-mem
    (when (eq +chosen-word-type+ (parse-type type))
      (return-from mget-t
        `(fast-mget-word (hl-asm:sap=>fast-sap ,ptr) ,word-index)))
    ;; common case
    `(%mget-t ,type ,ptr (the #+sbcl mem-word #-sbcl t
                              (* ,word-index +msizeof-word+))))
  

  (defmacro mset-t (value type ptr word-index)
    #?+hlmem/fast-mem
    (when (eq +chosen-word-type+ (parse-type type))
      (return-from mset-t
        `(fast-mset-word ,value (hl-asm:sap=>fast-sap ,ptr) ,word-index)))
    ;; common case
    `(%mset-t ,value ,type ,ptr (the #+sbcl mem-word #-sbcl t
                                     (* ,word-index +msizeof-word+)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-always
  (defmacro mget-word (ptr word-index)
    `(mget-t :word ,ptr ,word-index))

  (defmacro mset-word (ptr word-index value)
    `(mset-t ,value :word ,ptr ,word-index))

  (defsetf mget-word mset-word))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-always
  (defmacro mget-byte (ptr byte-index)
    "Used only by MREAD-MAGIC."
    `(%mget-t :byte ,ptr ,byte-index))

  (defmacro mset-byte (ptr byte-index value)
    #-abcl "Used only by MWRITE-MAGIC and and %DETECT-ENDIANITY."
    #+abcl "Used only by MWRITE-MAGIC, and %DETECT-ENDIANITY,
!MEMSET and !MEMCPY."
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
        (typep (code-char code) type)
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
(eval-always
  ;; round up base-chars to 8 bits (iso-8859-1 or similar)
  (defconstant +base-char/bits+          (max 8 (integer-length +most-positive-base-char+))))
(eval-always
  (defconstant +base-char/mask+          (1- (ash 1 +base-char/bits+))))
(eval-always
  (defconstant +base-char/fits-byte?+    (<= +base-char/bits+ +mem-byte/bits+)))





(eval-always

 ;; we need at least a 32-bit architecture to store a useful amount of data
 (when (< +mem-word/bits+ 32)
   (error "cannot build HYPERLUMINAL-MEM: unsupported architecture.
size of CPU word is ~S bits, expecting at least 32 bits" +mem-word/bits+))

 ;; we support up to 21 bits for characters 
 (when (> +character/bits+ 21)
   (error "cannot build HYPERLUMINAL-MEM: unsupported architecture.
each CHARACTER contains ~S bits, expecting at most 21 bits" +character/bits+))

 (set-feature 'hlmem/base-char/fits-byte +base-char/fits-byte?+)
 (set-feature 'hlmem/base-char/eql/character (= +most-positive-base-char+ +most-positive-character+))

 #+sbcl
 ;; used on SBCL to access the internal representation of Lisp objects
 (defconstant +lisp-object-header-length+ (msizeof :pointer))

 #+sbcl
 ;; used on SBCL to access the internal representation of Lisp objects
 (defconstant +lisp-object-address-mask+ (* -2 +lisp-object-header-length+)))






(eval-always
  (defun %detect-endianity ()
    (with-mem-words (p 1)
      (let ((little-endian 0)
            (big-endian 0))

        (loop for i from 0 below +msizeof-word+
             for bits = (logand (1+ i) +mem-byte/mask+) do

             (setf little-endian (logior little-endian (ash bits (* i +mem-byte/bits+)))
                   big-endian    (logior bits (ash big-endian +mem-byte/bits+)))

             (mset-byte p i bits))

        (let ((endianity (mget-word p 0)))
          (unless (or (eql endianity little-endian)
                      (eql endianity big-endian))
            (error "cannot build HYPERLUMINAL-MEM: unsupported architecture.
    CPU word endianity is #x~X, expecting either #x~X (little-endian) or #x~X (big-endian)"
                   endianity little-endian big-endian))

          (defconstant +mem/little-endian+ (eql little-endian endianity))
          
          endianity)))))



(defconstant +mem-word/endianity+ (%detect-endianity))
               







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      debugging utilities       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun !mdump-bytes (stream ptr &optional (start-byte 0) (end-byte (1+ start-byte)))
  "mdump-bytes is only used for debugging. it assumes sizeof(byte) == 1"
  (declare (type maddress ptr)
           (type fixnum start-byte end-byte))
  (loop for offset from start-byte below end-byte do
       (format stream "~2,'0X" (mget-byte ptr offset))))

(defun !mdump-bytes-reverse (stream ptr &optional (start-byte 0) (end-byte (1+ start-byte)))
  "mdump-bytes-reverse is only used for debugging. it assumes sizeof(byte) == 1"
  (declare (type maddress ptr)
           (type fixnum start-byte end-byte))
  (loop for offset from end-byte above start-byte do
       (format stream "~2,'0X" (mget-byte ptr (1- offset)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun !mfill (ptr n-bytes &key (value 0) (increment 0))
  "mfill is only used for debugging. it assumes sizeof(byte) == 1 and 8 bits in a byte"
  (declare (type maddress ptr)
           (type ufixnum n-bytes)
           (type (unsigned-byte 8) value increment))
  (loop for offset from 0 below n-bytes do
       (mset-byte ptr offset value)
       (setf value (logand #xFF (+ value increment)))))


           
(defun !memset-words (ptr &optional (fill-word 0) (start-index 0) (end-index (1+ start-index)))
  "!memset-words is only used for debugging."
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type maddress ptr)
           (type mem-word fill-word)
           (type ufixnum start-index end-index))

  #?+hlmem/fast-mem
  (let* ((n-words      (the ufixnum  (- end-index start-index)))
	 (n-bulk-words (the ufixnum  (logand -8 n-words)))
	 (i            (the ufixnum  start-index))
	 (bulk-end     (the ufixnum  (+ i n-bulk-words)))
         (base         (the hl-asm:fast-sap (hl-asm:sap=>fast-sap ptr))))
    (loop while (< i bulk-end)
       do
	 (let ((i (the ufixnum i)))
	   (fast-mset-word fill-word base i :offset (* 0 +msizeof-word+))
	   (fast-mset-word fill-word base i :offset (* 1 +msizeof-word+))
	   (fast-mset-word fill-word base i :offset (* 2 +msizeof-word+))
	   (fast-mset-word fill-word base i :offset (* 3 +msizeof-word+))
	   (fast-mset-word fill-word base i :offset (* 4 +msizeof-word+))
	   (fast-mset-word fill-word base i :offset (* 5 +msizeof-word+))
	   (fast-mset-word fill-word base i :offset (* 6 +msizeof-word+))
	   (fast-mset-word fill-word base i :offset (* 7 +msizeof-word+)))
	 (incf i 8))

    (loop while (< i end-index)
       do
         (fast-mset-word fill-word base i)
         (incf i)))
    
    
  #?-hlmem/fast-mem
  (let* ((i   (logand +mem-word/mask+ (* start-index +msizeof-word+)))
         (end (logand +mem-word/mask+ (* end-index   +msizeof-word+)))
         ;; 32-bit x86 is register-starved...
         #-x86 (bulk-end (logand end (* -4 +msizeof-word+))))
    (declare (type mem-word i end #-x86 bulk-end))

    #-x86
    (loop while (< i bulk-end)
       do
         (let ((i1 (the mem-word (+ i (* 1 +msizeof-word+))))
               (i2 (the mem-word (+ i (* 2 +msizeof-word+))))
               (i3 (the mem-word (+ i (* 3 +msizeof-word+)))))
           (%mset-t fill-word :word ptr i)
           (%mset-t fill-word :word ptr i1)
           (%mset-t fill-word :word ptr i2)
           (%mset-t fill-word :word ptr i3)
           (incf i (* 4 +msizeof-word+))))
    
    (loop while (< i end)
       do
         (%mset-t fill-word :word ptr i)
         (incf i +msizeof-word+))))
         

           

(defun !hex (n)
  (format t "#x~x" n))

(defun !bin (n)
  (format t "#b~b" n))

(defun !readable (n &optional (stream t))
  "Print N in human-readable format."
  (let* ((bits (integer-length n))
         (log-1024 (truncate bits 10))
         (mantissa (ash n (* -10 (1- log-1024)))))
    (format stream "~$ * 10.08^~D" (/ (float mantissa) 1024.0) (* 3 log-1024))))

    
