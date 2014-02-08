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


(in-package :hyperluminal-mem)

(deftype ufixnum () '(and fixnum (integer 0)))
(deftype maddress () 'cffi-sys:foreign-pointer)


(define-constant-once +null-pointer+ (cffi-sys:null-pointer))



(declaim (inline null-pointer?))

(defun null-pointer? (ptr)
  (declare (type maddress ptr))
  (cffi-sys:null-pointer-p ptr))


(eval-when (:compile-toplevel :load-toplevel)

  (defun expr-is-constant? (expr)
    (or (keywordp expr)
        (and (consp expr)
             (eq 'quote (first expr)))))

  (defun unquote (expr)
    (if (and (consp expr)
             (eq 'quote (first expr)))
        (second expr)
        expr))

  (declaim (type keyword +chosen-word-type+))
  (defconstant +chosen-word-type+ (choose-word-type))

  (defun parse-type (type)
    (case type
      (:sfloat :float)         ;; this is the ONLY code mapping :sfloat to a CFFI type
      (:dfloat :double)        ;; this is the ONLY code mapping :dfloat to a CFFI type
      (:byte   :unsigned-char) ;; this is the ONLY code mapping :byte to a CFFI type
      (:word   +chosen-word-type+) ;; :word is mapped to a CFFI type by (choose-word-type)
      (otherwise type))))


;; not really used, but handy
#-(and)
(eval-always
 (cffi:defctype mfloat  #.(parse-type :sfloat))
 (cffi:defctype mdouble #.(parse-type :dfloat))
 (cffi:defctype mbyte   #.(parse-type :byte))
 (cffi:defctype mword   #.(parse-type :word)))



(defmacro %msizeof (type)
  "Wrapper for (CFFI-SYS:%FOREIGN-TYPE-SIZE), interprets :SFLOAT :DFLOAT :BYTE AND :WORD"
  `(cffi-sys:%foreign-type-size ,(if (expr-is-constant? type)
                                     (parse-type type)
                                     `(parse-type ,type))))

(defmacro msizeof (type)
  "Wrapper for (%MSIZEOF), computes (CFFI:FOREIGN-TYPE-SIZE) at compile time whenever possible"
  (if (expr-is-constant? type)
      (%msizeof (unquote type))
      `(%msizeof ,type)))





(defconstant +msizeof-sfloat+  (msizeof :sfloat))
(defconstant +msizeof-dfloat+  (msizeof :dfloat))
(defconstant +msizeof-byte+    (msizeof :byte))
(defconstant +msizeof-word+    (msizeof :word))


(defmacro %mget-t (type ptr &optional (offset 0))
  `(cffi-sys:%mem-ref ,ptr ,(parse-type type) ,offset))

(defmacro %mset-t (value type ptr &optional (offset 0))
  `(cffi-sys:%mem-set ,value ,ptr ,(parse-type type) ,offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mget-t (type ptr word-index)
  `(%mget-t ,type ,ptr (logand +mem-word/mask+ (* ,word-index +msizeof-word+))))

(defmacro mset-t (value type ptr word-index)
  `(%mset-t ,value ,type ,ptr (logand +mem-word/mask+ (* ,word-index +msizeof-word+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition unsupported-arch (simple-error)
    ()))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun cffi-type-name (sym)
    (declare (type symbol sym))
    (string-downcase (symbol-name (parse-type sym))))

  ;; (msizeof :byte) must be 1
  (when (/= +msizeof-byte+ 1)
    (error "cannot build HYPERLUMINAL-DB: unsupported architecture.
size of ~S is ~S bytes, expecting exactly 1 byte"
           (cffi-type-name :byte) +msizeof-byte+))


  ;; we need at least a 32-bit architecture
  (when (< +msizeof-word+ 4)
    (error "cannot build HYPERLUMINAL-DB: unsupported architecture.
size of ~S is ~S bytes, expecting at least 4 bytes"
           (cffi-type-name :byte) +msizeof-word+))

  ;; determine number of bits per CPU word
  (defun %detect-bits-per-word ()
    (declare (optimize (speed 0) (safety 3))) ;; ABSOLUTELY NECESSARY!

    (let ((bits-per-word 1))
    
      (cffi-sys:with-foreign-pointer (p +msizeof-word+)
        (loop for i = 1 then (logior (ash i 1) 1)
           for bits = 1 then (1+ bits)
           do
             (handler-case
                 (progn
                   (%mset-t i :word p)
                   
                   #+hyperluminal-db/debug
                   (log:debug "(i #x~X) (bits ~D) ..." i bits)
                 
                   (let ((j (%mget-t :word p)))
                     #+hyperluminal-db/debug
                     (log:debug " read back: #x~X ..." j)
                     
                     (unless (eql i j)
                       (error "reading value '~S' stored in a CPU word returned '~S'" i j))
                   
                     #+hyperluminal-db/debug
                     (log:debug "ok"))

                   (setf bits-per-word bits))

               (condition ()
                 (return-from %detect-bits-per-word bits-per-word)))))))


  (defun binary-search-pred (low high pred)
    "find the largest integer in range LO...(1- HI) that satisfies PRED.
Assumes that (funcall PRED LOw) = T and (funcall PRED HIGH) = NIL."
    (declare (type integer low high)
             (type function pred))

    (loop for delta = (- high low)
       while (> delta 1)
       for middle = (+ low (ash delta -1))
       do
         (if (funcall pred middle)
             (setf low  middle)
             (setf high middle)))
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






(defconstant +mem-word/bits+      (%detect-bits-per-word))
(defconstant +mem-word/mask+      (1- (ash 1 +mem-word/bits+)))
(defconstant +most-positive-word+ +mem-word/mask+)

(defconstant +mem-byte/bits+     (truncate +mem-word/bits+ +msizeof-word+))
(defconstant +mem-byte/mask+     (1- (ash 1 +mem-byte/bits+)))
(defconstant +most-positive-byte+ +mem-byte/mask+)

(defconstant +most-positive-character+ (%detect-most-positive-character))
;; round up characters to 21 bits (unicode)
(defconstant +character/bits+          (max 21 (integer-length +most-positive-character+)))
(defconstant +character/mask+          (1- (ash 1 +character/bits+)))
(defconstant +characters-per-word+     (truncate +mem-word/bits+ +character/bits+))


(defconstant +most-positive-base-char+ (%detect-most-positive-base-char))
;; round up base-chars to 8 bits (iso-8859-1 or similar)
(defconstant +base-char/bits+          (max 8 (integer-length +most-positive-base-char+)))
(defconstant +base-char/mask+          (1- (ash 1 +base-char/bits+)))
(defconstant +base-char/fits-byte?+    (<= +base-char/bits+ +mem-byte/bits+))





(eval-always

 ;; we need at least a 32-bit architecture to store a useful amount of data
 (when (< +mem-word/bits+ 32)
   (error "cannot build HYPERLUMINAL-DB: unsupported architecture.
size of CPU word is ~S bits, expecting at least 32 bits" +mem-word/bits+))

 ;; we support up to 21 bits for characters 
 (when (> +character/bits+ 21)
   (error "cannot build HYPERLUMINAL-DB: unsupported architecture.
each CHARACTER contains ~S bits, expecting at most 21 bits" +character/bits+))

 (set-feature 'hldb/base-char/fits-byte +base-char/fits-byte?+)
 (set-feature 'hldb/base-char/eql/character (= +most-positive-base-char+ +most-positive-character+))

 #+sbcl
 ;; used on SBCL to access the internal representation of Lisp objects
 (defconstant +lisp-object-header-length+ (msizeof :pointer))

 #+sbcl
 ;; used on SBCL to access the internal representation of Lisp objects
 (defconstant +lisp-object-address-mask+ (* -2 +lisp-object-header-length+)))






(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %detect-endianity ()
    (cffi-sys:with-foreign-pointer (p +msizeof-word+)
      (let ((little-endian 0)
            (big-endian 0))

        (loop for i from 0 below +msizeof-word+
             for bits = (logand (1+ i) +mem-byte/mask+) do

             (setf little-endian (logior little-endian (ash bits (* i +mem-byte/bits+)))
                   big-endian    (logior bits (ash big-endian +mem-byte/bits+)))

             (%mset-t bits :byte p i))

        (let ((endianity (%mget-t :word p)))
          (unless (or (eql endianity little-endian)
                      (eql endianity big-endian))
            (error "cannot build HYPERLUMINAL-DB: unsupported architecture.
    CPU word endianity is #x~X, expecting either #x~X (little-endian) or #x~X (big-endian)"
                   endianity little-endian big-endian))

          (defconstant +mem/little-endian+ (eql little-endian endianity))
          
          endianity)))))



(defconstant +mem-word/endianity+ (%detect-endianity))
               






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mget-word (ptr word-index)
  `(mget-t :word ,ptr ,word-index))

(defmacro mset-word (ptr word-index value)
  `(mset-t ,value :word ,ptr ,word-index))

(defsetf mget-word mset-word)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mget-byte (ptr byte-index)
  "Used only by MREAD-MAGIC."
  `(%mget-t :byte ,ptr ,byte-index))

(defmacro mset-byte (ptr byte-index value)
  "Used only by MWRITE-MAGIC."
  `(%mset-t ,value :byte ,ptr ,byte-index))

(defsetf mget-byte mset-byte)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      debugging utilities       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun !mdump-bytes (stream ptr &optional (start-byte 0) (end-byte (1+ start-byte)))
  "mdump-bytes is only used for debugging. it assumes sizeof(byte) == 1"
  (declare (type maddress ptr)
           (type fixnum start-byte end-byte))
  (loop for offset from start-byte below end-byte do
       (format stream "~2,'0X" (%mget-t :byte ptr offset))))

(defun !mdump-bytes-reverse (stream ptr &optional (start-byte 0) (end-byte (1+ start-byte)))
  "mdump-bytes-reverse is only used for debugging. it assumes sizeof(byte) == 1"
  (declare (type maddress ptr)
           (type fixnum start-byte end-byte))
  (loop for offset from end-byte above start-byte do
       (format stream "~2,'0X" (%mget-t :byte ptr (1- offset)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun !mfill (ptr n-bytes &key (value 0) (increment 0))
  "mfill is only used for debugging. it assumes sizeof(byte) == 1 and 8 bits in a byte"
  (declare (type maddress ptr)
           (type ufixnum n-bytes)
           (type (unsigned-byte 8) value increment))
  (loop for offset from 0 below n-bytes do
       (%mset-t value :byte ptr offset)
       (setf value (logand #xFF (+ value increment)))))


(declaim (notinline !memset !memcpy)
	 (inline !mzero))
           

(defun !memset (ptr fill-byte start-byte end-byte)
  (declare (type maddress ptr)
           (type (unsigned-byte 8) fill-byte)
           (type ufixnum start-byte end-byte))
  (osicat-posix:memset (if (zerop start-byte)
                           ptr
                           (cffi-sys:inc-pointer ptr start-byte))
                       fill-byte
                       (- end-byte start-byte))
  nil)

(defun !mzero (ptr start-byte end-byte)
  (declare (type maddress ptr)
           (type ufixnum start-byte end-byte))
  (!memset ptr 0 start-byte end-byte))
           

(defun !mzero-words (ptr &optional (start-index 0) (end-index (1+ start-index)))
  "!mzero-words is only used for debugging."
  (declare (type maddress ptr)
           (type ufixnum start-index end-index))
        
  (loop for index from start-index below end-index
       do (mset-word ptr index 0)))


(defun !memset-words (ptr fill-word &optional (start-index 0) (end-index (1+ start-index)))
  "!memset-words is only used for debugging."
  (declare (type maddress ptr)
	   (type mem-word fill-word)
           (type ufixnum start-index end-index))
        
  (loop for index from start-index below end-index
       do (mset-word ptr index fill-word)))



(defun !memcpy (dst src n-bytes)
  (declare (type maddress dst src)
           (type ufixnum n-bytes))
  (osicat-posix:memcpy dst src n-bytes))


(declaim (inline memcpy-words))

(defun memcpy-words (dst dst-index src src-index n-words)
  (declare (type maddress dst src)
           (type ufixnum dst-index src-index n-words))
  (loop for i from 0 below n-words
     do (mset-word dst (the ufixnum (+ dst-index i))
                   (mget-word src (the ufixnum (+ src-index i))))))
  
           
(declaim (inline malloc mfree))

(defun malloc (n-bytes)
  "Allocate N-BYTES of raw memory and return raw pointer to it.
The obtained memory must be freed manually: call MFREE on it when no longer needed."
  (cffi-sys:%foreign-alloc n-bytes))

(defun mfree (ptr)
  "Deallocate a block of raw memory previously obtained with MALLOC."
  (cffi-sys:foreign-free ptr))


(defmacro with-mem-words ((ptr n-words &optional n-words-var) &body body)
  "Bind PTR to N-WORDS words of raw memory while executing BODY.
Raw memory is automatically deallocated when BODY terminates."

  (when (and (null n-words-var) (constantp n-words))
    (return-from with-mem-words
      `(cffi-sys:with-foreign-pointer (,ptr ,(eval (* n-words +msizeof-word+)))
         ,@body)))
      
  (unless n-words-var
    (setf n-words-var (gensym (symbol-name 'n-words))))
  
  `(let ((,n-words-var (the mem-size ,n-words)))
     (cffi-sys:with-foreign-pointer
         (,ptr
          ,(if (constantp n-words)
               (eval `(* ,n-words +msizeof-word+))
               `(the mem-word (* ,n-words-var +msizeof-word+))))
       ,@body)))


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

    