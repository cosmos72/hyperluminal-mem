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


(in-package :hyperluminal-sbcl)

(deftype arm-offset () '(integer -4095 4095))

(deftype arm-shift () '(integer -32 31))
(deftype arm-scale ()
  `(member ,@(loop for i from -32 to 31
		collect (if (plusp i)
			    (ash 1 i)
			    (/ 1 (ash 1 (- i)))))))

(deftype arm-fixnum-shift () `(integer ,(+ -32 +n-fixnum-tag-bits+) ,(+ 31 +n-fixnum-tag-bits+)))
(deftype arm-fixnum-scale ()
  `(member ,@(loop for i from (+ -32 +n-fixnum-tag-bits+) to (+ 31 +n-fixnum-tag-bits+)
		collect (if (plusp i)
			    (ash 1 i)
			    (/ 1 (ash 1 (- i)))))))

(defun arm-scale=>shift (scale)
  (declare (type (or arm-scale arm-fixnum-scale) scale))
  (the arm-shift
    (if (>= scale 1)
	(- (integer-length scale) 1)
	(- 1 (integer-length (/ 1 scale))))))

(defun arm-mem-shifter (register &optional (index 0) (shift 0))
  (declare (type arm-shift shift))
  (cond
    ((zerop shift) (sb-vm::@ register index))
    ((plusp shift) (sb-vm::@ register (sb-vm::lsl index shift)))
    (t             (sb-vm::@ register (sb-vm::lsr index (- shift))))))

(defun encode-arm-large-offset (offset)
  ;; allow exceeding fixnum, but not addressable memory
  (check-type offset (integer #.(- sb-ext:most-positive-word)
			      #.sb-ext:most-positive-word))

  (let ((shift 0))
    (declare (type arm-shift shift))
    ;; try to use large shift to avoid overflows
    (loop while (and (evenp offset) (< shift 31))
       do
	 (setf offset (ash offset -1)
	       shift (1+ shift)))
    (check-type offset fixnum)
    (values offset shift 0)))


;; try to use large shift to avoid overflows
(defun encode-arm-index+shift+large-offset (index index-shift offset)
  (check-type index-shift arm-shift)
  (check-type offset fixnum)

  (multiple-value-bind (offset offset-shift) (encode-arm-large-offset offset)
    (declare (type arm-shift offset-shift))
    (let* ((min-shift    (the arm-shift (min index-shift offset-shift)))
	   (delta-index  (the arm-shift (- index-shift  min-shift)))
	   (delta-offset (the arm-shift (- offset-shift min-shift))))
      
      (values `(+ ,(if (zerop delta-index)  index `(ash ,index ,delta-index))
		  ,(if (zerop delta-offset) offset (ash offset  delta-offset)))
	      min-shift 0))))

    

(defun check-arm-fixnum-addressing (index fixnum-scale offset)
  "Return a triplet (values index shift offset)
suitable for LDR and STR addressing modes"
  (let* ((fixnum-scale (check-compile-constant fixnum-scale))
	 (offset  (check-compile-constant offset)))

    (check-type fixnum-scale arm-fixnum-scale)
    (check-type offset integer)

    (when (constantp index)
      (let ((offset (+ (* (eval index) fixnum-scale) offset)))
	(return-from check-arm-fixnum-addressing
	  (if (typep offset 'arm-offset)
	      ;; encode as immediate offset
	      (values 0 0 offset)
	      (encode-arm-large-offset offset)))))

    (let ((fixnum-shift (arm-scale=>shift fixnum-scale)))
      ;; ARM instructions LDR and STR do not support simultaneous index and offset
      ;; but scale is quite flexible
      (if (zerop offset)
	  (values index fixnum-shift 0)
	  ;; worst case: work around non-zero offset
	  (encode-arm-index+shift+large-offset index fixnum-shift offset)))))
	    

(defmacro define-fast-mread-mwrite (&key mread-name mwrite-name type size)

  (let ((%mread-name  (concat-symbols '% mread-name))
        (%mwrite-name (concat-symbols '% mwrite-name))
	(%mread-name-c  (concat-symbols '% mread-name '/const))
	(%mwrite-name-c (concat-symbols '% mwrite-name '/const)))

    `(progn
       (defknown ,%mread-name
           ;;arg-types
           (fast-sap fixnum arm-shift arm-offset)
           ;;result-type
           ,type
           (sb-c::flushable sb-c::important-result sb-c::always-translatable))

       (defknown ,%mwrite-name
           ;;arg-types
           (,type fast-sap fixnum arm-shift arm-offset)
           ;;result-type
           (values)
           (sb-c::always-translatable))

       (sb-c:define-vop (,%mread-name)
         (:policy :fast-safe)
         (:translate ,%mread-name)

         (:args (sap   :scs (sb-vm::sap-reg))
                ;; directly use a tagged FIXNUM as INDEX... on SBCL
                ;; its representation is shifted by +n-fixnum-tag-bits+
                ;; which means that INDEX is effectively shifted
                ;; by that many bits. This discrepancy is solved by
		;; changing the allowed values of scale, i.e.
                ;; using arm-fixnum-shift instead of arm-shift
                (index :scs (sb-vm::any-reg)))
         (:info shift offset)
         (:arg-types sb-vm::system-area-pointer sb-vm::tagged-num
                     (:constant arm-fixnum-shift)
		     (:constant (member 0)))

         (:results   (r :scs (sb-vm::unsigned-reg)))
         (:result-types sb-vm::unsigned-num)

         (:generator 2
          (sb-assem:inst ldr r (arm-mem-shifter sap index (- shift +n-fixnum-tag-bits+)))))

       (sb-c:define-vop (,%mread-name-c)
         (:policy :fast-safe)
         (:translate ,%mread-name)

         (:args (sap   :scs (sb-vm::sap-reg)))
         (:info index shift offset)
         (:arg-types sb-vm::system-area-pointer
                     (:constant (member 0))
                     (:constant integer)
                     (:constant arm-offset))
	      
         (:results   (r :scs (sb-vm::unsigned-reg)))
         (:result-types sb-vm::unsigned-num)

         (:generator 1
          (sb-assem:inst ldr r (arm-mem-shifter sap offset))))

       (sb-c:define-vop (,%mwrite-name)
         (:policy :fast-safe)
         (:translate ,%mwrite-name)
         
         (:args (value :scs (sb-vm::unsigned-reg))
                (sap   :scs (sb-vm::sap-reg))
                ;; directly use a tagged FIXNUM as INDEX... on SBCL
                ;; its representation is shifted by +n-fixnum-tag-bits+
                ;; which means that INDEX is effectively shifted
                ;; by that many bits. This discrepancy is solved by
		;; changing the allowed values of scale, i.e.
                ;; using arm-fixnum-shift instead of arm-shift
                (index :scs (sb-vm::any-reg)))
         (:info shift offset)
         (:arg-types sb-vm::unsigned-num sb-vm::system-area-pointer sb-vm::tagged-num
                     (:constant arm-fixnum-shift)
		     (:constant (member 0)))
         (:generator 2
	  (sb-assem:inst str value (arm-mem-shifter sap index (- shift +n-fixnum-tag-bits+)))))

       (sb-c:define-vop (,%mwrite-name-c)
         (:policy :fast-safe)
         (:translate ,%mwrite-name)
         
         (:args (value :scs (sb-vm::unsigned-reg))
                (sap   :scs (sb-vm::sap-reg)))
         (:info index shift offset)
         (:arg-types sb-vm::unsigned-num sb-vm::system-area-pointer
                     (:constant (member 0))
                     (:constant integer)
                     (:constant arm-offset))
	      
         (:generator 1
          (sb-assem:inst str value (arm-mem-shifter sap offset))))

       (defmacro ,mread-name (sap index
                              &key (scale +fixnum-zero-mask+1+) (offset 0))
         (multiple-value-bind (index shift offset)
             (check-arm-fixnum-addressing index scale offset)
	   (list ',%mread-name sap index shift offset)))

       (defmacro ,mwrite-name (value sap index
                               &key (scale +fixnum-zero-mask+1+) (offset 0))
         (multiple-value-bind (index shift offset)
             (check-arm-fixnum-addressing index scale offset)
	   (list ',%mwrite-name value sap index shift offset))))))
	       


(define-fast-mread-mwrite :mread-name fast-mread/4 :mwrite-name fast-mwrite/4
                          :type (unsigned-byte 32) :size :long)


(defmacro define-fast-mword=>fixnum ()
  (let* ((sizeof-word (truncate (integer-length sb-ext:most-positive-word) 8))
         (name  (concat-symbols 'fast-mword/ sizeof-word '=>fixnum))
         (%name (concat-symbols '% name)))
    
    `(progn
       (eval-always
         (defknown ,%name
             ;;arg-types
             (sb-ext:word)
             ;;result-type
             fixnum
             (sb-c::flushable sb-c::foldable sb-c::movable sb-c::always-translatable)))

       (eval-always
         (sb-c:define-vop (,%name)
           (:policy :fast-safe)
           (:translate ,%name)
         
           (:args (x :scs (sb-vm::unsigned-reg)))
           (:arg-types sb-vm::unsigned-num)
           (:results (y :scs (sb-vm::any-reg)))
           (:result-types sb-vm::tagged-num)
           (:generator 1
	    (sb-assem::inst mov y (sb-vm::lsl x +n-fixnum-tag-bits+)))))

       (eval-always
         (declaim (ftype (function (sb-ext:word) (values fixnum &optional)) ,name)
                  (inline ,name)))
       (eval-always
         (defun ,name (x)
           (declare (type sb-ext:word x))
           (the fixnum (,%name x)))))))

(define-fast-mword=>fixnum)
