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


(in-package :hyperluminal-mem-sbcl)

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

(defstruct memory-operand
  base
  offset
  direction
  mode)

(defun arm-scale=>shift (scale)
  (declare (type (or arm-scale arm-fixnum-scale) scale))
  (the arm-shift
    (if (>= scale 1)
	(- (integer-length scale) 1)
	(- 1 (integer-length (/ 1 scale))))))

(defun arm-reg-shifter (&optional (index 0) (shift 0))
  (declare (type arm-shift shift))
  (cond
    ((zerop shift) index)
    ((plusp shift) (sb-vm::lsl index shift))
    (t             (sb-vm::lsr index (- shift)))))

(defun arm-mem-shifter (register &optional (index 0) (shift 0))
  (declare (type arm-shift shift))
  (sb-vm::@ register (arm-reg-shifter index shift)))

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
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-fast-mword=>fixnum ()
  (let* ((sizeof-word (truncate (integer-length sb-ext:most-positive-word) 8))
         (name  (concat-symbols 'fast-mword/ sizeof-word '=>fixnum))
         (%name (concat-symbols '% name)))
    
    `(progn
       (eval-always
         (defknown ,%name
             ;;arg-types
             (word)
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
         (declaim (ftype (function (word) (values fixnum &optional)) ,name)
                  (inline ,name)))
       (eval-always
         (defun ,name (x)
           (declare (type word x))
           (the fixnum (,%name x)))))))

(define-fast-mword=>fixnum)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defknown %fast-sap+
    (fast-sap fixnum arm-fixnum-shift arm-offset)
    (fast-sap)
    (sb-c::flushable sb-c::foldable sb-c::movable sb-c::always-translatable))

(sb-c:define-vop (%fast-sap+)
  (:policy :fast-safe)
  (:translate %fast-sap+)
  (:args (sap   :scs (sb-vm::sap-reg))
	 (index :scs (sb-vm::any-reg)))
  (:info shift offset)
  (:arg-types sb-sys:system-area-pointer sb-vm::tagged-num
	      (:constant arm-fixnum-shift)
	      (:constant (member 0)))

  (:results   (r :scs (sb-vm::sap-reg)))
  (:result-types sb-sys:system-area-pointer)

  (:generator
   2
   (sb-assem:inst add r sap (arm-reg-shifter
			     index (fixnum- shift +n-fixnum-tag-bits+)))))

(sb-c:define-vop (%fast-sap+-c)
  (:policy :fast-safe)
  (:translate %fast-sap+)

  (:args (sap   :scs (sb-vm::sap-reg)))
  (:info index shift offset)
  (:arg-types sb-vm::system-area-pointer
	      (:constant (member 0))
	      (:constant integer)
	      (:constant arm-offset))
	      
  (:results   (r :scs (sb-vm::sap-reg)))
  (:result-types sb-sys:system-area-pointer)

  (:generator
   1
   (sb-assem:inst add r sap (arm-reg-shifter offset))))

(defmacro fast-sap+ (sap index &key (scale +fixnum-zero-mask+1+) (offset 0))
  (multiple-value-bind (index shift offset)
      (check-arm-fixnum-addressing index scale offset)
    `(%fast-sap+ ,sap ,index ,shift ,offset)))


(defun emit-bulk-transfer (kind tn n-words reg-list)
  (check-type n-words (integer 1 8))
   
  (let ((reg-bitmap 0))
    (declare (type word reg-bitmap))
    (dotimes (i n-words)
      (let ((reg (pop reg-list)))
	(setf reg-bitmap (logior reg-bitmap
				 (the word (ash 1 (sb-vm::tn-offset reg)))))))

    ;; 31-28: COND = :always = :al = 14 = #xe
    ;;
    ;; ldmia tn!, {reg-bitmap}
    ;; 27-20: #b100 P=0 U=1 S=0 W=1 L=1 = #b10001011 = #x8b
    ;; stmia tn!, {reg-bitmap}
    ;; 27-20: #b100 P=0 U=1 S=0 W=1 L=0 = #b10001010 = #x8a
    ;;
    ;; 19-16: tn
    ;; 15-00: subset of {r0-r6,r8} = reg-bitmap
    (sb-vm::emit-word (sb-assem::%%current-segment%%)
		      (logior #xe0000000
			      (the word
				(ecase kind (:store #x08a00000) (:load #x08b00000)))
			      (the word (ash (sb-vm::tn-offset tn) 16))
			      reg-bitmap))))

(defun emit-ldmia (tn n-words reg-list)
  (emit-bulk-transfer :load tn n-words reg-list))

(defun emit-stmia (tn n-words reg-list)
  (emit-bulk-transfer :store tn n-words reg-list))


;;;; copied from SBCL sources sbcl.git/src/compiler/arm/insts.lisp
(sb-assem::define-bitfield-emitter emit-dp-instruction 32
  (byte 4 28) (byte 2 26) (byte 1 25) (byte 5 20)
  (byte 4 16) (byte 4 12) (byte 12 0))

#-(and) ;; unfinished
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    ;; DEFINE-ARG-TYPE requires that any :PRINTER be defined at
    ;; compile-time...  Why?
    (defun print-pld (value stream dstate)
      (declare (type stream stream)
               (ignore dstate))
      (princ "pld " stream)
      (princ value stream)))

  (sb-disassem:define-instruction-format
      (pld 32
           :default-printer '(:name :tab "[" rn ", " rm shift "]"))
      (cond       :field (byte 4 28) :value #xf)
    (opcode-1   :field (byte 4 25) :value #x7)
    (up-p       :field (byte 1 23))
    (read-p     :field (byte 1 22))
    (rn         :field (byte 4 16) :type 'sb-vm::reg)
    (opcode-2   :field (byte 4 12) :value #b1111)
    (shift      :field (list (byte 5 7) (byte 2 5)) :type 'sb-vm::immediate-shift)
    (register-shift-p :field (byte 1 4) :value 0)
    (rm :field (byte 4 0) :type 'sb-vm::reg)))

(defun emit-pld (address &optional (segment (sb-assem::%%current-segment%%)))
  "Emit cache-preload instruction: PLD register [,offset]"
  (flet ((compute-opcode (direction)
           (if (eq direction :down) #b10101 #b11101)))
    (sb-impl::aver (typep address 'memory-operand))
    (let* ((base      (memory-operand-base      address))
           (offset    (memory-operand-offset    address))
           (direction (memory-operand-direction address))
           (mode      (memory-operand-mode      address))
           (cond-bits #xf))
      (sb-impl::aver (eq mode :offset))
      (sb-impl::aver (integerp offset))
      (sb-impl::aver (typep offset '(unsigned-byte 12)))
      (emit-dp-instruction segment cond-bits #b01 1
                           (compute-opcode direction)
                           (sb-c::tn-offset base) #xf
                           offset))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defknown %memcpy/4
    (fast-sap fast-sap word)
    (values)
    (sb-c:always-translatable))

(sb-c:define-vop (%memcpy-0/4)
  (:policy :fast-safe)
  (:translate %memcpy/4)
  (:args (dst        :scs (sb-vm::sap-reg))
	 (src        :scs (sb-vm::sap-reg)))
  (:info n-words)
  (:arg-types sb-sys:system-area-pointer
	      sb-sys:system-area-pointer
	      (:constant (integer 0 0)))
  (:generator
   0
   nil))

(sb-c:define-vop (%memcpy-1-4/4)
  (:policy :fast-safe)
  (:translate %memcpy/4)
  (:args (dst        :scs (sb-vm::sap-reg))
	 (src        :scs (sb-vm::sap-reg)))
  (:info n-words)
  (:arg-types sb-sys:system-area-pointer
	      sb-sys:system-area-pointer
	      (:constant (integer 1 4)))
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r0-offset)     r0)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r1-offset)     r1)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r2-offset)     r2)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::lexenv-offset) r3)
  (:generator
   16
   (let ((regs (list r0 r1 r2 r3)))
     (emit-ldmia src n-words regs)
     (emit-stmia dst n-words regs))))

(sb-c:define-vop (%memcpy-5-64/4)
  (:policy :fast-safe)
  (:translate %memcpy/4)
  (:args (dst        :scs (sb-vm::sap-reg))
	 (src        :scs (sb-vm::sap-reg)))
  (:info n-words)
  (:arg-types sb-sys:system-area-pointer
	      sb-sys:system-area-pointer
	      (:constant (integer 5 64)))
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r0-offset)     r0)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r1-offset)     r1)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r2-offset)     r2)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::lexenv-offset) r3)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::nl2-offset)    r4)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::code-offset)   r5)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::nl3-offset)    r6)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r8-offset)     r8)
  (:generator
   32
   (let ((regs (list r0 r1 r2 r3 r4 r5 r6 r8)))
     (declare (type (integer 5 64) n-words))
     (loop for n-left fixnum = n-words then (- n-left n)
	for n fixnum = (min n-left 8)
	while (plusp n-left)
	do
	  (emit-ldmia src n regs)
	  (emit-stmia dst n regs)))))

(sb-c:define-vop (%memcpy-n/4)
  (:policy :fast-safe)
  (:translate %memcpy/4)
  (:args (dst        :scs (sb-vm::sap-reg))
	 (src        :scs (sb-vm::sap-reg))
	 (n-words    :scs (sb-vm::unsigned-reg)))
  (:arg-types sb-sys:system-area-pointer
	      sb-sys:system-area-pointer
	      sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r0-offset)     r0)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r1-offset)     r1)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r2-offset)     r2)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::lexenv-offset) r3)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::nl2-offset)    r4)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::code-offset)   r5)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r8-offset)     r8)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::null-offset)   r10)
  (:generator
   48
   ;; save NULL
   (sb-assem:inst str r10 (sb-vm::@ sb-vm::nsp-tn (- sb-vm::n-word-bytes)))
   (sb-assem:inst b loop8-test)
   loop8
   (let ((regs (list r0 r1 r2 r3 r4 r5 r8 r10)))
     (emit-ldmia src 8 regs)
     (emit-pld
      (make-memory-operand :base src :offset 256
                           :direction :up :mode :offset))
     (emit-stmia dst 8 regs)
     (sb-assem:inst sub n-words n-words 8))
   loop8-test
   (sb-assem:inst cmp n-words 8)
   (sb-assem:inst b :ge loop8)
   (sb-assem:inst b loop1-test)

   loop1
   (let ((regs (list r0)))
     (emit-ldmia src 1 regs)
     (emit-stmia dst 1 regs)
     (sb-assem:inst sub n-words n-words 1))
   loop1-test
   (sb-assem:inst cmp n-words 1)
   (sb-assem:inst b :ge loop1)
   ;; restore NULL
   (sb-assem:inst ldr r10 (sb-vm::@ sb-vm::nsp-tn (- sb-vm::n-word-bytes)))))



(defmacro fast-memcpy/4 (dst dst-index src src-index n-words &key
			 (dst-scale +fixnum-zero-mask+1+) (dst-offset 0)
			 (src-scale +fixnum-zero-mask+1+) (src-offset 0))
  (with-gensyms (dsap ssap)
    `(let ((,dsap (fast-sap+ ,dst ,dst-index :scale ,dst-scale :offset ,dst-offset))
	   (,ssap (fast-sap+ ,src ,src-index :scale ,src-scale :offset ,src-offset)))

       (%memcpy/4 ,dsap ,ssap ,n-words))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defknown %memset/4
    (fast-sap fixnum word)
    (values)
    (sb-c:always-translatable))

(sb-c:define-vop (%memset-1-7/4)
  (:policy :fast-safe)
  (:translate %memset/4)
  (:args (sap        :scs (sb-vm::sap-reg))
         (fill-word  :scs (sb-vm::unsigned-reg)))
  (:info n-words)
  (:arg-types sb-sys:system-area-pointer
              sb-vm::unsigned-num
	      (:constant (integer 1 7)))
  (:generator
   16
   (dotimes (i (the (integer 1 7) n-words))
     (sb-assem:inst str fill-word (arm-mem-shifter sap (* i 4))))))

(sb-c:define-vop (%memset-n/4)
  (:policy :fast-safe)
  (:translate %memset/4)
  (:args (sap        :scs (sb-vm::sap-reg))
	 (fill-word  :scs (sb-vm::unsigned-reg) :target r0)
	 (n-words    :scs (sb-vm::unsigned-reg)))
  (:arg-types sb-sys:system-area-pointer
	      sb-vm::unsigned-num
	      sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r0-offset)     r0)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r1-offset)     r1)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r2-offset)     r2)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::lexenv-offset) r3)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::nl2-offset)    r4)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::code-offset)   r5)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::r8-offset)     r8)
  (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::null-offset)   r10)
  (:generator
   32
   ;; save NULL
   (sb-assem:inst str r10 (sb-vm::@ sb-vm::nsp-tn (- sb-vm::n-word-bytes)))
   (dolist (reg (list r0 r1 r2 r3 r4 r5 r8 r10))
     (sb-c::move reg fill-word))
   (sb-assem:inst b loop8-test)
   loop8
   (let ((regs (list r0 r1 r2 r3 r4 r5 r8 r10)))
     (emit-stmia sap 8 regs)
     (sb-assem:inst sub n-words n-words 8))
   loop8-test
   (sb-assem:inst cmp n-words 8)
   (sb-assem:inst b :ge loop8)
   (sb-assem:inst b loop1-test)

   loop1
   (let ((regs (list r0)))
     (emit-stmia sap 1 regs)
     (sb-assem:inst sub n-words n-words 1))
   loop1-test
   (sb-assem:inst cmp n-words 1)
   (sb-assem:inst b :ge loop1)
   ;; restore NULL
   (sb-assem:inst ldr r10 (sb-vm::@ sb-vm::nsp-tn (- sb-vm::n-word-bytes)))))

(defmacro fast-memset/4 (ptr index n-words fill-word &key
			 (scale +fixnum-zero-mask+1+) (offset 0))
  (with-gensyms (sap n)
    `(let ((,sap (fast-sap+ ,ptr ,index :scale ,scale :offset ,offset))
           (,n ,n-words))
       (%memset/4 ,sap ,fill-word ,n))))
