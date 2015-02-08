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

(deftype x86-scale () '(member 1 2 4 8))

(defmacro x86-fixnum-scale ()
  ''#.(case +n-fixnum-tag-bits+
        (1 '(member 2 4 8 16))
        (2 '(member 4 8 16 32))
        (otherwise
         `(member ,@(loop for i in '(1 2 4 8)
                       collect (ash i +n-fixnum-tag-bits+))))))

(deftype x86-fixnum-scale () (x86-fixnum-scale))


(defun check-x86-fixnum-addressing (index scale offset)
  (let ((scale  (check-compile-constant scale))
        (offset (check-compile-constant offset)))
    (when (constantp index)
      (let ((offset (+ (* (eval index) scale) offset)))
        (when (typep offset '(signed-byte 32))
          (return-from check-x86-fixnum-addressing (values 0 +fixnum-zero-mask+1+ offset)))))
    (check-type offset (signed-byte 32))
    (check-type scale #.(x86-fixnum-scale))
    (values index scale offset)))


(defmacro define-fast-mread-mwrite (&key mread-name mwrite-name type size)

  (let ((%mread-name  (concat-symbols '% mread-name))
        (%mwrite-name (concat-symbols '% mwrite-name))
        (%mread-name-c  (concat-symbols '% mread-name '/const))
        (%mwrite-name-c (concat-symbols '% mwrite-name '/const)))

    `(progn
       (defknown ,%mread-name
           ;;arg-types
           (fast-sap fixnum x86-fixnum-scale (signed-byte 32))
           ;;result-type
           ,type
           (sb-c::flushable sb-c::important-result sb-c::always-translatable))

       (defknown ,%mwrite-name
           ;;arg-types
           (,type fast-sap fixnum x86-fixnum-scale (signed-byte 32))
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
                ;; using x86-fixnum-scale instead of x86-scale
                (index :scs (sb-vm::any-reg)))
         (:info scale offset)
         (:arg-types sb-vm::system-area-pointer sb-vm::tagged-num
                     (:constant x86-fixnum-scale)
                     (:constant (signed-byte 32)))
	      
         (:results   (r :scs (sb-vm::unsigned-reg)))
         (:result-types sb-vm::unsigned-num)

         (:generator 2
          (sb-assem:inst mov
                         #+x86 r
                         #-x86 (sb-vm::reg-in-size r ,size)
                         (sb-vm::make-ea ,size :base sap :index index
                                         :scale (ash scale (- +n-fixnum-tag-bits+))
                                         :disp offset))))

       (sb-c:define-vop (,%mread-name-c)
         (:policy :fast-safe)
         (:translate ,%mread-name)

         (:args (sap   :scs (sb-vm::sap-reg)))
         (:info index scale offset)
         (:arg-types sb-vm::system-area-pointer
                     (:constant (member 0))
                     (:constant rational)
                     (:constant (signed-byte 32)))
	      
         (:results   (r :scs (sb-vm::unsigned-reg)))
         (:result-types sb-vm::unsigned-num)

         (:generator 1
          (sb-assem:inst mov
                         #+x86 r
                         #-x86 (sb-vm::reg-in-size r ,size)
                         (sb-vm::make-ea ,size :base sap :disp offset))))

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
                ;; using x86-fixnum-scale instead of x86-scale
                (index :scs (sb-vm::any-reg)))
         (:info scale offset)
         (:arg-types sb-vm::unsigned-num sb-vm::system-area-pointer sb-vm::tagged-num
                     (:constant x86-fixnum-scale)
                     (:constant (signed-byte 32)))
	      
         (:generator 2
          (sb-assem:inst mov
                         (sb-vm::make-ea ,size :base sap :index index
                                         :scale (ash scale (- +n-fixnum-tag-bits+))
                                         :disp offset)
                         #+x86 value
                         #-x86 (sb-vm::reg-in-size value ,size))))

       (sb-c:define-vop (,%mwrite-name-c)
         (:policy :fast-safe)
         (:translate ,%mwrite-name)
         
         (:args (value :scs (sb-vm::unsigned-reg))
                (sap   :scs (sb-vm::sap-reg)))
         (:info index scale offset)
         (:arg-types sb-vm::unsigned-num sb-vm::system-area-pointer
                     (:constant (member 0))
                     (:constant rational)
                     (:constant (signed-byte 32)))
	      
         (:generator 1
          (sb-assem:inst mov
                         (sb-vm::make-ea ,size :base sap :disp offset)
                         #+x86 value
                         #-x86 (sb-vm::reg-in-size value ,size))))

       (defmacro ,mread-name (sap index
                              &key (scale +fixnum-zero-mask+1+) (offset 0))
         (multiple-value-bind (index scale offset)
             (check-x86-fixnum-addressing index scale offset)
           (list ',%mread-name sap index scale offset)))

       (defmacro ,mwrite-name (value sap index
                               &key (scale +fixnum-zero-mask+1+) (offset 0))
         (multiple-value-bind (index scale offset)
             (check-x86-fixnum-addressing index scale offset)
           (list ',%mwrite-name value sap index scale offset))))))

	   


(define-fast-mread-mwrite :mread-name fast-mread/4 :mwrite-name fast-mwrite/4
                          :type (unsigned-byte 32) :size :dword)


#+x86-64
(define-fast-mread-mwrite :mread-name fast-mread/8 :mwrite-name fast-mwrite/8
                          :type (unsigned-byte 64) :size :qword)







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
         
           (:args (x :scs (sb-vm::unsigned-reg) :target y
                     :load-if (not (sb-c::location= x y))))
           (:arg-types sb-vm::unsigned-num)
           (:results (y :scs (sb-vm::any-reg)
                        :load-if (not (sb-c::location= x y))))
           (:result-types sb-vm::tagged-num)
           (:generator 1
            (cond ((not (sb-c::location= x y))
                   (if (= +n-fixnum-tag-bits+ 1)
                       (sb-assem::inst lea y (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                                             :base x :index x))
                       (sb-assem::inst lea y (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                                             :index x
                                                             :scale (ash 1 +n-fixnum-tag-bits+)))))
                  (t
                   (sb-c::move y x)
                   (sb-assem::inst shl y +n-fixnum-tag-bits+))))))

       (eval-always
         (declaim (ftype (function (sb-ext:word) (values fixnum &optional)) ,name)
                  (inline ,name)))
       (eval-always
         (defun ,name (x)
           (declare (type sb-ext:word x))
           (the fixnum (,%name x)))))))

(define-fast-mword=>fixnum)
