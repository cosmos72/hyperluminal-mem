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
  "Return a triplet (values index scale offset)
suitable for MOV addressing modes"
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
       (declaim (inline ,%mread-name))
       (defknown ,%mread-name
           ;;arg-types
           (fast-sap fixnum x86-fixnum-scale (signed-byte 32))
           ;;result-type
           ,type
           (sb-c::flushable sb-c::important-result sb-c::always-translatable))

       (declaim (inline ,%mwrite-name))
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
                                         :scale (ash (the fixnum scale) (- +n-fixnum-tag-bits+))
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
                                         :scale (ash (the fixnum scale) (- +n-fixnum-tag-bits+))
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




(defmacro define-fast-memcpy (&key memcpy-name type size)
  (let ((%memcpy-name (concat-symbols '% memcpy-name)))
    `(progn
       (eval-always
         (declaim (inline ,%memcpy-name)))

       (eval-always
         (defknown ,%memcpy-name
             (fast-sap fixnum fast-sap fixnum sb-ext:word
                       x86-fixnum-scale (signed-byte 32)
                       x86-fixnum-scale (signed-byte 32))
             (values)
             (sb-c:always-translatable)))

       (eval-always
         (sb-c:define-vop (,%memcpy-name)
           (:policy :fast-safe)
           (:translate ,%memcpy-name)
           (:args (dst       :scs (sb-vm::sap-reg))
                  (dst-index :scs (sb-vm::any-reg))
                  (src       :scs (sb-vm::sap-reg))
                  (src-index :scs (sb-vm::any-reg))
                  (n-words   :scs (sb-vm::unsigned-reg) :target rcx))
           (:info dst-scale dst-offset src-scale src-offset)
           (:arg-types sb-sys:system-area-pointer sb-vm::tagged-num
                       sb-sys:system-area-pointer sb-vm::tagged-num
                       sb-vm::unsigned-num
                       (:constant x86-fixnum-scale)
                       (:constant (signed-byte 32))
                       (:constant x86-fixnum-scale)
                       (:constant (signed-byte 32)))
           (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rcx-offset) rcx)
           (:temporary (:sc sb-vm::sap-reg      :offset sb-vm::rsi-offset) rsi)
           (:temporary (:sc sb-vm::sap-reg      :offset sb-vm::rdi-offset) rdi)
           (:generator
            20
            (sb-assem:inst lea rsi
                           (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                           :base src :index src-index
                                           :scale (ash (the fixnum src-scale)
                                                       (- +n-fixnum-tag-bits+))
                                           :disp src-offset))
            (sb-assem:inst lea rdi
                           (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                           :base dst :index dst-index
                                           :scale (ash (the fixnum dst-scale)
                                                       (- +n-fixnum-tag-bits+))
                                           :disp dst-offset))
            #+(and)
            (progn
              (sb-c::move rcx n-words)
              ;; (sb-assem:inst std) ; not needed
              (sb-assem:inst rep)
              (sb-assem:inst movs ,size))

            #-(and)
            (progn
              (sb-assem:inst lea rcx
                             (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                             :index n-words
                                             :scale (ash (second ',type) -3)))
              ;; (sb-assem:inst std) ; not needed
              (sb-assem:inst rep)
              (sb-assem:inst movs :byte)))))
              
       (eval-always
         (defmacro ,memcpy-name (dst dst-index src src-index n-words
                                 &key
                                   (dst-scale +fixnum-zero-mask+1+) (dst-offset 0)
                                   (src-scale +fixnum-zero-mask+1+) (src-offset 0))
           (let ((memcpy-fun ',%memcpy-name))
             `(,memcpy-fun ,dst ,dst-index ,src ,src-index ,n-words
                           ,dst-scale ,dst-offset
                           ,src-scale ,src-offset)))))))


(define-fast-memcpy :memcpy-name fast-memcpy/4 :type (unsigned-byte 32) :size :dword)

#+x86-64
(define-fast-memcpy :memcpy-name fast-memcpy/8 :type (unsigned-byte 64) :size :qword)








(defmacro define-fast-memset (&key memset-name type size)
  (let ((%memset-name (concat-symbols '% memset-name)))
    `(progn
       (eval-always
         (declaim (inline ,%memset-name)))

       (eval-always
         (defknown ,%memset-name
             (fast-sap fixnum sb-ext:word ,type
                       x86-fixnum-scale (signed-byte 32))
             (values)
             (sb-c:always-translatable)))

       (eval-always
         (sb-c:define-vop (,%memset-name)
           (:policy :fast-safe)
           (:translate ,%memset-name)
           (:args (sap       :scs (sb-vm::sap-reg))
                  (index     :scs (sb-vm::any-reg))
                  (n-words   :scs (sb-vm::unsigned-reg) :target rcx)
                  (fill-word :scs (sb-vm::unsigned-reg) :target rax))
           (:info scale offset)
           (:arg-types sb-sys:system-area-pointer sb-vm::tagged-num
                       sb-vm::unsigned-num
                       sb-vm::unsigned-num
                       (:constant x86-fixnum-scale)
                       (:constant (signed-byte 32)))
           (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rax-offset) rax)
           (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rcx-offset) rcx)
           (:temporary (:sc sb-vm::sap-reg      :offset sb-vm::rdi-offset) rdi)
           (:generator
            20
            (sb-assem:inst lea rdi
                           (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                           :base sap :index index
                                           :scale (ash (the fixnum scale)
                                                       (- +n-fixnum-tag-bits+))
                                           :disp offset))
            (sb-c::move rcx n-words)
            (sb-c::move rax fill-word)
            ;; (sb-assem:inst std) ; not needed
            (sb-assem:inst rep)
            (sb-assem:inst stos
                           #+x86 rax
                           #-x86 (sb-vm::reg-in-size rax ,size)))))
              
       (eval-always
         (defmacro ,memset-name (ptr index n-words fill-word
                                 &key
                                   (scale +fixnum-zero-mask+1+) (offset 0))
           (let ((memset-fun ',%memset-name))
             `(,memset-fun ,ptr ,index ,n-words ,fill-word ,scale ,offset)))))))

(define-fast-memset :memset-name fast-memset/4 :type (unsigned-byte 32) :size :dword)

#+x86-64
(define-fast-memset :memset-name fast-memset/8 :type (unsigned-byte 64) :size :qword)

