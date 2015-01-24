;; -*- lisp -*-

;; This file is part of Hyperluminal-MEM.
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


(in-package :hyperluminal-sbcl)

(defun check-x86-addressing (index scale disp)
  (let ((scale (check-compile-constant scale))
        (disp  (check-compile-constant disp)))
    (when (constantp index)
      (let ((index (+ (* (eval index) scale) disp)))
        (when (typep index '(signed-byte 32))
          (return-from check-x86-addressing (values 0 1 index)))))
    (let ((scale (/ scale +fixnum-zero-mask+1+)))
      (check-type scale (member 1 2 4 8))
      (values index scale disp))))


(defmacro def-fast-mread-mwrite (&key mread-name mwrite-name type size)

  (let ((%mread-name  (concat-symbols '% mread-name))
        (%mwrite-name (concat-symbols '% mwrite-name))
        (%mread-name-c  (concat-symbols '% mread-name '/const))
        (%mwrite-name-c (concat-symbols '% mwrite-name '/const)))

    `(progn
       (defknown ,%mread-name
           ;;arg-types
           (fast-sap fixnum (member 1 2 4 8) (signed-byte 32))
           ;;result-type
           ,type
           (sb-c::flushable sb-c::important-result sb-c::always-translatable))

       (defknown ,%mread-name-c
           ;;arg-types
           (fast-sap (member 0) (member 1 2 4 8) (signed-byte 32))
           ;;result-type
           ,type
           (sb-c::flushable sb-c::important-result sb-c::always-translatable))

       (defknown ,%mwrite-name
           ;;arg-types
           (,type fast-sap fixnum (member 1 2 4 8) (signed-byte 32))
           ;;result-type
           (values)
           (sb-c::always-translatable))

       (defknown ,%mwrite-name-c
           ;;arg-types
           (,type fast-sap (member 0) (member 1 2 4 8) (signed-byte 32))
           ;;result-type
           (values)
           (sb-c::always-translatable))


       (sb-c:define-vop (,%mread-name)
         (:policy :fast-safe)
         (:translate ,%mread-name)

         (:args (sap   :scs (sb-vm::sap-reg))
                ;; cheat and use a FIXNUM as INDEX... on SBCL
                ;; its representation is shifted by +n-fixnum-tag-bits+
                ;; which means that INDEX is effectively shifted
                ;; by that many bits
                (index :scs (sb-vm::any-reg)))
         (:info scale disp)
         (:arg-types sb-vm::system-area-pointer sb-vm::tagged-num
                     (:constant (member 1 2 4 8))
                     (:constant (signed-byte 32)))
	      
         (:results   (r :scs (sb-vm::unsigned-reg)))
         (:result-types sb-vm::unsigned-num)

         (:generator 2
          (sb-assem:inst mov
                         (sb-vm::reg-in-size r ,size)
                         (sb-vm::make-ea ,size :base sap :index index
                                         :scale scale :disp disp))))

       (sb-c:define-vop (,%mread-name-c)
         (:policy :fast-safe)
         (:translate ,%mread-name)

         (:args (sap   :scs (sb-vm::sap-reg)))
         (:info index scale disp)
         (:arg-types sb-vm::system-area-pointer
                     (:constant (member 0))
                     (:constant (member 1 2 4 8))
                     (:constant (signed-byte 32)))
	      
         (:results   (r :scs (sb-vm::unsigned-reg)))
         (:result-types sb-vm::unsigned-num)

         (:generator 1
          (sb-assem:inst mov
                         (sb-vm::reg-in-size r ,size)
                         (sb-vm::make-ea ,size :base sap :disp disp))))

       (sb-c:define-vop (,%mwrite-name)
         (:policy :fast-safe)
         (:translate ,%mwrite-name)
         
         (:args (value :scs (sb-vm::unsigned-reg))
                (sap   :scs (sb-vm::sap-reg))
                ;; cheat and use a FIXNUM as INDEX... on SBCL
                ;; its representation is shifted by +n-fixnum-tag-bits+
                ;; which means that INDEX is effectively shifted
                ;; by that many bits
                (index :scs (sb-vm::any-reg)))
         (:info scale disp)
         (:arg-types sb-vm::unsigned-num sb-vm::system-area-pointer sb-vm::tagged-num
                     (:constant (member 1 2 4 8))
                     (:constant (signed-byte 32)))
	      
         (:generator 2
          (sb-assem:inst mov
                         (sb-vm::make-ea ,size :base sap :index index
                                         :scale scale :disp disp)
                         (sb-vm::reg-in-size value ,size))))

       (sb-c:define-vop (,%mwrite-name-c)
         (:policy :fast-safe)
         (:translate ,%mwrite-name)
         
         (:args (value :scs (sb-vm::unsigned-reg))
                (sap   :scs (sb-vm::sap-reg)))
         (:info index scale disp)
         (:arg-types sb-vm::unsigned-num sb-vm::system-area-pointer
                     (:constant (member 0))
                     (:constant (member 1 2 4 8))
                     (:constant (signed-byte 32)))
	      
         (:generator 1
          (sb-assem:inst mov
                         (sb-vm::make-ea ,size :base sap :disp disp)
                         (sb-vm::reg-in-size value ,size))))


       (defmacro ,mread-name (sap index
                              &key (scale +fixnum-zero-mask+1+) (disp 0))
         (multiple-value-bind (index scale disp)
             (check-x86-addressing index scale disp)
           (list ',%mread-name sap index scale disp)))

       (defmacro ,mwrite-name (value sap index
                               &key (scale +fixnum-zero-mask+1+) (disp 0))
         (multiple-value-bind (index scale disp)
             (check-x86-addressing index scale disp)
           (list ',%mwrite-name value sap index scale disp))))))

	   


(def-fast-mread-mwrite :mread-name fast-mread/4 :mwrite-name fast-mwrite/4
                       :type (unsigned-byte 32) :size :dword)


#+x86-64
(def-fast-mread-mwrite :mread-name fast-mread/8 :mwrite-name fast-mwrite/8
                       :type (unsigned-byte 64) :size :qword)

