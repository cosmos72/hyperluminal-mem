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


(in-package :hyperluminal-sbcl)


(defknown %fast-mread/4
    ;;arg-types
    (fixnum fixnum (member 1 2 4 8) (signed-byte 32))
    ;;result-type
    (unsigned-byte 32)
    (sb-c::flushable sb-c::important-result sb-c::always-translatable))


(defknown %fast-mwrite/4
    ;;arg-types
    ((unsigned-byte 32) fixnum fixnum (member 1 2 4 8) (signed-byte 32))
    ;;result-type
    (values)
    (sb-c::always-translatable))



(sb-c:define-vop (%fast-mread/4)
  (:policy :fast-safe)
  (:translate %fast-mread/4)

  (:args (base  :scs (sb-vm::any-reg))
	 ;; cheat and use a FIXNUM as INDEX...
	 ;; its representation is shifted by +n-fixnum-tag-bits+
	 ;; which means that INDEX is effectively shifted
	 ;; by that many bits
         (index :scs (sb-vm::any-reg)))
  (:info scale disp)
  (:arg-types sb-vm::tagged-num sb-vm::tagged-num
              (:constant (member 1 2 4 8))
              (:constant (signed-byte 32)))
	      
  (:results   (r :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)

  (:generator 1
   (sb-assem:inst mov r (sb-vm::make-ea :dword :base base :index index
					:scale scale :disp disp))))


(sb-c:define-vop (%fast-mwrite/4)
  (:policy :fast-safe)
  (:translate %fast-mwrite/4)

  (:args (value :scs (sb-vm::unsigned-reg))
	 (base  :scs (sb-vm::any-reg))
	 ;; cheat and use a FIXNUM as INDEX...
	 ;; its representation is shifted by +n-fixnum-tag-bits+
	 ;; which means that INDEX is effectively shifted
	 ;; by that many bits
         (index :scs (sb-vm::any-reg)))
  (:info scale disp)
  (:arg-types sb-vm::unsigned-num sb-vm::tagged-num sb-vm::tagged-num
              (:constant (member 1 2 4 8))
              (:constant (signed-byte 32)))
	      
  (:generator 1
   (sb-assem:inst mov (sb-vm::make-ea :dword :base base :index index
				      :scale scale :disp disp) value)))


(defmacro fast-mread/4 (base index
			&key (scale +fixnum-zero-mask+1+) (disp 0))
  (let ((scale (/ (check-compile-constant scale) +fixnum-zero-mask+1+))
	(disp  (check-compile-constant disp)))
    (check-type scale (member 1 2 4 8))
    `(%fast-mread/4 ,base ,index ,scale ,disp)))

(defmacro fast-mwrite/4 (value base index
			 &key (scale +fixnum-zero-mask+1+) (disp 0))
  (let ((scale (/ (check-compile-constant scale) +fixnum-zero-mask+1+))
	(disp  (check-compile-constant disp)))
    (check-type scale (member 1 2 4 8))
    `(%fast-mwrite/4 ,value ,base ,index ,scale ,disp)))

	   