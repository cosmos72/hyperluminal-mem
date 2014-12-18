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


(in-package :hyperluminal-db)

(eval-always
  (defconstant +btree-el-type+   +native-word-type+)
  (deftype     btree-el ()       'mem-word)
  (defconstant +btree-nbytes+    4096)
  (defconstant +btree-nelements+ (truncate +btree-nbytes+ (msizeof +btree-el-type+)))
  (defconstant +btree-payloads-elements+ 3) ;; one child index and one range [start, end)
  (defconstant +btree-npayloads+ (truncate (1+ +btree-nelements+) +btree-payloads-elements+))
  (defconstant +btree-nindex+    +btree-npayloads+)
  (defconstant +btree-range-start-index+ +btree-nindex+))
  

          
(ffi-defstruct (btree :size #.+btree-nbytes+)
  (el #.+btree-el-type+ :count #.+btree-nelements+))

(declaim (inline btree-el set-btree-el))

(defun btree-el (ptr index)
  (declare (type maddress ptr)
           (type mem-size index))
  (mget-t #.+btree-el-type+ ptr index))


(defun set-btree-el (ptr index value)
  (declare (type maddress ptr)
           (type mem-size index)
           (type btree-el value))
  (mset-t value #.+btree-el-type+ ptr index))

(defsetf btree-el set-btree-el)


(defmacro btree-index (ptr index)
  `(btree-el ,ptr ,index))

(defmacro set-btree-index (ptr index value)
  `(set-btree-el ,ptr ,index ,value))

(defsetf btree-index set-btree-index)



(defmacro with-btree-range ((rstart &optional rend) ptr index &body body)
  (with-gensyms (vptr vindex)
    `(let* ((,vptr ,ptr)
            (,vindex (mem-size+ +btree-nindex+ (mem-size* 2 ,index)))
            (,rstart (btree-el ,vptr ,vindex))
            ,@(when rend
                    `((,rend (btree-el ,vptr (mem-size+1 ,vindex))))))
       ,@body)))


(defmacro set-btree-range (ptr index &key rstart rend)
  (when (or rstart rend)
    (with-gensyms (vptr vindex)
      `(let* ((,vptr ,ptr)
              (,vindex (mem-size+ +btree-nindex+ (mem-size* 2 ,index))))
         ,@(when rstart
                 `((set-btree-el ,vptr ,vindex ,rstart)))
         ,@(when rend
                 `((set-btree-el ,vptr (mem-size+1 ,vindex) ,rend)))
         nil))))

    
                 
