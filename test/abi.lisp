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


(in-package :hyperluminal-mem.test)

(def-suite abi-suite :in suite)
(in-suite abi-suite)

;; #xFFFEFDFCFBFAF9F8F7F6F5F4F3F2F1F0EFEEEDECEBEAE9E8E7E6E5E4E3E2E1E0DFDEDDDCDBDAD9D8D7D6D5D4D3D2D1D0CFCECDCCCBCAC9C8C7C6C5C4C3C2C1C0BFBEBDBCBBBAB9B8B7B6B5B4B3B2B1B0AFAEADACABAAA9A8A7A6A5A4A3A2A1A09F9E9D9C9B9A999897969594939291908F8E8D8C8B8A898887868584838281807F7E7D7C7B7A797877767574737271706F6E6D6C6B6A696867666564636261605F5E5D5C5B5A595857565554535251504F4E4D4C4B4A494847464544434241403F3E3D3C3B3A393837363534333231302F2E2D2C2B2A292827262524232221201F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100
(defconstant +abi-n+ (loop for i from 0 to #xFF
                        for bits = 0 then (logior bits (ash (logand i #xFF) (* i 8)))
                        finally (return bits)))

(defun mset-float/inline-test (ptr count)
  (declare (type fixnum count))

  ;; on Intel Core i7-4770,
  ;; HW transactions succeed very often when writing up to 16KBytes
  ;; as long as the mmapped RAM is already dirty:
  ;; each RAM page must be written to *before* the HW transaction.

  ;; WARNING:
  ;; calling (hlmem::mset-unboxed) instead of (hlmem::mset-float/inline)
  ;; at low settings of (optimize (speed)) sometimes causes *all*  HW transactions to fail!
  ;; the problem disappears by setting (optimize (speed 3)) before loading HYPERLUMINAL-MEM

  (loop for idx from 0 below count by 512
       for value = (hlmem::mget-word ptr idx) do
       (hlmem::mset-word ptr idx value))

  (stmx::hw-atomic2 ()
   (loop for idx from 0 below count
      for value from 0.0 by 0.1 do
        #-(and) (hlmem::mset-float/inline :sfloat ptr idx value)
        #+(and) (hlmem::mset-unboxed ptr idx value)
        finally (return :hw-tx))
   :fallback))



(defun mwrite-mread-test (ptr index end-index x &key (comparator #'equalp))
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type function comparator))
  (let ((send (msize index x))
        (xend (mwrite ptr index end-index x)))
    (is (= send xend))
    (multiple-value-bind (y yend) (mread ptr index end-index)
      (is (= xend yend))
      (is-true (funcall comparator x y)))))
          

(defun bignum-test (count)
  (let ((index 0)
        (end-index (+ 10 (truncate count +msizeof-word+))))
    (with-mem-words (ptr end-index)
      (loop for i from 0 below count
         for x = 0 then (logior (ash x 8) i) do
           (mwrite-mread-test ptr index end-index x)
           (mwrite-mread-test ptr index end-index (- x))))))

(def-test bignum (:compile-at :definition-time)
  (bignum-test 400))


(defparameter *abi-tree*
  (list #(0 1 -1 2 -2 1/2 -2/3 3/4 -4/5)

          most-positive-fixnum most-negative-fixnum
          (ash most-positive-fixnum 10) (ash most-negative-fixnum 10)
          +abi-n+ (- +abi-n+)
          
          #(0.0 0.1 -0.9999)
          most-positive-single-float most-negative-single-float
          least-positive-normalized-single-float least-negative-normalized-single-float
          least-positive-single-float least-negative-single-float

          #(0.0d0 0.1d0 -0.9999999999d0)
          most-positive-double-float most-negative-double-float
          least-positive-normalized-double-float least-negative-normalized-double-float
          least-positive-double-float least-negative-double-float

          #2A((#C(0.7 0.8)  #C(-0.3d0 -0.4d0))
              (#C(1/5 2/5)  #C(0 -1/123456789012345678901234567890)))

          #0A42 ;; zero-dimensional array
          #*010010001 ;; bit-vector
          "foobarbaz"    ;; string
          (make-array 3 :element-type 'base-char :initial-contents "xyz") ;; base-string
          #P"/a/b/c"  ;; pathname

          (let ((h (make-hash-table)))
            (setf (gethash 'foo h) 'bar)
            h)

          nil t 'get-universal-time 'zerop 'foobar
          stmx:+unbound-tvar+ 
          stmx.util::+empty-tcell+
          :compile-toplevel :load-toplevel :execute))


(defun tree-test ()
  (let ((tree *abi-tree*)
        (index 0))
    (with-mem-words (ptr (msize index tree) end-index)
      (dolist (e tree)
        (mwrite-mread-test ptr index end-index e))
      (mwrite-mread-test ptr index end-index tree))))
        


(def-test tree (:compile-at :definition-time)
  (tree-test))
