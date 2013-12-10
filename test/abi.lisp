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


(in-package :hyperluminal-db.test)

;; #xFFFEFDFCFBFAF9F8F7F6F5F4F3F2F1F0EFEEEDECEBEAE9E8E7E6E5E4E3E2E1E0DFDEDDDCDBDAD9D8D7D6D5D4D3D2D1D0CFCECDCCCBCAC9C8C7C6C5C4C3C2C1C0BFBEBDBCBBBAB9B8B7B6B5B4B3B2B1B0AFAEADACABAAA9A8A7A6A5A4A3A2A1A09F9E9D9C9B9A999897969594939291908F8E8D8C8B8A898887868584838281807F7E7D7C7B7A797877767574737271706F6E6D6C6B6A696867666564636261605F5E5D5C5B5A595857565554535251504F4E4D4C4B4A494847464544434241403F3E3D3C3B3A393837363534333231302F2E2D2C2B2A292827262524232221201F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100
(defconstant +n+ (loop for i from 0 to #xFF
                    for bits = 0 then (logior bits (ash (logand i #xFF) (* i 8)))
                    finally (return bits)))

(defun test-mset-float/inline (ptr count)
  (declare (type fixnum count))

  ;; on Intel Core i7-4770,
  ;; HW transactions succeed very often when writing up to 16KBytes
  ;; as long as the mmapped RAM is already dirty:
  ;; each RAM page must be written to *before* the HW transaction.

  ;; WARNING:
  ;; calling (sp::mset-unboxed) instead of (sp::mset-float-inline)
  ;; at low settings of (optimize (speed)) sometimes causes *all*  HW transactions to fail!
  ;; the problem disappears by setting (optimize (speed 3)) before loading HYPERLUMINAL-DB

  (loop for idx from 0 below count by 512
       for value = (sp::mget-word ptr idx) do
       (sp::mset-word ptr idx value))

  (stmx::hw-atomic2 ()
   (loop for idx from 0 below count
      for value from 0.0 by 0.1 do
        #-(and) (sp::mset-float/inline :sfloat ptr idx value)
        #+(and) (sp::mset-unboxed ptr idx value)
        finally (return :hw-tx))
   :fallback))



(defun test-mwrite-mread-bignum (ptr count)
  (flet ((write-then-read-bignum (x)
           (sp::mzero ptr 1000)
           (sp::%mwrite-box/bignum ptr 0 count x)
           (let ((y (sp::mread-box/bignum ptr 0)))
             (unless (eql x y)
               (error "wrote ~S but read ~S" x y)))))

    (loop for i from 0 below count
       for x = 0 then (logior (ash x 8) i) do
         (write-then-read-bignum x)
         (write-then-read-bignum (- x)))))