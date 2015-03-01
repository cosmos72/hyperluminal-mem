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


(in-package :hyperluminal-mem-test)

(def-suite magic-suite :in suite)
(in-suite magic-suite)

(defun compact-test (&optional (count 1000000))
  (declare (type integer count))
  (dotimes (i count)
    (let ((c (hlmem::compact-sizeof i)))
      (when c
        (is (eql i (hlmem::uncompact-sizeof c)))))))

(def-test compact  (:compile-at :definition-time)
  (compact-test))


(defun uncompact-test ()
  (dotimes (i 255)
    (let ((u (hlmem::uncompact-sizeof i)))
      (is (eql i (hlmem::compact-sizeof u))))))


(def-test uncompact (:compile-at :definition-time)
  (uncompact-test))
