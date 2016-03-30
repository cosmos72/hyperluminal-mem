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


(in-package :hyperluminal-mem-tree)

(defun b+leaf (&key (leaf t) size capacity contents contents-start contents-end)
  (declare (type (or null b+size) size capacity contents-start contents-end)
           (type (or null simple-vector) contents))
  (the (values b+node &optional)
       (b+node :leaf leaf
               :size (and size (1+ size))
               :capacity (and capacity (1+ capacity))
               :contents contents
               :contents-start (when (or contents contents-start)
                                 (1- (or contents-start 0)))
               :contents-end contents-end)))

(declaim (inline b+leaf-next))
(defun b+leaf-next (node)
  (declare (type b+node node))
  (let ((idx (b+size- (b+node-lo node) 1)))
    (the (values (or null b+node))
         (svref node idx))))


(declaim (inline (setf b+leaf-next)))
(defun (setf b+leaf-next) (value node)
  (declare (type b+node node value))
  (let ((idx (b+size- (b+node-lo node) 1)))
    (setf (svref node idx) value)))


(declaim (inline b+leaf-find))
(defun b+leaf-find (node key &optional default)
  (declare (type b+node node)
           (type fixnum key))
  (let ((lo (b+node-lo node))
        (hi (b+node-hi node)))
    ;; (>= lo hi) means no keys, and leaves cannot have children,
    ;; much less a lone child without keys
    (when (< lo hi) 
      (loop
         ;; lo, mid and hi point to keys and must always be even,
         ;; because odd positions contain values
         :do
         (let* ((mid  (the b+size (logand -2 (ash (+ lo hi) -1))))
                (kmid (the fixnum (b+node-ref node mid))))
           (cond
             ((fixnum< key kmid) (setf hi mid))
             ((fixnum> key kmid) (setf lo mid))
             (t (return-from b+leaf-find (values (b+node-ref node (1+ mid)) t)))))
         :while (< (+ lo 2) hi))
      (when (fixnum= key (b+node-ref node lo))
        (return-from b+leaf-find (values (b+node-ref node (1+ lo)) t)))))
  (values default nil))
