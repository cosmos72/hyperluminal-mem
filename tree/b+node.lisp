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

(defconstant most-positive-b+size
  #+(and) (ash most-positive-fixnum -1)
  #-(and) #xFFFFFF)

(deftype b+size ()  '(integer 0 #.most-positive-b+size))
(deftype ufixnum () '(integer 0 #.most-positive-fixnum))

(declaim (inline b+size+ b+size- fixnum+ fixnum- ))

(defun b+size+ (a b)
  (declare (type b+size a b))
  (the b+size (+ a b)))

(defun b+size- (a b)
  (declare (type b+size a b))
  (the b+size (- a b)))

(defun fixnum+ (a b)
  (declare (type fixnum a b))
  (the fixnum (+ a b)))

(defun fixnum- (a b)
  (declare (type fixnum a b))
  (the fixnum (- a b)))

(deftype b+node () 'simple-vector)
(deftype b+leaf () 'b+node)

(declaim (inline b+node-ref b+node-tag b+node-lo b+node-hi
                 (setf b+node-ref) (setf b+node-tag) (setf b+node-lo) (setf b+node-hi)))

(defun b+node-ref (node index)
  (declare (type b+node node)
           (type b+size index))
  (svref node index))
(defun (setf b+node-ref) (value node index)
  (declare (type b+node node)
           (type b+size index))
  (setf (svref node index) value))

(defun b+node-tag (node)
  (declare (type b+node node))
  (the (or (member t nil) b+node) (svref node 0)))
(defun (setf b+node-tag) (value node)
  (declare (type b+node node)
           (type (or (member t nil) b+node) value))
  (setf (svref node 0) value))

(defun b+node-lo (node)
  (declare (type b+node node))
  (the b+size (svref node 1)))
(defun (setf b+node-lo) (value node)
  (declare (type b+node node)
           (b+size value))
  (setf (svref node 1) value))

(defun b+node-hi (node)
  (declare (type b+node node))
  (the b+size (svref node 2)))
(defun (setf b+node-hi) (value node)
  (declare (type b+node node)
           (b+size value))
  (setf (svref node 2) value))

(declaim (inline b+node-empty?))
(defun b+node-empty? (node)
  (declare (type b+node node))
  (> (b+node-lo node) (b+node-hi node)))

(declaim (inline next-power-of-2))
(defun next-power-of-2 (n)
  (declare (type b+size n))
  (ash 1 (integer-length n)))

(declaim (inline round-n-items))
(defun round-n-items (n)
  (declare (type b+size n))
  (b+size- (next-power-of-2 (b+size+ 2 n))
           3))

(defun b+node (&key leaf size capacity contents contents-start contents-end)
  (declare (type (or null b+size) size capacity contents-end)
           (type (or null (member -1) b+size) contents-start)
           (type (or null simple-vector) contents))
  ;; size and capacity should be odd or zero
  ;; for non-leaves, (length contents) should be odd or zero
  ;; for leaves, (length contents) should be even
  (let* ((contents-len   (the b+size (if contents
                                         (length contents)
                                         0)))
         (contents-start (the (or (member -1) b+size) (or contents-start 0)))
         (contents-end   (the b+size (if contents-end
                                         (min contents-end contents-len)
                                         contents-len)))
         (size (the b+size (or size (fixnum- contents-end contents-start))))
	 (capacity (cond
		     ((null capacity)   (round-n-items (logior 1 size)))
		     ((plusp capacity)  (logior 1 capacity))
		     (t                 0)))
	 (node     (make-array (b+size+ 3 capacity))))
    (setf (b+node-tag node) (not leaf)
	  ;; position of first key, if present. first child would be at 3.
          (b+node-lo node) 4
          (b+node-hi node) (b+size+ 3 size))

    (loop
       :for i fixnum :from (if leaf 1 0) :below (min size (fixnum- contents-end
                                                                   contents-start))
       :for j fixnum = (+ i contents-start)
       :when (>= j 0)
       :do (setf (svref node (fixnum+ 3 i)) (svref contents j)))
    node))

(declaim (inline b+node-find))

(defun b+node-find (node key)
  (declare (type b+node node)
           (type fixnum key))
  (let ((lo (b+node-lo node))
        (hi (b+node-hi node)))
    (cond
      ((< lo hi)
       (loop
          ;; lo, mid and hi point to keys and must always be even,
          ;; because odd positions contain children
          :do
          (let* ((mid  (the b+size (logand -2 (ash (+ lo hi) -1))))
                 (kmid (the fixnum (b+node-ref node mid))))
            (cond
              ((fixnum< key kmid) (setf hi mid))
              ((fixnum> key kmid) (setf lo mid))
              (t (return-from b+node-find (b+node-ref node (1+ mid))))))
          :while (< (+ lo 2) hi))
       (b+node-ref node (if (fixnum< key (b+node-ref node lo))
                            (1- lo)
                            (1+ lo))))
      ((= lo hi) (b+node-ref node (1- lo))) ;; no keys, only one child
      ((> lo hi) nil))))

(declaim (inline b+node-append))

(defun b+node-append (node item)
  (declare (type b+node node))
  (let ((hi (b+node-hi node))
        (cap (length node)))
    (when (< hi cap)
      (setf (svref node hi) item)
      (setf (b+node-hi node) (fixnum+ 1 hi))
      t)))
        
    


               
