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

(deftype b+node-size ()
  #||#   #-(and) '(integer 0 #.(ash most-positive-fixnum -1))
         #+(and) '(unsigned-byte 24))


(deftype b+node () 'simple-vector)
(deftype b+leaf () 'b+node)

(declaim (inline b+node-ref b+node-tag b+node-lo b+node-hi
                 (setf b+node-ref) (setf b+node-tag) (setf b+node-lo) (setf b+node-hi)))

(defun b+node-ref (node index)
  (declare (type b+node node)
           (type b+node-size index))
  (svref node index))
(defun (setf b+node-ref) (value node index)
  (declare (type b+node node)
           (type b+node-size index))
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
  (the b+node-size (svref node 1)))
(defun (setf b+node-lo) (value node)
  (declare (type b+node node)
           (b+node-size value))
  (setf (svref node 1) value))

(defun b+node-hi (node)
  (declare (type b+node node))
  (the b+node-size (svref node 2)))
(defun (setf b+node-hi) (value node)
  (declare (type b+node node)
           (b+node-size value))
  (setf (svref node 2) value))

(declaim (inline next-power-of-2 round-n-items))

(defun next-power-of-2 (n)
  (declare (type b+node-size n))
  (ash 1 (integer-length n)))

(defun round-b+node-size (n)
  (declare (type b+node-size n))
  (- (the b+node-size (next-power-of-2 (the b+node-size (+ 2 n))))
     3))

(defun b+node (&key (size nil) (capacity nil) (leaf nil) (initial-contents nil))
  (declare (type (or null b+node-size) size capacity)
           (type list initial-contents))
  ;; (lentgh initial-contents), size and capacity should be odd or zero
  (let* ((size (the b+node-size
		    (cond
		      (size             size)
		      (initial-contents (length initial-contents))
		      (t 0))))
	 (capacity (cond
		     ((null capacity)   (round-b+node-size (logior 1 size)))
		     ((plusp capacity)  (logior 1 capacity))
		     (t                 0)))
	 (node     (make-array (+ 3 capacity))))
    (setf (b+node-tag node) (not leaf)
	  ;; position of first key, if present. first child would be at 3.
          (b+node-lo node) 4
          (b+node-hi node) (+ 3 size))
    (loop
       :for item :in initial-contents
       :for i :from 3 :below (+ 3 size)
       :do (setf (svref node i) item))
    node))

(defun b+leaf (&key (size nil) (capacity nil) (leaf t) (initial-contents nil))
  (declare (type (or null b+node-size) size capacity)
           (type list initial-contents))
  (b+node :size (and size (1+ size))
	  :capacity (and capacity (1+ capacity))
	  :leaf leaf
          :initial-contents (and initial-contents (cons nil initial-contents))))

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
          (let* ((mid  (the b+node-size (logand -2 (ash (+ lo hi) -1))))
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
      

(defun b+leaf-find (node key)
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
         (let* ((mid  (the b+node-size (logand -2 (ash (+ lo hi) -1))))
                (kmid (the fixnum (b+node-ref node mid))))
           (cond
             ((fixnum< key kmid) (setf hi mid))
             ((fixnum> key kmid) (setf lo mid))
             (t (return-from b+leaf-find (values (b+node-ref node (1+ mid)) t)))))
         :while (< (+ lo 2) hi))
      (when (fixnum= key (b+node-ref node lo))
        (return-from b+leaf-find (values (b+node-ref node (1+ lo)) t)))))
  (values nil nil))

               
            
(defun test-b+node-args (&key node min-key max-key expected-results)
  (declare (type b+node node)
           (type fixnum min-key max-key)
           (type simple-vector expected-results))
  (loop :for key :from min-key :to max-key
     :for expected :across expected-results
     :for actual = (b+node-find node key)
     :unless (eql actual expected)
     :do (error "TEST-B+NODE failed: searching key ~S in ~S returned value ~S, expected ~S"
                key node actual expected))
  t)


(defun test-b+node ()
  (test-b+node-args :node (b+node :capacity 0)
                    :min-key 0 :max-key 0
                    :expected-results #(nil))

  (test-b+node-args :node (b+node :capacity 1 :initial-contents '(a))
                    :min-key 0 :max-key 1
                    :expected-results #(a a))

  (test-b+node-args :node (b+node :capacity 3 :initial-contents '(a 10 b))
                    :min-key 8 :max-key 11
                    :expected-results #(a a b b))

  (test-b+node-args :node (b+node :capacity 11
				  :initial-contents '(a 10 b 12 c 14 d 16 e 18 f))
                    :min-key 8 :max-key 19
                    :expected-results #(a a b b c c d d e e f f)))

