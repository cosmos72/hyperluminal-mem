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


(defun round-up-to-power-of-2 (n)
  (declare (type b+node-size n))
  (if (or (<= n 2)
          (zerop (logand n (1- n))))
      n
      (ash 1 (integer-length n))))

(defun round-n-items (n)
  (declare (type b+node-size n))
  (- (the b+node-size (round-up-to-power-of-2
                       (the b+node-size (+ 3 n))))
     3))

(defun b+node (&key (n-items nil) (pad t) (leaf nil) (initial-contents nil))
  (declare (type (or null b+node-size) n-items)
           (type list initial-contents))
  ;; (lenth initial-contents) and n-items should be odd or zero
  (let* ((n-items   (the b+node-size (cond
                                       ((and n-items (plusp n-items))
                                        (logior 1 n-items))
                                       (initial-contents
                                        (logior 1 (length initial-contents)))
                                       (t 0))))
         (n-rounded (the b+node-size (if pad (round-n-items n-items) n-items)))
         (node      (make-array (+ 3 n-rounded))))
    (setf (b+node-tag node) (not leaf)
          (b+node-lo node) 4 ;; position of first key, if present. first child would be at 3.
          (b+node-hi node) (+ 3 n-items))
    (loop
       :for item :in initial-contents
       :for i :from 3 :below (+ 3 n-items)
       :do (setf (svref node i) item))
    node))

(defun b+leaf (&key (n-items nil) (pad t) (leaf t) (initial-contents nil))
  (declare (type b+node-size n-items)
           (type list initial-contents))
  (b+node :n-items (and n-items (1+ n-items))
          :pad pad :leaf leaf
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
  (test-b+node-args :node (b+node :pad nil)
                    :min-key 0 :max-key 0
                    :expected-results #(nil))

  (test-b+node-args :node (b+node :pad nil :initial-contents '(a))
                    :min-key 0 :max-key 1
                    :expected-results #(a a))

  (test-b+node-args :node (b+node :pad nil :initial-contents '(a 10 b))
                    :min-key 8 :max-key 11
                    :expected-results #(a a b b))

  (test-b+node-args :node (b+node :pad nil :initial-contents '(a 10 b 12 c 14 d 16 e 18 f))
                    :min-key 8 :max-key 19
                    :expected-results #(a a b b c c d d e e f f)))

