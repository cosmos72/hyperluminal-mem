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

(deftype b+node-size () '(integer 0 #.(ash most-positive-fixnum -1)))


(deftype b+node () 'simple-vector)

(declaim (inline b+node-ref b+node-lo b+node-hi
                 (setf b+node-ref) (setf b+node-lo) (setf b+node-hi)))

(defun b+node-ref (node index)
  (declare (type b+node node)
           (type b+node-size index))
  (svref node index))
(defun (setf b+node-ref) (value node index)
  (declare (type b+node node)
           (type b+node-size index))
  (setf (svref node index) value))

(defun b+node-lo (node)
  (declare (type b+node node))
  (the b+node-size (svref node 0)))
(defun (setf b+node-lo) (value node)
  (declare (type b+node node)
           (b+node-size value))
  (setf (svref node 0) value))

(defun b+node-hi (node)
  (declare (type b+node node))
  (the b+node-size (svref node 1)))
(defun (setf b+node-hi) (value node)
  (declare (type b+node node)
           (b+node-size value))
  (setf (svref node 1) value))


(defun round-up-to-power-of-2 (n)
  (declare (type b+node-size n))
  (if (or (<= n 2)
          (zerop (logand n (1- n))))
      n
      (ash 1 (integer-length n))))
   
(defun b+node (&key (min-keys 2))
  (declare (type b+node-size min-keys))
  (let* ((min-size (round-up-to-power-of-2 (+ 3 min-keys min-keys)))
         (node     (make-array min-size)))
    (setf (b+node-lo node) 2
          (b+node-hi node) 2)
    node))

(defun b+node-find (node key)
  (declare (type b+node node)
           (type fixnum key))
  (let ((lo (b+node-lo node))
        (hi (b+node-hi node)))
    (cond
      ((>= lo hi) nil)
      ((= (1+ lo) hi) (b+node-ref node lo)) ;; no keys, only one child
      (t
       (incf (the b+node-size lo))
       (loop
          ;; mid must always be odd, because even positions contain children
          :for mid = (the b+node-size (logior 1 (+ (ash lo -1) (ash hi -1))))
          :for kmid = (the fixnum (b+node-ref node mid))
          :do (cond
                ((fixnum< key kmid) (setf hi mid))
                ((fixnum> key kmid) (setf lo mid))
                (t (return-from b+node-find (b+node-ref node (1+ mid)))))
          :while (< (+ lo 2) hi))
       (b+node-ref node (if (fixnum< key (b+node-ref node lo))
                            (1- lo)
                            (1+ lo)))))))
           
               
            
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
  (test-b+node-args :node #(2 2)
                    :min-key 0 :max-key 0
                    :expected-results #(nil))

  (test-b+node-args :node #(2 3 a)
                    :min-key 0 :max-key 1
                    :expected-results #(a a))

  (test-b+node-args :node #(2 5 a 10 b)
                    :min-key 8 :max-key 11
                    :expected-results #(a a b b))

  (test-b+node-args :node #(2 13 a 10 b 12 c 14 d 16 e 18 f)
                    :min-key 8 :max-key 19
                    :expected-results #(a a b b c c d d e e f f)))

