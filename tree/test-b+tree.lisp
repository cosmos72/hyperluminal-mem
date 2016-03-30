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

  (test-b+node-args :node (b+node :capacity 1 :contents #(a))
                    :min-key 0 :max-key 1
                    :expected-results #(a a))

  (test-b+node-args :node (b+node :capacity 3 :contents #(a 10 b))
                    :min-key 8 :max-key 11
                    :expected-results #(a a b b))

  (test-b+node-args :node (b+node :capacity 11
				  :contents #(a 10 b 12 c 14 d 16 e 18 f))
                    :min-key 8 :max-key 19
                    :expected-results #(a a b b c c d d e e f f)))


(defun test-b+tree ()
  (let ((tree (b+tree :items-per-node 13 :contents #(1 a 2 b 3 c 4 d 5 e 6 f 7 g 8 h 9 i 10 j)))
        (expected-tree (%b+tree
                          :root #(t 4 6
                                  #(nil 4 16 0 1 a 2 b 3 c 4 d 5 e 6 f)
                                  7
                                  #(nil 4 12 0 7 g 8 h 9 i 10 j 0 0 0 0)
                                  0 0 0 0 0 0 0 0 0 0)
                          :depth 1)))
    (or (equalp tree expected-tree)
        (error "TEST-B+TREE failed: test B+TREE contains ~S, expected ~S"
               tree expected-tree))))
