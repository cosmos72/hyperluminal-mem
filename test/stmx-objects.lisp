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

(def-suite stmx-objects-suite :in suite)
(in-suite stmx-objects-suite)

 

(defun equalp-gmap (m1 m2)
  (declare (type gmap m1 m2))
  (equalp (gmap-pairs m1)
          (gmap-pairs m2)))


(defun equalp-ghash-table (h1 h2)
  (declare (type ghash-table h1 h2))

  (let ((hash (ghash-table-hash h1))
        (test (ghash-table-test h1)))
    
    (let ((n1 (ghash-table-count h1))
          (n2 (ghash-table-count h2))
          (hash2 (ghash-table-hash h2))
          (test2 (ghash-table-test h2)))

      (unless (eql n1 n2)
        (format *terminal-io* "hash-table H1 and H2 are not equal!
  H1 has ~S elements, while H2 has ~S elements~%" n1 n2)
        (return-from equalp-ghash-table nil))

      (unless (and (eq hash hash2)
                   (eq test test2))
        (format *terminal-io* "hash-table H1 and H2 are not equal!
  H1 uses TEST ~S and HASH ~S, while H2 uses TEST ~S and hash ~S~%"
                test hash test2 hash2)
        (return-from equalp-ghash-table nil)))

    
    (do-ghash (key val1) h1
      (multiple-value-bind (val2 present2) (get-ghash h2 key)
        (unless (and present2
                     (equalp val1 val2))
          (if present2
              (format t "hash-table H1 and H2 are not equal!
  H1 contains ~S ~S, while H2 contains ~S ~S~%" key val1 key val2)
              (format t "hash-table H1 and H2 are not equal!
  H1 contains ~S ~S, while H2 does not contain ~S~%" key val1 key))
              
          (return-from equalp-ghash-table nil)))))
  t)


(defun %ghash-table-test ()
  (let ((h (make-instance 'ghash-table :test 'equalp))
        (tree *abi-tree*)
        (index 0))
    
    (loop for key = (pop tree)
       for val = (pop tree)
       while tree
       do
         (setf (get-ghash h key) val))

    (with-mem-words (ptr (msize index h) end-index)
      (mwrite-mread-test ptr index end-index h
			 :comparator #'equalp-ghash-table))))


(defun %gmap-test ()
  (let ((m (make-instance 'rbmap :pred 'fixnum<))
        (tree *abi-tree*)
        (index 0))
    
    (loop for key = 0 then (the fixnum (1+ key))
       for val = (pop tree)
       while tree
       do
         (setf (get-gmap m key) val))

    (with-mem-words (ptr (msize index m) end-index)
      (mwrite-mread-test ptr index end-index m :comparator #'equalp-gmap))))
        

;; this fails on ABCL because (sxhash #0A42) returns different values at each call
(def-test ghash-table (:compile-at :definition-time)
  (%ghash-table-test))


(def-test gmap (:compile-at :definition-time)
  (%gmap-test))
