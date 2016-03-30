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

(defstruct (b+tree (:constructor %b+tree))
  (root  nil :type (or null b+node))
  (depth 0   :type b+size))

(defun b+tree (&key (items-per-node 1021) contents contents-start contents-end)
  (declare (type (integer 3 #.most-positive-b+size) items-per-node)
           (type (or null simple-vector) contents)
           (type (or null ufixnum) contents-start contents-end))
  (check-type items-per-node (integer 3 #.most-positive-b+size))
  (let* ((tree (%b+tree))
         (items-per-node (round-n-items (logior 1 items-per-node)))
         (items-per-node-1 (1- items-per-node))
         (contents-len   (if contents (length contents) 0))
         (contents-start (or contents-start 0))
         (contents-end   (min contents-len (or contents-end most-positive-fixnum)))
         (size           (max 0 (- contents-end contents-start))))
    (declare (type ufixnum size))
    (when (plusp size)
      (let ((node (b+node :capacity items-per-node)))
        (loop :until (zerop size)
           :do (let* ((n (min size items-per-node-1))
                      (leaf (b+leaf :size           n
                                    :capacity       items-per-node-1
                                    :contents       contents
                                    :contents-start contents-start
                                    ;; no need for :contents-end
                                    )))
                 (unless (b+node-empty? node)
                   (b+node-append node (svref contents contents-start)))
                 (b+node-append node leaf)
                 
                 (incf (the ufixnum contents-start) n)
                 (decf (the ufixnum size)           n)))
        (setf (b+tree-root tree) node
              (b+tree-depth tree) 1)))
    tree))
                 
             
       
    

(defun b+tree-find (tree key &optional default)
  (declare (type b+tree tree)
           (type fixnum key))
  (let ((node (b+tree-root tree))
        (depth (b+tree-depth tree)))
    (loop
       :until (zerop depth)
       :do (decf depth)
       :do (unless
               (setf node (b+node-find node key))
             (return-from b+tree-find (values default nil))))
    (b+leaf-find node key default)))
       
