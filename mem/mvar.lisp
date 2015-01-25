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


(in-package :hyperluminal-mem)


(defstruct (mvar (:include stmx:tvar))
  "a memory-mapped transactional variable (mvar) is the smallest unit of persistent,
transactional memory. it contains a single value that can be read or written during a transaction
using ($-slot var) and (setf ($-slot var) value).

The value of each mvar modified during a transaction is written to memory-mapped persistent store
while committing.

mvars are seldom used directly, since persistent transactional objects (mobjs)
wrap them with a more convenient interface: you can read and write normally
the slots of a persistent transactional object (with slot-value, accessors ...),
and behind the scenes the slots will be stored in mvars."

  (slot-address 0 :type mem-size) ;; address of this mvar in store's area allocated to parent obj
  (box-address  0 :type mem-size) ;; address of boxed memory, if needed, allocated to this mvar
  (box-n-words  0 :type mem-size) ;; length of boxed memory, if needed, allocated to this mvar

  (parent-obj nil :type t #-(and)(or null mobject)))
