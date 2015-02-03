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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    boxed hash-table                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun box-words/hash-table (index htable)
  "Return the number of words needed to store hash-table HTABLE in mmap memory,
not including BOX header."
  (declare (type hash-table htable)
           (type mem-size index))

  (let ((len (hash-table-count htable)))
    (unless (<= len +most-positive-int+)
      (error "HYPERLUMINAL-MEM: hash-table too large for object store.
it contains ~S entries, maximum supported is ~S entries"
	     len +most-positive-int+)))

  ;; +1 to store test function, and another +1 for number of entries
  (incf-mem-size index 2)

  (loop for k being the hash-keys in htable using (hash-value v)
     do
       (setf index (msize* index k v)))
  index)


(eval-always
  (define-constant-once +hash-table-tests+ #(eq eql equal equalp)))

(defun mwrite-box/hash-table (ptr index end-index htable)
  "Write hash-table HTABLE into the memory starting at (PTR+INDEX).
Return INDEX pointing to immediately after written hash-table.

Assumes BOX header is already written, and that enough memory is available
at (PTR+INDEX)."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type hash-table htable))

  (let ((len (hash-table-count htable))
        (test (hash-table-test htable)))

    (check-mem-overrun ptr index end-index 2)

    (mset-int ptr index
              (ecase test
                ((eq     #+clisp ext:fasthash-eq)    0)
                ((eql    #+clisp ext:fasthash-eql)   1)
                ((equal  #+clisp ext:fasthash-equal) 2)
                ((equalp)                            3)))

    (mset-int ptr (incf-mem-size index) len)
    (incf-mem-size index)

    (loop for k being the hash-keys in htable using (hash-value v)
       do
         (setf index (mwrite* ptr index end-index k v)))

    index))



(defun mread-box/hash-table (ptr index end-index)
  "Read a hash-table from the boxed memory starting at (PTR+INDEX) and return it.
Also returns as additional value INDEX pointing to immediately after read hash-table.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (check-mem-length ptr index end-index 2)

  (let* ((hash-test-function-index (mget-int ptr index))
	 (len (mget-int ptr (incf-mem-size index))))

    (check-type hash-test-function-index (mod #.(length +hash-table-tests+)))
    (incf-mem-size index)

    (let ((htable (make-hash-table :test (svref +hash-table-tests+ hash-test-function-index)
                                   :size len)))
      (loop for i from 0 below len
	 do (with-mread* (k v new-index) (ptr index end-index)
              (setf index (the mem-size new-index)
                    (gethash k htable) v)))

      (values htable index))))
