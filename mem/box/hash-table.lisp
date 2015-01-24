;; -*- lisp -*-

;; This file is part of Hyperluminal-MEM.
;; Copyright (c) 2013 Massimiliano Ghilardi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


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

  ;; +1 to store number of entries
  (incf-mem-size index)

  (loop for k being the hash-keys in htable using (hash-value v)
     do
       (setf index (msize* index k v)))
  index)

  

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

    (check-mem-overrun ptr index end-index 1)

    (mset-int ptr index
              (case test
                ((eq equal #+clisp ext:fasthash-eq #+clisp ext:fasthash-equal) len)
                (otherwise (lognot len))))
                            
    (incf-mem-size index)

    (loop for k being the hash-keys in htable using (hash-value v)
       do
         (setf index (mwrite* ptr index end-index k v)))

    index))


(define-constant-once +hash-table-tests+ #(eq eql equal equalp))

(defun mread-box/hash-table (ptr index end-index)
  "Read a hash-table from the boxed memory starting at (PTR+INDEX) and return it.
Also returns as additional value INDEX pointing to immediately after read hash-table.

Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index end-index))
  
  (check-mem-length ptr index end-index 1)

  (let* (;; re-read BOX header, because we need the boxed-type
         (boxed-type (mget-fulltag ptr (mem-size- index +mem-box/header-words+)))
         (test-index (if (= boxed-type +mem-box/hash-table-eq+) 0 2))
	 (len (mget-int ptr index)))

    (declare (type (mod 4) test-index)
             (type mem-int len))
         
    (when (< len 0)
      (incf test-index)
      (setf len (lognot len)))

    (incf-mem-size index)

    (let ((htable (make-hash-table :test (svref +hash-table-tests+ test-index)
                                   :size len)))
      (loop for i from 0 below len
	 do (with-mread* (k v new-index) (ptr index end-index)
              (setf index (the mem-size new-index)
                    (gethash k htable) v)))

      (values htable index))))
