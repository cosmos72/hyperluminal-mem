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

(declaim (notinline array-mwrite-test array-mwrite-slow-test))

(defun array-mwrite-slow-test (ptr index end-index array)
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type array array))



(defun array-mwrite-test (ptr index end-index array)
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type array array))

  (let ((type (array-element-type array))
        (simple (typep array 'simple-array))
        (rank   (array-rank array)))

    (if (and simple (eql rank 1) (eq type 'fixnum))

        (loop
           for e across (the (simple-array fixnum (*)) array)
           do
             (hlmem::mset-int ptr index (the fixnum e))
             (incf (the fixnum index)))

        (loop for j from 0 below (array-total-size array)
           for e = (row-major-aref (the (array * *) array) j)
           do
             (setf index (mwrite ptr index end-index e))))))



(defun array-test (&optional (len (truncate 1048576 +msizeof-word+)))
  (declare (type fixnum len))
  
  (let* ((array (make-array len
                            :element-type 'fixnum
                            :initial-element 0))
         (idx 0)
         (end (msize idx array)))

    (declare (type mem-size idx end))
    
    (with-mem-words (ptr end)
      (time

       #+(and)
       (dotimes (i 1024)
         (mwrite ptr idx end array))

       #-(and)
       (dotimes (i 1024)
         (hlmem::mwrite-box ptr idx end array (hlmem::mdetect-box-type array)))

       #-(and)
       (dotimes (i 1024)
         (hlmem::call-box-func
          hlmem::+mwrite-box-funcs+
          (hlmem::mdetect-box-type array)
          ptr
          (hlmem::mem-size+ idx hlmem::+mem-box/header-words+)
          end array))

       #-(and)
       (dotimes (i 1024)
         (hlmem::mwrite-box/vector ptr idx end array))

       #-(and)
       (dotimes (i 1024)
         (array-mwrite-test ptr idx end array))

       #-(and)
       (dotimes (i 1024)
         (loop for j from 0 below len
            for e fixnum = (row-major-aref array j)
            ;; do (mwrite ptr j end e)
              ))
         
         #-(and)
         (dotimes (i 1024)
           (dotimes (j len)
             (mwrite ptr j end (row-major-aref array j))))))))
