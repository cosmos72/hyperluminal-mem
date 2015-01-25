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

(defun array-test (&optional (len (truncate 1048576 +msizeof-word+)))
  (declare (type fixnum len))
  
  (let* ((array (make-array len :element-type 'fixnum :initial-element 0))
         (idx 0)
         (end (msize idx array)))

    (declare (type mem-size idx end))
    (print len)
    
    (with-mem-words (ptr end)
      (time
       (dotimes (i 1024)

         #+(and)
         (mwrite ptr idx end array)

         #-(and)
         (mwrite-box ptr idx end array (hlmem::mdetect-box-type array))

         #-(and)
         (loop for j from 0 below len
            for e fixnum = (row-major-aref array j) do
              (mwrite ptr j end e))
         
         #-(and)
         (dotimes (j len)
           (mwrite ptr j end (row-major-aref array j))))))))
