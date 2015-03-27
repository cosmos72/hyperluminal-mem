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
;;;;    boxed pathname                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun msize-box/pathname (index path)
  "Return the number of words needed to store pathname PATH in mmap memory,
not including BOX header."
  (declare (type pathname path)
           (type mem-size index))

  (let ((host (pathname-host path :case :common))
        (default-host (pathname-host *default-pathname-defaults* :case :common)))

    (msize* index
            (if (eq host default-host) nil host)
            (pathname-device path :case :common)
            (pathname-directory path :case :common) 
            (pathname-name path :case :common) 
            (pathname-type path :case :common) 
            (pathname-version path))))



  

(defun mwrite-box/pathname (ptr index end-index path)
  "write pathname PATH into the memory starting at (PTR+INDEX).
Assumes BOX header is already written."
  (declare (type maddress ptr)
           (type mem-size index end-index)
           (type pathname path))

  (let ((host (pathname-host path :case :common))
        (default-host (pathname-host *default-pathname-defaults* :case :common)))

    (mwrite* ptr index end-index
             (if (eq host default-host) nil host)
             (pathname-device path :case :common)
             (pathname-directory path :case :common)
             (pathname-name path :case :common)
             (pathname-type path :case :common)
             (pathname-version path))))


(defun mread-box/pathname (ptr index end-index)
  "Read a pathname from the memory starting at (PTR+INDEX) and return it.
Assumes BOX header was already read."
  (declare (type maddress ptr)
           (type mem-size index))
  
  (with-mread* (host device directory name type version new-index) (ptr index end-index)
    (values
     (make-pathname
      :host (or host (pathname-host *default-pathname-defaults* :case :common))
      :device device :directory directory
      :name name :type type :version version :case :common)
     new-index)))
