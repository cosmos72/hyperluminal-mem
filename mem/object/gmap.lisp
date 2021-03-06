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
;;;;   read and write sorted map STMX.UTIL:RBMAP                             ;;;;
;;;;   and its transactional version STMX.UTIL:TMAP                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enable-#?-syntax)


#?+(and (symbol :stmx.util :ghash-table-test)
        (symbol :stmx.util :ghash-table-hash))
(progn
  (defmethod msize-object ((obj gmap) index)
    (declare (type mem-size index))

    (setf index (msize* index (gmap-pred obj) (gmap-count obj)))
    (do-gmap (key value) obj
      (setf index (msize* index key value)))
    index)


  (defmethod mwrite-object ((obj gmap) ptr index end-index)
    (declare (type mem-size index end-index))

    (setf index (mwrite* ptr index end-index (gmap-pred obj) (gmap-count obj)))
    (do-gmap (key value) obj
      (setf index (mwrite* ptr index end-index key value)))
    index)


  ;; we currently do NOT allow deserializing arbitrary functions as RBMAP predicates:
  ;; it would allow a malicious remote user to execute arbitrary code!
  (define-global *gmap-trusted-pred-list*
      '(< > fixnum< fixnum> char< char> string< string>))



  (defun mread-object/gmap (type ptr index end-index)
    (declare (type symbol type)
             (type maddress ptr)
             (type mem-size index end-index))

    (with-mread* (pred n index) (ptr index end-index)

      (unless (member pred *gmap-trusted-pred-list* :test #'eq)
        (error "HYPERLUMINAL-MEM: refusing to use untrusted ~S ~S value ~S,
expecting one of the trusted values ~S" type :pred pred *gmap-trusted-pred-list*))

      (check-type n mem-uint)
      
      (let ((obj (make-instance type :pred pred)))
        (declare (type gmap obj))
        (dotimes (i n)
          (with-mread* (key value new-index) (ptr index end-index)
            (setf (get-gmap obj key) value)
            (setf index new-index)))
        
        (values obj index))))


  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read RBMAP                                                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (defmethod mread-object ((type (eql 'rbmap)) ptr index end-index
                           &key &allow-other-keys)
    (declare (type mem-size index end-index))

    (mread-object/gmap type ptr index end-index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   read TMAP                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defmethod mread-object ((type (eql 'tmap)) ptr index end-index
                           &key &allow-other-keys)
    (declare (type mem-size index end-index))

    (mread-object/gmap type ptr index end-index)))

