;; -*- lisp -*-

;; This file is part of hyperluminal-DB.
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


(in-package :hyperluminal-db.test)

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
    
    (unless (and (eql (ghash-table-count h1) (ghash-table-count h2))
                 (eq hash (ghash-table-hash h2))
                 (eq test (ghash-table-test h2)))
      (return-from equalp-ghash-table nil))
    
    (do-ghash (key val1) h1
      (multiple-value-bind (val2 present2) (get-ghash h2 key)
        (unless (and present2
                     (equalp val1 val2))
          (return-from equalp-ghash-table nil)))))
  t)


(defun %ghash-table-test ()
  (let ((h (make-instance 'ghash-table :test 'equalp))
        (tree *tree*)
        (index 0))
    
    (loop for key = (pop tree)
       for val = (pop tree)
       while tree
       do
         (setf (get-ghash h key) val))

    (with-mem-words (ptr (msize h index) end-index)
      (mwrite-mread-test ptr index end-index h
			 :comparator #'equalp-ghash-table))))


(defun %gmap-test ()
  (let ((m (make-instance 'rbmap :pred 'fixnum<))
        (tree *tree*)
        (index 0))
    
    (loop for key = 0 then (the fixnum (1+ key))
       for val = (pop tree)
       while tree
       do
         (setf (get-gmap m key) val))

    (with-mem-words (ptr (msize m index) end-index)
      (mwrite-mread-test ptr index end-index m :comparator #'equalp-gmap))))
        


(def-test ghash-table (:compile-at :definition-time)
  (%ghash-table-test))


(def-test gmap (:compile-at :definition-time)
  (%gmap-test))
