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


(in-package :hyperluminal-mem)


(eval-always

 (define-global +package-keyword+          (find-package '#:keyword))
 (define-global +package-common-lisp+      (find-package '#:common-lisp))
 (define-global +package-common-lisp-user+ (find-package '#:common-lisp-user))

 (defun collect-symbols (package-designator &key expected-count skip-list)
   "Return a sorted list containing all external symbols of a package."
   (declare (type (or null fixnum) expected-count)
            (type list skip-list))

   (let ((package package-designator)
         (skip-n 0)
         (n 0)
         (syms nil))
     (declare (type fixnum skip-n n)
              (type list syms))
     (with-package-iterator (iter package :external)
       (loop do
            (multiple-value-bind (found sym) (iter)
              (unless found
                (return))
              (incf n)
              (if (member sym skip-list)
                  (incf skip-n)
                  (push sym syms)))))

       (unless (= skip-n (length skip-list))
         (error "HYPERLUMINAL-DB: cannot compile! ~R of the symbols ~S are not present in package ~S"
                (- (length skip-list) skip-n) skip-list (package-name package)))

       (when (and expected-count
                  (not (eql n expected-count)))
         (error "HYPERLUMINAL-DB: cannot compile! found ~S external symbols in package ~S, expecting ~S"
                n (package-name package) expected-count))

       (sort syms (lambda (sym1 sym2)
                    (string< (symbol-name sym1) (symbol-name sym2))))))


 (defun symbols-vector-to-table (refs &key (first-key 0))
   (declare (type vector refs)
            (type fixnum first-key))
   (let* ((n-refs (length refs))
          (htable (make-hash-table :test 'eql :size n-refs)))
     (dotimes (i n-refs)
       (let ((ref (svref refs i)))
         (unless (eql 0 ref)
           (setf (gethash ref htable) (the fixnum (+ i first-key))))))
     htable)))
    


;; sorted vector of all external symbols in package COMMON-LISP
#-(and)
(define-global +symbols-vector+
    (collect-symbols '#:common-lisp :expected-n-symbols 978 :start-with '(nil t)))

(eval-always
 (define-global +symbols-vector+ (coerce `(

 nil t  ,stmx::+unbound-tvar+ ,stmx.util::+empty-tcell+

 ,@(collect-symbols '#:common-lisp :expected-count 978 :skip-list '(nil t))

 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0

 ,+package-common-lisp-user+
 ,+package-common-lisp+
 ,+package-keyword+

 :compile-toplevel :load-toplevel :execute ;; eval-when options
 :inherited :external :internal ;; intern options
 :element-type :initial-element :initial-contents :adjustable :fill-pointer :displaced-to :displaced-index-offset ;; make-array options
 :test :size :rehash-size :rehash-threshold ;; make-hash-table options
 :case :common :local ;; make-pathname options
 :absolute :relative :wild :newest :unspecific :oldest :previous :installed ;; used inside pathnames
 :before :after :around ;; defmethod options

) 'vector)))





(eval-always
  (define-global +symbols-table+  (symbols-vector-to-table +symbols-vector+)))

(defconstant +mem-pkg/common-lisp-user+ 1021 "persistent representation of the package COMMON-LISP-USER")
(defconstant +mem-pkg/common-lisp+   1022 "persistent representation of the package COMMON-LISP")
(defconstant +mem-pkg/keyword+       1023 "persistent representation of the package KEYWORD")


(defconstant +mem-syms/first+           0 "first value used by predefined symbols")

(defconstant +mem-syms/last+ (+ +mem-syms/first+ (length +symbols-vector+) -1) "last value used for predefined symbols")


(defconstant +mem-syms-user/first+   2048 "first value available for user-defined symbols")




(eval-always

  (loop for (sym . expected-pos)
     in `((nil     . ,+mem-sym/nil+)
          (t       . ,+mem-sym/t+)
          (&whole  .  11)
          (and     .  86)
          (car     . 182)
          (cons    . 257)
          (do      . 319)
          (if      . 443)
          (map     . 565)
          (nth     . 633)
          (setf    . 785)
          (string  . 852)
          (vector  . 949)
          (zerop   . 979)
          (,+package-common-lisp-user+ . ,+mem-pkg/common-lisp-user+)
          (,+package-common-lisp+      . ,+mem-pkg/common-lisp+)
          (,+package-keyword+          . ,+mem-pkg/keyword+)
          (:compile-toplevel           . 1024)
          (:around                     . 1054))
     for pos = (gethash sym +symbols-table+)
     do
       (unless (eql pos expected-pos)
         (error "HYPERLUMINAL-DB: unexpected contents of ~S, cannot compile!
symbol ~S is associated to value ~S, it must be associated to value ~S instead"
                '+symbols-table+ sym pos expected-pos))))

