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


(in-package :hyperluminal-mem-test)


(defun mset-int-test (ptr index &optional (num-threads 8) (iterations 300000000))
  (declare (type maddress ptr)
           (type mem-size index num-threads)
           (type mem-int iterations))

  (let ((threads
         (loop for i from 0 below num-threads collect
            ;; assume cache lines are 64 bytes
              (let ((offset (the mem-size (+ index (* i (truncate 64 +msizeof-word+))))))
                (bt:make-thread
                 (lambda ()
                   (let* ((num-failed 0)
                          (cons (cons 0 0))
                          (func (lambda ()
                                  (mset-int ptr offset (first cons)))))

                     (loop for i from 0 below iterations
                        do
                          #-(and)
                          (if (eql (sb-transaction:transaction-begin)
                                   sb-transaction:+transaction-started+)
                              (progn
                                (mset-int ptr offset i)
                                (sb-transaction:transaction-end))
                              (progn
                                (mset-int ptr offset i)
                                (incf (the fixnum num-failed))))

                          #+(and)
                          (stmx::hw-atomic2 (nil :test-for-running-tx? nil)
                           (mset-int ptr offset i)
                           (progn
                             (setf (first cons) i)
                             (stmx::%run-sw-atomic func)
                             (incf (the fixnum num-failed))))

                          #-(and)
                          (progn
                            (setf (first cons) i)
                            (stmx:atomic
                              (mset-int ptr offset (first cons)))))

                     num-failed)))))))
     
    (loop for th in threads collect
         (bt:join-thread th))))


(defun mset-int-test-report (ptr index &optional (num-threads 8) (iterations 300000000))
  (declare (type maddress ptr)
           (type mem-size index num-threads)
           (type mem-int iterations))

  (let* ((start-tics (get-internal-real-time))
         (fails (mset-int-test ptr index num-threads iterations))
         (end-tics (get-internal-real-time))
         (seconds (/ (- end-tics start-tics) (float internal-time-units-per-second))))

    ;; benchmark results for 64 bit SBCL running on Intel Core i7 4770
    ;;
    ;; if SW transactions are not used,
    ;; speed is 367 millions HW transactions per second.
    ;; 
    ;; if SW transactions are used as fallback, and thus HW transactions
    ;; are augmented with compatibility constraints,
    ;; speed drops to 225 millions (hybrid) transactions per second
    ;;
    ;; the unoptimized version (stmx:atomic (mset-int ptr offset (first cons)))
    ;; reaches 197 millions (hybrid) transactions per second
    ;; 
    ;; in all cases, HW transactions success rate is > 99.999%
    (format t "elapsed time: ~S seconds~%HW transactions per second: ~S~%"
            seconds
            (/ (* num-threads iterations) seconds))

    (loop for fail in fails
       initially (format t "HW transactions success rate: ")
       do (format t "~5$% " (* 100 (1+ (- (/ fail (float iterations))))))
       finally (format t "~%"))))

