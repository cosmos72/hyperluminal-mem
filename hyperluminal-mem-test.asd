;; -*- lisp -*-

;; This file is part of HYPERLUMINAL-MEM.
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




(in-package :cl-user)

(asdf:defsystem :hyperluminal-mem-test
  :name "HYPERLUMINAL-MEM-TEST"
  :version "0.6.2"
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :description "test suite for hyperluminal-mem"

  :depends-on (:log4cl
               :fiveam
               :hyperluminal-mem)

  :components ((:module :test
                :components ((:file "package")
                             (:file "magic"         :depends-on ("package"))
                             (:file "mem"           :depends-on ("package"))
                             (:file "memcpy"        :depends-on ("mem"))
                             (:file "abi"           :depends-on ("mem"))
                             (:file "string"        :depends-on ("abi"))
                             (:file "stmx-objects"  :depends-on ("abi"))
                             (:file "run-suite"     :depends-on ("package")))))

  :perform (asdf:test-op
            (o c)
            (eval (read-from-string "(fiveam:run! 'hyperluminal-mem-test:suite)"))))
