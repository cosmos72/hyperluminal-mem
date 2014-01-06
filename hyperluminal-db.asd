;; -*- lisp -*-

;; This file is part of HYPERLUMINAL-DB.
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



(in-package :cl-user)

(asdf:defsystem :hyperluminal-db
  :name "HYPERLUMINAL-DB"
  :version "0.0.1"
  :license "GPLv3"
  :author "Massimiliano Ghilardi"
  :description "Persistent, transactional object store"

  :depends-on (:log4cl
               :cffi
               :osicat
               :closer-mop
               :bordeaux-threads
               :stmx
               :trivial-garbage)

  :components ((:static-file "hyperluminal-db.asd")

               (:module :src
                :components ((:file "package")
                             (:file "mem"         :depends-on ("package"))
                             (:file "constants"   :depends-on ("mem"))
                             (:file "unboxed"     :depends-on ("constants"))
                             (:file "box"         :depends-on ("unboxed"))
                             (:file "bignum"      :depends-on ("box"))
                             (:file "string"      :depends-on ("box"))
                             (:file "boxed"       :depends-on ("bignum" "string"))
                             (:file "store"       :depends-on ("boxed"))))))


(asdf:defsystem :hyperluminal-db.test
  :name "HYPERLUMINAL-DB.TEST"
  :version "0.0.1"
  :author "Massimiliano Ghilardi"
  :license "GPLv3"
  :description "test suite for hyperluminal-db"

  :depends-on (:log4cl
               :fiveam
               :hyperluminal-db)

  :components ((:module :test
                :components ((:file "package")
                             (:file "mem"           :depends-on ("package"))
                             (:file "abi"           :depends-on ("mem"))))))


(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :hyperluminal-db))))
  (asdf:load-system :hyperluminal-db.test)
  (eval (read-from-string "(fiveam:run! 'hyperluminal-db.test:suite)")))
