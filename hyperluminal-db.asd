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
  :version "0.5.1"
  :license "GPLv3"
  :author "Massimiliano Ghilardi"
  :description "Persistent, transactional object store. Also includes a serialization library."

  :depends-on (:log4cl
               #-abcl :cffi
               #-abcl :osicat
               :closer-mop
               :bordeaux-threads
               :stmx
               :trivial-garbage)

  :components
  ((:static-file "hyperluminal-db.asd")
	       
   (:module :lang
    :components ((:file "package")
		 (:file "lang"           :depends-on ("package"))))
                  

   (:module :ffi
    :components ((:file "package")
		 (:file "ffi"            :depends-on ("package"))
		 (:file "struct"         :depends-on ("ffi"))
		 (:file "os"             :depends-on ("ffi")))
    :depends-on (:lang))
                  
                               
   #+(and sbcl (or x86 x86-64 #|... other archs here ...|#))
   (:module :sbcl
    :components ((:file "package")
		 (:file "compiler"       :depends-on ("package"))
		 #+(or x86 x86-64)
		 (:file "x86"            :depends-on ("compiler")))
    :depends-on (:ffi))
                  
   (:module :mem
    :components ((:file "package")
		 (:file "lang"           :depends-on ("package"))
		 (:file "version"        :depends-on ("lang"))
		 (:file "defs"           :depends-on ("lang"))
		 (:file "mem"            :depends-on ("defs"))
		 (:file "ffi-late"       :depends-on ("mem"))
		 (:file "constants"      :depends-on ("ffi-late"))
		 (:file "symbols"        :depends-on ("constants"))
		 (:file "unboxed"        :depends-on ("symbols"))
		 (:file "box"            :depends-on ("version" "unboxed"))
		 (:file "magic"          :depends-on ("box"))
		 
		 (:file "box/bignum"     :depends-on ("box"))
		 (:file "box/ratio"      :depends-on ("box/bignum"))
		 (:file "box/float"      :depends-on ("box"))
		 (:file "box/complex"    :depends-on ("box/float" "box/ratio"))
		 (:file "box/pathname"   :depends-on ("box"))
		 (:file "box/hash-table" :depends-on ("box"))
		 (:file "box/list"       :depends-on ("box"))
		 (:file "box/array"      :depends-on ("box"))
		 (:file "box/vector"     :depends-on ("box/array"))
		 (:file "box/string-utf-21" :depends-on ("box/vector"))
		 (:file "box/string-utf-8"  :depends-on ("box/vector"))
		 (:file "box/string-base"   :depends-on ("box/vector"))
		 (:file "box/bit-vector" :depends-on ("box/vector"))
		 (:file "box/symbol"     :depends-on ("box"))

		 (:file "mvar"           :depends-on ("box"))
		 (:file "struct"         :depends-on ("mvar"))
		 (:file "object"         :depends-on ("struct"))
		 (:file "object/gmap"    :depends-on ("object"))
		 (:file "object/ghash-table" :depends-on ("object"))
		 (:file "object/tcell"   :depends-on ("object"))
		 (:file "object/tstack"  :depends-on ("object"))

		 (:file "boxed"          :depends-on ("box"
						      "box/bignum"
						      "box/ratio"
						      "box/float"
						      "box/complex"
						      "box/pathname"
						      "box/hash-table"
						      "box/list"
						      "box/array"
						      "box/vector"
						      "box/string-utf-21"
						      "box/string-utf-8"
						      "box/string-base"
						      "box/bit-vector"
						      "box/symbol"
						      "object")))
    :depends-on (:ffi
		 #+(and sbcl (or x86 x86-64)) :sbcl))

   (:module :db
    :components ((:file "package")
		 (:file "version"        :depends-on ("package"))
		 (:file "ffi-btree"      :depends-on ("version"))
		 (:file "box"            :depends-on ("version"))
		 (:file "alloc"          :depends-on ("box"))
		 (:file "store"          :depends-on ("alloc")))
    :depends-on (:mem))))


(asdf:defsystem :hyperluminal-db.test
  :name "HYPERLUMINAL-DB.TEST"
  :version "0.5.1"
  :author "Massimiliano Ghilardi"
  :license "GPLv3"
  :description "test suite for hyperluminal-db"

  :depends-on (:log4cl
               :fiveam
               :hyperluminal-db)

  :components ((:module :test
                :components ((:file "package")
                             (:file "mem"           :depends-on ("package"))
                             (:file "abi"           :depends-on ("mem"))
                             (:file "stmx-objects"  :depends-on ("abi"))))))


(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :hyperluminal-db))))
  (asdf:load-system :hyperluminal-db.test)
  (eval (read-from-string "(fiveam:run! 'hyperluminal-db.test:suite)")))
