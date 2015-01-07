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

;;;; * HYPERLUMINAL-DB.TEST

(in-package :cl-user)

(defpackage #:hyperluminal-db.test

  (:nicknames #:hldb.test)

  (:use #:cl
        #:fiveam
        #:stmx.util
        #:hyperluminal-mem
        #:hyperluminal-db)

  (:import-from #:hyperluminal-mem
                #:!mdump)

  (:import-from #:hyperluminal-db
                #:hldb-open #:hldb-close)

  (:export #:suite))




(in-package :hyperluminal-db.test)

(fiveam:def-suite suite)
