;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:portch-system
  (:use :cl :asdf))

(in-package #:portch-system)

(defsystem :portch
  :depends-on (:ptester :cl-fad)
  :description 
"a small framework for organizing and running tests written with the `ptester' library.
especially useful when each test or group of tests requires its own files or
directories"
  :author "Nick Allen <nallen05@gmail.com>"
  :version "0.1"
  :components
  ((:file portch)))