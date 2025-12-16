
(defpackage :shieldwall-system
  (:use :cl :asdf))

(in-package :shieldwall-system)

(defsystem :shieldwall
  :depends-on (:asdf :uiop)
  :description "
SHIELDWALL: Common Lisp unit test framework. A simple & lightweight way to organize & run large hierarchical test suites across multiple files &/or ASDF systems

Choose SHIELDWALL when you want...
1. A simple, lightweight testing API (at the expense of missing out on specialized testing features)
2. Minimal dependencies (just ASDF/UIOP)
3. To organize a large hierarchical test suite with deep nesting, spanning across multiple files & ASDF packages
4. Fine-grained filtering (via XPATH-like path queries) to run specific subsets of a large corpus of tests
"
  :author "Nick Allen <nallen05@gmail.com>"
  :version "0.1.2"
  :serial t
  :components ((:file shieldwall)))
