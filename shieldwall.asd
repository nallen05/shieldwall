
(defpackage :shieldwall-system
  (:use :cl :asdf))

(in-package :shieldwall-system)

(defsystem :shieldwall
  :depends-on (:asdf :uiop)
  :description "
SHIELDWALL: Common Lisp unit test framework. A simple & lightweight way to organize & run large hierarchical
test suites across multiple files &/or ASDF systems

Choose SHIELDWALL when you want...
1. To organize a large hierarchical test suite with deep nesting, spanning across multiple files & ASDF packages
2. Fine-grained filtering (via XPATH-like path queries) to run specific subsets of tests
3. Minimal dependencies (just ASDF/UIOP)
4. Simplicity & a lightweight API (at the expense of missing out on specialized testing features)
"
  :author "Nick Allen <nallen05@gmail.com>"
  :version "0.1"
  :serial t
  :components ((:file shieldwall)))
