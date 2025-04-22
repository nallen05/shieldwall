

;;;; clickaround tests

;; some passing tests

(shieldwall:shield "An a la carte test"
                   3
                   (+ 1 2))

(shieldwall:with-shield-group "Passing tests"
  (shieldwall:with-shield-group "Subgroup 1"
    (shieldwall:with-shield-group "Sub-subgroup 1.1"
      (shieldwall:shield "A simple nested test"
                         3
                         (+ 1 2)))
    (shieldwall:with-shield-group "Subgroup 2"
      (shieldwall:with-shield-group "Sub-subgroup 2.1"
        (shieldwall:shield ("A more complex example" :test #'equalp)
                           '(1 2 (3))
                           (list 1 2 (list 3)))))))

;; some failing tests

(shieldwall:shield "Failing singleton test"
                   3
                   (+ 1 2 3))

(shieldwall:with-shield-group "These tests should fail. Oh no!"
  (shieldwall:with-shield-group "1.1"
    (shieldwall:with-shield-group "1.1.2"
      (shieldwall:shield "A simple failing test"
                         3
                         (+ 1 2 3)))
    (shieldwall:with-shield-group "1.2"
      (shieldwall:with-shield-group "1.2.1"
        (shieldwall:shield ("More complex failing test" :test #'equalp)
                           '(1 2 (3 5))
                           (list 1 2 (list 3)))))))


;; tests with unexpected errors (suppressed)

(shieldwall:with-shield-config (:suppress-errors-p t)
  (shieldwall:shield "An a la carte test"
                     3
                     (+ 1 "2")))

(shieldwall:with-shield-config (:suppress-errors-p t)
  (shieldwall:with-shield-group "All these tests should pass"
    (shieldwall:with-shield-group "1.1"
      (shieldwall:with-shield-group "1.1.2"
        (shieldwall:shield "A simple nested test"
                           3
                           (+ 1 "2")))
      (shieldwall:with-shield-group "1.2"
        (shieldwall:with-shield-group "1.2.1"
          (shieldwall:shield ("A more complex example" :test #'equalp)
                             '(1 2 (3))
                             (list 1 2 (list 3))))))))


;; tests with expected errors
(shieldwall:shield ("Singleton test with the right error" :expect-error-p t)
                   'cl:error
                   (+ 1 "2"))

(shieldwall:shield ("Singleton test with the wrong error" :expect-error-p t)
                   'cl:warning
                   (+ 1 "2"))

(shieldwall:with-shield-group "This test has an error!"
  (shieldwall:shield ("Test with an error" :expect-error-p t)
                     'cl:error
                     (+ 1 "2")))

(shieldwall:with-shield-config (:suppress-errors-p t)
  (shieldwall:with-shield-group "This test has an error!"
    (shieldwall:shield ("Test with an error" :expect-error-p t)
                       'cl:error
                       (+ 1 "2"))))


;; muffling failures
(shieldwall:with-shield-config (:verbose-fail-p nil)
  (shieldwall:shield "A failing singleton test"
                     3
                     (+ 1 2 3)))

(shieldwall:with-shield-config (:verbose-fail-p nil)
  (shieldwall:with-shield-group "These tests should fail. Oh no!"
    (shieldwall:with-shield-group "1.1"
      (shieldwall:with-shield-group "1.1.2"
        (shieldwall:shield "A simple failing test"
                           3
                           (+ 1 2 3)))
      (shieldwall:with-shield-group "1.2"
        (shieldwall:with-shield-group "1.2.1"
          (shieldwall:shield ("A more complex failing test" :test #'equalp)
                             '(1 2 (3 5))
                             (list 1 2 (list 3))))))))

(shieldwall:with-shield-config (:suppress-errors-p t
                                :verbose-fail-p t)
  (shieldwall:with-shield-group "This test has an error!"
    (shieldwall:shield "Test with an error"
                       3
                       (+ 1 "2"))))


;; stop on first fail

(shieldwall:with-shield-config (:stop-on-first-fail-p t)
  (shieldwall:with-shield-group "These tests should fail. Oh no!"
    (shieldwall:with-shield-group "1.1"
      (shieldwall:with-shield-group "1.1.2"
        (shieldwall:shield "A simple failing test"
                           3
                           (+ 1 2 3)))
      (shieldwall:with-shield-group "1.2"
        (shieldwall:with-shield-group "1.2.1"
          (shieldwall:shield ("A more complex failing test" :test #'equalp)
                             '(1 2 (3 5))
                             (list 1 2 (list 3))))))))

;; skipped tests

(shieldwall:with-shield-group "Foobar tests"
  (shieldwall:with-shield-group "1"
    (shieldwall:shield "a simple test 1.1"
                       3
                       (+ 1 2))
    (shieldwall:shield ("a simple test 1.2" :skip t)
                       6
                       (+ 1 2 3)))
  (shieldwall:with-shield-group "2"
    (shieldwall:with-shield-group ("2.1" :skip t)
      (shieldwall:shield ("A more complex failing test" :test #'equalp)
                         '(1 2 (3 5))
                         (list 1 2 (list 3))))))


;; setup & teardown
(defvar *cowbell* 0)

(defun call-with-more-cowbell (thunk)
  (let ((*cowbell* (1+ *cowbell*)))
    (declare (special *cowbell*))
    (funcall thunk)))

(shieldwall:with-shield-group "Cowbell tests"
  (shieldwall:shield ("1 cowbell" :setup 'call-with-more-cowbell)
                     1
                     *cowbell*)
  (shieldwall:with-shield-group ("More cowbell" :setup 'call-with-more-cowbell)
    (shieldwall:shield ("2 cowbell" :setup 'call-with-more-cowbell)
                       2
                       *cowbell*)
    (shieldwall:with-shield-group ("Even more cowbell" :setup 'call-with-more-cowbell)    
      (shieldwall:shield ("3 cowbell" :setup 'call-with-more-cowbell)
                         3
                         *cowbell*))))




;; path based filtering

(flet ((run ()
         (shieldwall:with-shield-group "Foobar tests"
           (shieldwall:with-shield-group "1"
             (shieldwall:shield "a simple test 1.1"
                                3
                                (+ 1 2))
             (shieldwall:shield "a simple test 1.2"
                                6
                                (+ 1 2 3)))
           (shieldwall:with-shield-group "2"
             (shieldwall:with-shield-group "2.1"
               (shieldwall:shield ("A more complex failing test" :test #'equalp)
                                  '(1 2 (3 5))
                                  (list 1 2 (list 3))))))))
  (shieldwall:with-shield-config (:filter '("Foo" "1"))
    (shieldwall:format-shieldwall "~%FILTER TEST 1")
    (run))
  (shieldwall:with-shield-config (:filter '("Foo" "2" "1"))
    (shieldwall:format-shieldwall "~%FILTER TEST 2")
    (run)))


;; redirecting to a file

(shieldwall:with-shield-config (:directory :shieldwall
                                :output "~/cl/shieldwall-test-reports/test-run.txt")
  (shieldwall:with-shield-group "Foobar tests"
    (shieldwall:with-shield-group "1"
      (shieldwall:with-shield-group "1.2"
        (shieldwall:shield "a simple test 1"
                           3
                           (+ 1 2))
        (shieldwall:shield "a simple test 2"
                           3
                           (+ 1 2 3)))
      (shieldwall:with-shield-group "1.2"
        (shieldwall:with-shield-group "1.2.1"
          (shieldwall:shield ("A more complex failing test" :test #'equalp)
                             '(1 2 (3 5))
                             (list 1 2 (list 3))))))))


  


;;;; shield-files

(shieldwall:shield-file "test/clickaround-test-asset.lisp"
                        :directory :shieldwall)

(shieldwall:shield-file "test/clickaround-test-asset.lisp"
                        :directory :shieldwall
                        :describe "File full of tests")

(shieldwall:shield-file "test/clickaround-test-asset.lisp"
                        :skip t)

(shieldwall:with-shield-config (:filter '(t "1" "1" "2")
                                :directory :shieldwall)
  (shieldwall:shield-file "test/clickaround-test-asset.lisp"))
