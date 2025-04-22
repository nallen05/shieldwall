

;;

(shieldwall:with-shield-group "Test group 1"
  (shieldwall:with-shield-group "Subgroup 1.1"
    (shieldwall:with-shield-group "Simple tests 1.1.1"
      (shieldwall:shield "Simple passing test"
                         3
                         (+ 1 2))
      (shieldwall:shield "Simple failing test"
                         3
                         (+ 1 2 3)))
    (shieldwall:with-shield-group "Slightly more complex tests 1.1.2"
      (shieldwall:shield ("A slightly more complicated test" :test #'equalp)
                         '(1 2 (3 5))
                         (list 1 2 (list 3)))
      (shieldwall:shield ("WTF! test with an error" :expect-error-p t)
                         3
                         (+ 1 "2")))))




(shieldwall:with-shield-group ("Test group 2" :skip t)
  (shieldwall:with-shield-group "Simple tests 1.2.1"
    (shieldwall:shield "Simple passing test"
                       3
                       (+ 1 2))
    (shieldwall:shield ("WTF! test with an error" :expect-error-p t)
                       3
                       (+ 1 "2"))))
