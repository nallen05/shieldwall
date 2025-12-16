

(defpackage :shieldwall
  (:use :cl)
  (:export ;; writing tests
           :shield
           :shield-condition

           ;; organizing tests into groups
           :with-shield-group
           :shield-file
   
           ;; configuration
           :with-shield-config
           :*shieldwall-directory*
           :*shieldwall-filter-path*
           :*shieldwall-line-width*
           :*shieldwall-output*
           :*shieldwall-stop-on-first-fail-p*
           :*shieldwall-suppress-errors-p*
           :*shieldwall-test*
           :*shieldwall-verbose-fail-p*
           :*shieldwall-verbose-nonfail-p*
   
           ;; utility
           :format-shieldwall
           
           ;; debugging test results
           :*last-failed-shield*
           :shield
           :shield-created-at
           :shield-describe
           :shield-describe-path
           :shield-expect
           :shield-expect-error-p
           :shield-encounted-error-p
           :shield-test-function
           :shield-form
           :shield-got
           :shield-passed-p

           ;; roll-up stats
           :*last-shield-group*
           :shield-group
           :shield-group-created-at
           :shield-group-stopped-on-first-fail-p
           :shield-group-results
           ))

(in-package :shieldwall)


;;;; variables

;; debugging help
(defvar *last-failed-shield* nil)
(defvar *last-shield-group*  nil)

;; configuration
(defparameter *shieldwall-directory*            nil)
(defparameter *shieldwall-filter-path*          nil)
(defparameter *shieldwall-line-width*           35)
(defparameter *shieldwall-output*               t)
(defparameter *shieldwall-stop-on-first-fail-p* nil)
(defparameter *shieldwall-suppress-errors-p*    t)
(defparameter *shieldwall-test*                 'cl:equalp)
(defparameter *shieldwall-verbose-fail-p*       t)
(defparameter *shieldwall-verbose-nonfail-p*    nil)

;; internal only
(defvar *%in-test-run*        nil)
(defvar *%line-position*      0)
(defvar *%shield-group-stack* nil)
(defvar *%shield-file-stack*  nil)


;; class definitions

(defstruct %shield-bucket
  (created-at (get-universal-time))
  describe
  setup-function
  skip-p)

(defstruct (shield (:include %shield-bucket))

  ;; filter location
  group-path

  ;; expectation
  expect
  expect-error-p
  test-function

  ;; form to be evaluated
  form

  ;; result
  got
  passed-p
  encountered-error-p)

(defstruct (shield-group (:include %shield-bucket))

  ;; flags
  stopped-on-first-fail-p

  ;; stats
  (test-error-count  0)
  (test-fail-count   0)
  (test-pass-count   0)
  (test-skip-count   0)
  (group-error-count 0)
  (group-skip-count  0))

;; (defstruct shieldwall-test-run
;;   results
;;   stopped-on-first-fail-p
;;   (created-at (get-universal-time)))

(define-condition %shieldwall-stop-test-run! () ())


;; finding files

(defun call-with-shieldwall-directory (thunk pathspec)
  (let ((*shieldwall-directory* (etypecase pathspec
                                  (null nil)
                                  (string  (uiop:pathname-directory-pathname pathspec))
                                  (pathname  (uiop:pathname-directory-pathname pathspec))
                                  (symbol (asdf:system-source-directory pathspec)))))
    (declare (special *shieldwall-directory*))
    (funcall thunk)))

(defmacro with-shieldwall-directory (pathspec &body body)
  `(call-with-shieldwall-directory
    (lambda ()
      "WITH-SHIELDWALL-DIRECTORY body"
      ,@body)
    ,pathspec))

(defun shieldwall-resolve-pathname (pathspec)
  (cond
    ((uiop:absolute-pathname-p pathspec)
     pathspec)
    ((uiop:relative-pathname-p pathspec)
     (if (null *shieldwall-directory*)
         (error "~S is null. Unable to resolve relative pathname designator: ~S"
                                       '*shieldwall-directory*
                                       pathspec)

         (uiop:merge-pathnames* pathspec *shieldwall-directory*)))
    (t (error "Bad pathname designator: ~S" pathspec))))


;; primitives for printing test reports

(defun call-with-shieldwall-output (thunk output
                                    &key (direction :output)
                                         (element-type 'cl:character)
                                         (external-format :default)
                                         (if-exists :append)
                                         (if-does-not-exist :create))
  (cond
    ((or (null output)
         (eql output t)
         (streamp output))
     (let ((*shieldwall-output* (if (eql output t)
                                    *standard-output*
                                    output)))
       (declare (special *shieldwall-output*))
       (funcall thunk)))
    ((or (stringp output)
         (pathnamep output)
         (listp output))
     (with-open-file (*shieldwall-output*
                      (shieldwall-resolve-pathname output)
                      :direction direction
                      :element-type element-type
                      :external-format external-format
                      :if-exists if-exists
                      :if-does-not-exist if-does-not-exist)
       (declare (special *shieldwall-output*))
       (funcall thunk))
     (format t
             "~%;;~%;; SHIELDWALL test report output redirected to file:~%;;~%;;   ~A~%;;~%"
             (shieldwall-resolve-pathname output)))
    (t (error "Bad SHIELDWALL output designator: ~S" output))))

(defmacro with-shieldwall-output (output open-file-args &body body)
  `(apply #'call-with-shieldwall-output
          (lambda ()
            "WITH-SHIELDWALL-OUTPUT body"
            ,@body)
          ,output
          ,open-file-args))

(defun format-shieldwall (fmt-string &rest fmt-args)
  (when *shieldwall-output*
    (apply #'format
           (if (eql *shieldwall-output* t)
               *standard-output*
               *shieldwall-output*)
           fmt-string
           fmt-args)))

(defun format-shieldwall-nested (fmt-string &rest fmt-args)
  (format-shieldwall "~&~A~A"
                     (make-string (length *%shield-group-stack*)
                                  :initial-element #\Space)
                     (apply #'format
                            nil
                            fmt-string
                            fmt-args)))

(defun %print-newline ()
  (format-shieldwall "~%"))

(defun %ensure-newline ()
  (format-shieldwall "~&"))

(defun %print-hline-big ()
  (%ensure-newline)
  (dotimes (i *shieldwall-line-width*)
    (format-shieldwall "=")))

(defun %print-hline-small (&key (left-corner-char #\,))
  (format-shieldwall "~&~A" left-corner-char)
  (let* ((half (/ (1- *shieldwall-line-width*)
                  2))
         (floor (floor half)))
    (dotimes (i floor)
      (format-shieldwall "- "))
    (unless (= half floor)
      (format-shieldwall "-"))))

(defun %reset-line-dots! ()
  (setf *%line-position* 0))

(defun %register-line-dot! ()
  (if (>= *%line-position*
          *shieldwall-line-width*)
      (progn
        (%print-newline)
        (setf *%line-position* 0))
      (incf *%line-position*)))


;; shield group paths & path-based filtering

(defun shield-describe-path (&key (shield *last-failed-shield*))
  (when shield
    (nreverse (cons (shield-describe shield)
                    (mapcar #'shield-group-describe
                            (shield-group-path shield))))))

(defun shieldwall-filter-passes-p (description)
  (if (null *shieldwall-filter-path*)
      t
      (case (first *shieldwall-filter-path*)
        ((nil) nil)
        ((t "") t)
        (otherwise
         (search (first *shieldwall-filter-path*)
                 description)))))


;; configuring test run behavior

(defun call-with-shield-config (thunk &key 
                                        (directory *shieldwall-directory*)
                                        (filter *shieldwall-filter-path*)
                                        (line-width *shieldwall-line-width*)
                                        (output *shieldwall-output*)
                                        (stop-on-first-fail-p *shieldwall-stop-on-first-fail-p*)
                                        (suppress-errors-p *shieldwall-suppress-errors-p*)
                                        (verbose-fail-p *shieldwall-verbose-fail-p*)
                                        (verbose-nonfail-p *shieldwall-verbose-nonfail-p*))
  (with-shieldwall-directory directory
    (with-shieldwall-output
        (if (listp output)
            (first output)
            output)
        (when (listp output)                   
          (rest output))
      (let ((*shieldwall-filter-path* filter)
            (*shieldwall-line-width* line-width)
            (*shieldwall-stop-on-first-fail-p* stop-on-first-fail-p)
            (*shieldwall-suppress-errors-p* suppress-errors-p)
            (*shieldwall-verbose-fail-p* verbose-fail-p)
            (*shieldwall-verbose-nonfail-p* verbose-nonfail-p))
        (declare (special *shieldwall-filter-path*
                          *shieldwall-line-width*
                          *shieldwall-suppress-errors-p*
                          *shieldwall-stop-on-first-fail-p*
                          *shieldwall-verbose-fail-p*
                          *shieldwall-verbose-nonfail-p*))
        (funcall thunk)))))

(defmacro with-shield-config ((&rest args
                               &key directory filter output stop-on-first-fail-p
                                    suppress-errors-p verbose-fail-p verbose-nonfail-p)
                              &body body)
  (declare (ignore directory filter output stop-on-first-fail-p suppress-errors-p
                   verbose-fail-p verbose-nonfail-p))
  `(call-with-shield-config (lambda ()
                              "WITH-SHIELD-CONFIG body"
                              ,@body)
                            ,@args))


;; defining & running tests (aka "shields")

(defun %print-shield-result (shield)
  (cond

    ;; skipped test
    ((shield-skip-p shield)
     (if *shieldwall-verbose-nonfail-p*
         (format-shieldwall-nested "~&SKIP: ~A" (shield-describe shield))
         (progn
           (%register-line-dot!)
           (format-shieldwall "s"))))

    ;; passed test
    ((shield-passed-p shield)
     (if *shieldwall-verbose-nonfail-p*
         (format-shieldwall-nested "~&PASS: ~A" (shield-describe shield))
         (progn
           (%register-line-dot!)
           (format-shieldwall "."))))

    ;; failed test - non-verbose failure output
    ((not *shieldwall-verbose-fail-p*)
     (if *shieldwall-verbose-nonfail-p*
         (format-shieldwall-nested "~&FAIL~A(!): ~A"
                                   (if (and (shield-encountered-error-p shield)
                                            (not (shield-expect-error-p shield)))
                                       " WITH ERROR"
                                       "")
                                   (shield-describe shield))
         (progn
           (%register-line-dot!)
           (format-shieldwall (if (shield-encountered-error-p shield)
                                  "e"
                                  "f")))))

    ;; failed test - verbose failure output
    (t
     (%reset-line-dots!)
     (%print-hline-small :left-corner-char #\,)
     (format-shieldwall "~%|TEST FAILURE")
     (let ((indent -1))
       (dolist (d (shield-describe-path :shield shield))
         (format-shieldwall "~%|~A~A"
                            (make-string (incf indent) :initial-element #\space)
                            d)))
     (%ensure-newline)
     (pprint-logical-block (*shieldwall-output* nil :per-line-prefix "|")
       (when *%shield-file-stack*
         (format-shieldwall "~%in file~A~14T~W"
                            (make-string 8 :initial-element #\space)
                            (or (ignore-errors (uiop:native-namestring (first *%shield-file-stack*)))
                                (first *%shield-file-stack*))))
       (format-shieldwall "~%test form~A~14T~W"
                          (make-string 6 :initial-element #\space)
                          (shield-form shield))     
       (if (shield-encountered-error-p shield)
           (format-shieldwall "~%threw error~A~14T~W"
                              (make-string 4 :initial-element #\space)
                              (shield-encountered-error-p shield))
           (format-shieldwall "~%evaluated to~A~14T~W"
                              (make-string 3 :initial-element #\space)
                              (shield-got shield)))
       (if (shield-expect-error-p shield)
           (format-shieldwall "~%when condition ~14T~W"
                              (shield-expect shield))
           (unless (shield-encountered-error-p shield)
             (format-shieldwall "~%when value~A~14T~W"
                                (make-string 5 :initial-element #\space)
                                (shield-expect shield))))
       (unless (and (shield-encountered-error-p shield)
                    (not (shield-expect-error-p shield)))
         (format-shieldwall "~%was expected~A~14T~W"
                            (make-string 3 :initial-element #\space)
                            (shield-test-function shield))))
     (when (and *shieldwall-stop-on-first-fail-p*
                (not (shield-passed-p shield)))
       (format-shieldwall "~%|")
       (format-shieldwall "~%|stopping test run because ~s is ~s"
                          '*shieldwall-stop-on-first-fail-p*
                          *shieldwall-stop-on-first-fail-p*)
       (format-shieldwall "~%|to inspect: ~S" '*last-failed-shield*))
     (%print-hline-small :left-corner-char #\`)
     (%print-newline))))

(defun call-in-shield (fn describe form &key expect-error-p setup skip test)

  ;; create a new shield object to capture the test result
  (let ((new-shield (make-shield :describe describe
                                 :expect-error-p expect-error-p
                                 :form form
                                 :setup-function setup
                                 :group-path *%shield-group-stack*
                                 :skip-p skip
                                 :test-function (or test
                                                    (if expect-error-p
                                                        'cl:subtypep
                                                        *shieldwall-test* )))))
    
    ;; if not inside of a test group, it will just print a singleton dot
    (unless *%shield-group-stack*
      (%reset-line-dots!)
      (%print-newline))
    
    (when (shieldwall-filter-passes-p describe)
      (if skip
          (when *%shield-group-stack*
            (incf (shield-group-test-skip-count (first *%shield-group-stack*)))
            (%print-shield-result new-shield))

          ;; let's do this! run the test
          (progn
            (handler-case (progn
                            (if setup
                                (funcall setup
                                         (lambda () (funcall fn new-shield)))
                                (funcall fn new-shield))
                            (setf (shield-passed-p new-shield)
                                  (unless expect-error-p
                                    (funcall (coerce (shield-test-function new-shield)
                                                     'function)
                                             (shield-got new-shield)
                                             (shield-expect new-shield)))))
              (error (err)
                (setf (shield-encountered-error-p new-shield)
                      err)
                (if expect-error-p
                    (setf (shield-passed-p new-shield)
                          (ignore-errors 
                           (subtypep (type-of err)
                                     (shield-expect new-shield))))
                    (progn
                      (when *%shield-group-stack*
                        (incf (shield-group-test-error-count (first *%shield-group-stack*))))
                      (setf *last-failed-shield*
                            new-shield)
                      (unless *shieldwall-suppress-errors-p*
                        (error err))))))
            (when *%shield-group-stack*
              (if (shield-passed-p new-shield)
                  (incf (shield-group-test-pass-count (first *%shield-group-stack*)))
                  (incf (shield-group-test-fail-count (first *%shield-group-stack*)))))
            (%print-shield-result new-shield)
            (unless (shield-passed-p new-shield)
              (setf *last-failed-shield*
                    new-shield)
              (when (and *%in-test-run*
                         *shieldwall-stop-on-first-fail-p*)
                (signal '%shieldwall-stop-test-run!))))))
    new-shield))

(defmacro shield (description-and-args expect try)
  (let ((%shield (gensym "shield")))
    `(call-in-shield (lambda (,%shield)
                       "SHIELD body"
                       (setf (shield-expect ,%shield)
                             ,expect

                             (shield-got ,%shield)
                             ,try))
                     ,(if (stringp description-and-args)
                          description-and-args
                          (first description-and-args))
                     ',try
                     ,@(when (listp description-and-args)
                         (rest description-and-args)))))

;; organizing tests into groups (aka "shield groups")

(defun %print-time-to-string (ut)
  (multiple-value-bind (second minute hour day month year dow dst-p tz)
      (decode-universal-time ut)
    (declare (ignore dow dst-p))
    (format nil
            "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~@d:00"
            year
            month
            day
            hour
            minute
            second
            (- tz))))
  
(defun %print-test-run-header (shield-group)
  (%print-newline)
  (%print-hline-big)
  (format-shieldwall "~%TEST RUN STARTED (~A)"
                     (%print-time-to-string (shield-group-created-at shield-group)))
  (when *shieldwall-filter-path*
    (format-shieldwall "~%with filter: ~S"
                       *shieldwall-filter-path*))
  (%print-newline)
  (%reset-line-dots!))

(defun %print-test-run-footer (shield-group)
  (%ensure-newline)
  (%print-hline-big)
  (let* ((now (get-universal-time)))
    (format-shieldwall "~%TEST RUN FINISHED")
    (when *shieldwall-filter-path*
      (format-shieldwall "~%with filter: ~S"
                         *shieldwall-filter-path*))
    (format-shieldwall "~%  ~A total tests run"
                       (+ (shield-group-test-pass-count shield-group)
                          (shield-group-test-fail-count shield-group)))
    (format-shieldwall "~%  ~A passing test~A"
                       (shield-group-test-pass-count shield-group)
                       (if (= (shield-group-test-pass-count shield-group)
                              1)
                           ""
                           "s"))
    (format-shieldwall "~%  ~A failed test~A~A"
                       (shield-group-test-fail-count shield-group)
                       (if (= (shield-group-test-fail-count shield-group)
                              1)
                           ""
                           "s")
                       (if (> (shield-group-test-error-count shield-group)
                              0)
                           (format nil
                                   " (~A error~A)"
                                   (shield-group-test-error-count shield-group)
                                   (if (= (shield-group-test-error-count shield-group)
                                          1)
                                       ""
                                       "s"))
                           ""))
    ;; (when (or (> (shield-group-test-skip-count shield-group)
    ;;              0)
    ;;           (> (shield-group-group-skip-count shield-group)
    ;;              0))
    ;;   (format-shieldwall "~%  ~A test~A~A were skipped"
    ;;                      (shield-group-test-skip-count shield-group)
    ;;                      (if (= (shield-group-test-skip-count shield-group)
    ;;                             1)
    ;;                           ""
    ;;                           "s")
    ;;                      (case (shield-group-group-skip-count shield-group)
    ;;                        (0 "")
    ;;                        (1 " & 1 group")
    ;;                        (otherwise (format nil
    ;;                                           " & ~A groups"
    ;;                                           (shield-group-group-skip-count shield-group))))))
    (let* ((tests (shield-group-test-skip-count shield-group))
           (groups (shield-group-group-skip-count shield-group))
           (total (+ tests groups)))
      (when (> total 0)
        (format-shieldwall "~%  ~A skip~A (~A~A~A)"

                           total
                           (if (= total 1)
                               ""
                               "s")
                           (if (> tests 0)
                               (format nil
                                       "~A test~A"
                                       tests
                                       (if (= tests 1)
                                           ""
                                           "s"))
                               "")
                           (if (and (> tests 0)
                                    (> groups 0))
                               " & "
                               "")
                           (if (> groups 0)
                               (format nil
                                       "~A group~A"
                                       groups
                                       (if (= groups 1)
                                           ""
                                           "s"))
                               ""))))
    (when (> (shield-group-group-error-count shield-group)
             0)
      (format-shieldwall "~%  ~A group~A with an error"
                         (shield-group-group-error-count shield-group)
                          (if (= (shield-group-group-error-count shield-group)
                                  1)
                              ""
                              "s")))
    (let ((msec-duration (* (- now
                               (shield-group-created-at shield-group))
                            1000)))
      (format-shieldwall "~%  ~D millisecond~A elapsed (~A)"
                         msec-duration
                         (if (= msec-duration 1)
                             ""
                             "s")
                         (%print-time-to-string now))))
  (%print-newline)
  (%print-hline-big)
  (%print-newline)
  (%reset-line-dots!))

(defun %print-shield-group-result (shield-group)
  (if *shieldwall-verbose-nonfail-p*
      (if (shield-group-skip-p shield-group)
          (format-shieldwall-nested "SKIPPING WHOLE GROUP: ~A"
                                    (shield-group-describe shield-group))
          (format-shieldwall-nested "~A" (shield-group-describe shield-group)))
      (when (shield-group-skip-p shield-group)
        (%register-line-dot!)
        (format-shieldwall "S"))))

(defun %percolate-up-shield-group-stats (shield-group)
  (when *%shield-group-stack*               
    (incf (shield-group-test-error-count (first *%shield-group-stack*))
          (shield-group-test-error-count shield-group))
    (incf (shield-group-test-fail-count (first *%shield-group-stack*))
          (shield-group-test-fail-count shield-group))
    (incf (shield-group-test-pass-count (first *%shield-group-stack*))
          (shield-group-test-pass-count shield-group))
    (incf (shield-group-test-skip-count (first *%shield-group-stack*))
          (shield-group-test-skip-count shield-group))
    (incf (shield-group-group-error-count (first *%shield-group-stack*))
          (shield-group-group-error-count shield-group))
    (incf (shield-group-group-skip-count (first *%shield-group-stack*))
          (shield-group-group-skip-count shield-group))))

(defun call-with-shield-group (thunk describe &key (directory *shieldwall-directory*)
                                                   setup skip)
  
  ;; create a new SHIELD-GROUP object to capture test results
  (let ((new-shield-group (make-shield-group :describe describe
                                             :setup-function setup
                                             :skip-p skip)))

    ;; if this is the top-most test group, then print a test report header & reset the dots
    (unless *%in-test-run*
      (%print-test-run-header new-shield-group))
   
    (when (shieldwall-filter-passes-p describe)
      (%print-shield-group-result new-shield-group)
      (if skip
          (incf (shield-group-group-skip-count new-shield-group))

          ;; let's do this! run the test group
          (progn
            (with-shieldwall-directory directory
              (block run-group
                (handler-case
                    (let ((*%in-test-run* t)
                          (*%shield-group-stack* (cons new-shield-group
                                                       *%shield-group-stack*))
                          (*shieldwall-filter-path* (rest *shieldwall-filter-path*)))
                      (declare (special *%in-test-run*
                                        *%shield-group-stack*
                                        *shieldwall-filter-path*))
                      (if setup
                          (funcall setup thunk)
                          (funcall thunk)))
                  (%shieldwall-stop-test-run! (_)
                    (declare (ignore _))
                    (if *%in-test-run*
                        (progn
                          (%percolate-up-shield-group-stats new-shield-group)
                          (signal '%shieldwall-stop-test-run!))
                        (progn
                          (setf (shield-group-stopped-on-first-fail-p new-shield-group)
                                *shieldwall-stop-on-first-fail-p*)
                          (return-from run-group))))))))))
    (if *%in-test-run*

        ;; if within another test group, percolate up the stats
        (%percolate-up-shield-group-stats new-shield-group)

        ;; otherwise, if this is the topmost group, print a test report footer
        (progn
          (%print-test-run-footer new-shield-group)
          (finish-output *shieldwall-output*)))

    ;; always return a SHIELD-GROUP object
    (setf *last-shield-group*
          new-shield-group)))

(defmacro with-shield-group (description-and-args &body body)
  `(call-with-shield-group (lambda ()
                           "WITH-SHIELD-GROUP body"
                           ,@body)
                         ,(if (stringp description-and-args)
                              description-and-args
                              (first description-and-args))
                         ,@(when (listp description-and-args)
                             (rest description-and-args))))
  
   


;; organizing tests in files

(defun shield-file (filespec &key describe (directory *shieldwall-directory*) setup skip)
  (with-shield-group ((or describe
                          (ignore-errors (format nil "file: ~S" (uiop:native-namestring filespec)))
                          (format nil "file: ~S" filespec))
                      :directory directory
                      :setup setup
                      :skip skip)
    (let ((src (shieldwall-resolve-pathname filespec)))

      ;; safety checks
      (unless (uiop:file-exists-p src)
        (error "SHIELD-FILE not found: ~S" src))
      (when directory
        (unless (ignore-errors (uiop:subpathp src *shieldwall-directory*))
          (error "SHIELD-FILE: file ~S is outside of allowed directory ~S"
                 src
                 *shieldwall-directory*)))
      (when (member src
                    *%shield-file-stack*
                    :test #'uiop:pathname-equal)
        (error "SHIELD-FILE: refusing to load circular reference! ~S" src))

      ;; continue
      (when *shieldwall-verbose-nonfail-p*
        (format-shieldwall-nested "~%FILE: ~S" (uiop:native-namestring src)))
      (let* ((src-modified-at (uiop:safe-file-write-date src))
             (fasl (uiop:compile-file-pathname* src))
             (fasl-modified-at (when fasl
                                 (uiop:safe-file-write-date fasl))))
        ;; recompile if needed
        (when (or (null fasl-modified-at)
                  (>= src-modified-at
                      fasl-modified-at))
          (let ((uiop:*compile-file-warnings-behaviour* :ignore)
                (uiop:*compile-file-failure-behaviour* :ignore))
            (declare (special uiop:*compile-file-warnings-behaviour*
                              uiop:*compile-file-failure-behaviour*))
            (uiop:compile-file* src
                                :output-file fasl
                                :print nil
                                :verbose nil)))
        
        ;; load it
        (assert fasl)
        (assert (uiop:file-exists-p fasl))
        (let ((*%shield-file-stack* (cons src
                                          *%shield-file-stack*)))
          (declare (special *%shield-file-stack*))
          (load fasl
                :print nil
                :verbose nil))))))
