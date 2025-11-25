# SHIELDWALL

SHIELDWALL is a Common Lisp library for creating, organizing, & running unit tests

# Core Concepts

Tests are defined with SHIELD and grouped with WITH-SHIELD-GROUP to form a hierarchy. SHIELD-FILE loads tests from files. WITH-SHIELD-CONFIG allows dynamic configuration of test run execution.

# Key Forms & Syntax

# 1. SHIELD (describe expect try)
  describe: A string describing the test
  expect: The expected result
  try: The Lisp form to evaluate
  Example: (SHIELD "Addition works" 3 (+ 1 2))

# 2. SHIELD ((describe &key test expect-error-p setup skip) expect try)
  describe: Description of the test
  :test: Custom comparison function (default: EQUAL)
  :expect-error-p: Expect an error of type expect if true
  :setup: A function taking a thunk (function to run the test) for setup/teardown
  :skip: If true, the test is skipped
  Example: (shield ("list equality" :test #'equalp) '(1 2) (LIST 1 2))
  Example (with setup): (shield ("increment" :setup (lambda (thunk) (incf *counter*) (funcall thunk))))

# 3. WITH-SHIELD-GROUP (describe &body body)
  describe: Description of the group
  body: Contains SHIELD, WITH-SHIELD-GROUP, or SHIELD-FILE forms
  Example: (WITH-SHIELD-GROUP "Math tests" (SHIELD "..." ...) (SHIELD "..."))

# 4. WITH-SHIELD-GROUP ((describe &key directory setup skip) &body body)
  describe: Description of the group
  :directory: Specifies a directory for relative SHIELD-FILE paths
  :setup: Setup function for the group
  :skip: Skips all tests in the group if true
  Example: (WITH-SHIELD-GROUP ("File tests" :directory "test-files") (SHIELD-FILE "file1.lisp"))

# 5. SHIELD-FILE (filespec &key skip describe directory)
  filespec: Path to the test file
  :describe, :skip, :directory: Applied to the implicit group created
  Example: (SHIELD-FILE "my-tests.lisp")

# 6. WITH-SHIELD-CONFIG ((&key filter directory output stop-on-first-fail-p suppress-errors-p verbose-fail-p verbose-nonfail-p) &body body)
  :filter: List of strings for selecting tests by their hierarchical path (substring matching). Use "" or T as wildcard levels
  :directory: Base directory for resolving paths. Can be a pathname or an ASDF system keyword
  :output: Destination for test reports (T for stdout, a pathname for a file, list to provide pathname with keyword args to pass to OPEN, NIL to suppress)
  :stop-on-first-fail-p: Stop tests after the first failure if true
  :suppress-errors-p: Treat errors during tests as failures (no Lisp error)
  :verbose-fail-p: don't print detailed info about each failing tests/groups if NIL
  :verbose-nonfail-p: print more detailed info about passing & skipped tests if T
  Example (filtering): (WITH-SHIELD-CONFIG (:filter '("math" "addition")) ...)
  
# Key Special Variables
Special variables set by WITH-SHIELD-CONFIG:
  *SHIELDWALL-DIRECTORY*: Base path for files (pathname or )
  *SHIELDWALL-FILTER-PATH*: List of strings for test filtering
  *SHIELDWALL-OUTPUT*: Output destination (T, NIL, or stream)
  *SHIELDWALL-STOP-ON-FIRST-FAIL-P*: Stop at first failure
  *SHIELDWALL-SUPPRESS-ERRORS-P*: Treat errors as test failures
  *SHIELDWALL-TEST*: default comparison test (defaults to CL:EQUAL)
  *SHIELDWALL-VERBOSE-FAIL-P*: Print detailed info about each failing tests
  *SHIELDWALL-VERBOSE-NONFAIL-P*: Print detailed info about passing & skipped tests/groups
Other:
  *LAST-FAILED-SHIELD*: Holds the last failed test object (for inspection)

# Common mistakes
 - EXPECT is the first parameter to SHIELD. TRY is the second. There are no EXPECT/TRY functions/macros. There are no :EXPECT/:TRE keywords.
 - There should NEVER be any keywords after EXPECT & TRY. To add keyword parameters to a test, bundle them with the description (before EXPECT/TRY) like this: "my test" -> ("my test" :test #'equalp). 
 - in order for a SHIELD's EXPECT & TRY arguments to have access to the same environment you need to either: (A) put the whole SHIELD inside of a LET form or (B) use a :SETUP form

Use this information to understand SHIELDWALL and generate unit tests using its API.
