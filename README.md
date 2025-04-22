

# SHIELDWALL

Common Lisp unit test framework. A simple & lightweight way to organize & run large hierarchical
test suites across multiple files &/or ASDF systems

Choose SHIELDWALL when you want...
1. To organize a large hierarchical test suite with deep nesting, spanning across multiple files
   &/or ASDF systems
2. Fine-grained filtering (via XPATH-like path queries) to run specific subsets of tests
3. Minimal dependencies (just ASDF/UIOP)
4. Simplicity & a lightweight API (at the expense of missing out on specialized testing features)


## Installation

```lisp
(pushnew #p"~/path/to/shieldwall.asd" asdf:*central-registry* :test #'equal)
(require :shieldwall)
```


## Dependencies

SHIELDWALL uses UIOP for file system interaction. Since UIOP is already distributed as part of
ASDF, there are no additional dependencies after you install it with ASDF.


## Quick Start

### Writing tests & organizing them into groups

```lisp
(shieldwall:with-shield-group "Example group 1"
  (shieldwall:with-shield-group "Example subgroup 1.2"
    (shieldwall:shield "Simple test"
                       3 
                       (+ 1 2))
    (shieldwall:shield ("Change the test function" :test #'equalp)
                       '(1 2 (3))
                       (list 1 2 (list 3)))
    (shieldwall:shield ("Verify certain actions trigger an error" :expect-error-p t)
                       'cl:error
                       (+ 1 "2"))
    (shieldwall:shield-file "test.lisp" 
                            :describe "run tests in a file, treating them the same as a group")))
```


### Running only a specific subset of tests (using XPATH-like path filter)

```lisp
;; run test 2.1.3, found 3 levels deep inside of file "test-b.lisp"
(shieldwall:with-shield-config (:filter '("b" "2" "1" "3"))
  (shieldwall:shield-file "test-a.lisp")
  (shieldwall:shield-file "test-b.lisp"))
```


### Setting specific tests & test groups to always be skipped


```lisp
(shieldwall:with-shield-group ("This group of tests is skipped" :skip t)
  ;; never evaluated
)

(shieldwall:shield ("This test is skipped" :skip t)
                   never-evaluated
                   never-evaluated)
```

Note: SKIP is evaluated, so you can use a dynamic predicate


### Test setup / teardown / fixtures / mocks / etc

You can add a :SETUP form to any test or test group. It should be a function that takes a single
argument: a "thunk" (function with zero arguments) passed to it by SHIELDWALL. Calling it runs
the test/group.
- to implement test setup, do it before calling the thunk
- to implement test teardown, do it after calling the thunk
- you can also LET special variables before calling the thunk, to further configure the 
  environment


```lisp
(defvar *cowbell* 0)

(defun call-with-cowbell (thunk)
  (let ((*cowbell* (1+ *cowbell*)))
    (declare (special *cowbell*))
    (funcall thunk)))
  
(shieldwall:with-shield-group ("Test cowbell" :setup 'call-with-cowbell)
  (shieldwall:with-shield-group ("Test even more cowbell" :setup 'call-with-cowbell)
    (shieldwall:shield ("So much cowbel!" :setup 'call-with-cowbell)
                       3
                       *cowbell*)))
```



## API

### SHIELD (describe expect try)
### SHIELD ((describe &key expect-error-p setup skip test) expect try)

Runs a unit test, comparing the test form TRY vs the expected value EXPECT
- by default: values are compared ith EQL. Signalled error conditions compared with SUBTYPEP.
- DESCRIBE is a string printed in the test report if the test fails. It's also used by
  WITH-SHIELD-CONFIG's pattern-matching :FILTER feature

In the primary syntax, DESCRIBE should be a string literal

```lisp
(shield "a simple test"
        6
        (+ 1 2 3))
```

But in the secondary advanced syntax, it can be a list, allowing for keyword arguments to be
passed:

```lisp
(shield ("a simple test" :test #'equalp)
        '(1 2 (3))
        (list 1 2 (list 3)))
```

In this advanced syntax:
- the first element of the list should evaluate to the description string
- the rest of the list should be a plist, which may contain any of the following keyword args:
  - EXPECT-ERROR-P - if non-null, then the test will fail unless evaluating TRY triggers an error
                     of type EXPECT
  - SETUP - a setup function
  - SKIP - if non-null, neither EXPECT nor TRY are evaluted, & the test is skipped
  - TEST - customize the function used to compare TRY & EXPECT. If note provided, values are 
           compared with EQL, & signalled error conditions by SUBTYPEP


### WITH-SHIELD-GROUP (describe &body body)
### WITH-SHIELD-GROUP ((describe &key directory setup skip) &body body)

a macro used to organize a group of tests.
- DESCRIBE is a string printed in the test report if the test fails. It's also used by
  WITH-SHIELD-CONFIG's pattern-matching :FILTER feature

In the primary syntax, DESCRIBE should be a string literal

```lisp
(with-shield-group "test group"
  (shield-file "test.lisp"))
```

But in the secondary advanced syntax, it can be a list, allowing for keyword arguments to be
passed:

```lisp
(with-shield-group ("test group" :directory mydirectory
                                 :skip "Expected failure. Skip for now)
  (shield-file "test.lisp"))
```

In this advanced syntax:
- the first element of the list should evaluate to the description string
- the rest of the list should be a plist, which may contain any of the following keyword args:
  - DIRECTORY - see: `*shieldwall-directory*`
  - SETUP - a setup function
  - SKIP - if non-null, then BODY is never evaluated


### SHIELD-FILE (filespec &key skip describe directory)

loads the lisp file FILESPEC, compiling if necessary.
- This is done within an implicit WITH-SHIELD-GROUP, which SKIP, DESCRIBE, & DIRECTORY are all 
  passed to.
- for safety, SHIELD-FILE will error if asked to load a file outside of DIRECTORY(!)


### WITH-SHIELD-CONFIG ((&key directory filter line-width muffle-failures-p output stop-on-first-fail-p suppress-errors-p) &body body)

a macro used to configure the behavior of test run
- WITH-SHIELD-CONFIG has the ability to set several special variables. If you wanted to, you
  just do this yourself with SETF. However, it's recommend that your code use WITH-SHIELD-CONFIG
  instead, to reduce the chance of settings leaking from your test suite. Additionally, there are
  some helpful special behaviors that can only be accessed by WITH-SHIELD-CONFIG (& aren't
  available if you SETF the variables without it)
- WITH-SHIELD-GROUP does NOT create an implicit WITH-SHIELD-GROUP, so the form itself should be
  transparant to the test report. Feel free to call at any point & as many times as you want 
  inside of your test run

Here is the list of special variables that can be set by WITH-SHIELD-CONFIG:

#### `*SHIELDWALL-DIRECTORY*`

A pathname designator used to help resolve local path name designators.
- Used by SHIELD-FILE to find files. If non-null, SHIELD-FILE will not load files that are
  outside of `*SHIELDWALL-DIRECTORY*`
- also by WITH-SHIELD-CONFIG when redirecting test report output (`*SHIELDWALL-OUTPUT*`) to a file

It can be any of these types:
- If it a pathname (or string that can be converted to a pathname) it is used
- if it is a keyword, then it is assumed to be the name of an ASDF system, & it uses that ASDF
  system's source directory
- If it is NIL, then local pathnames trigger an error

#### `*SHIELDWALL-FILTER-PATH*`

A path-based pattern used to filter tests/groups, similar to an XPATH query.
- If `*SHIELDWALL-FILTER-PATH*` is NIL, then nothing is filtered
- If it is a list of strings, then SHIELDWALL will treat each element as a substring that is tries
  to match against tests & test groups. It will onoy run tests/group that match that pattern.
- `t` or `""` can be used as wildcards to matchany tests/groups at that level in the path

Eg:
```
(shieldwall:with-shield-config (:filter '("file-1" "1.2" "1.2.3" "feature-x"))
  (shieldwall:shield-file "test-file1.lisp")
  )
```
  

#### `*SHIELDWALL-LINE-WIDTH*` 

this is the assumed width of the terminal screen, & can be used to configure the width of 
horizontal divider lines & the line of test characters printed in the terminal


#### `*SHIELDWALL-MUFFLE-FAILURES-P*`

if set to T, then detailed descriptions are not printed in the case of test failures


#### `*SHIELDWALL-OUTPUT*`

This is the output stream that the test report is written to. It can be any of these types:
- if it's a stream, then the test report is written there
- if it's `T`, then test report is written to `*STANDARD-OUTPUT*`
- if it's NIL, nothing is written anywhere

Buy when setting `*SHIELDWALL-OUTPUT*` with WITH-SHIELD-CONFIG :OUTPUT, you can also give it 
these additional options as well:
- if WITH-SHIELD-CONFIG is given a pathname (or string that can be converted a pathname) an output
  stream is opened to that file with options `(:IF-EXISTS :APPEND :IF-DOES-NOT-EXIST :CREATE)`
- if it's given a list, then the first element of the list is treated as the FILESPEC & the rest
  of the list is a plist, allowing keywords to be passed to OPEN
       

#### `*SHIELDWALL-STOP-ON-FIRST-FAIL-P*`
       
if set to T, then the test run will immedietly stop the first time it encounters an error
- you can inspect that failed test via: `*LAST-FAILED-SHIELD*`


#### `*SHIELDWALL-SUPPRESS-ERRORS-P*`

if set to T, then errors inside of tests are treated as test failtures (within signaling a lisp
error condition)


### FORMAT-SHIELDWALL (control-string &rest format-arguments)

if needed, you can be used to write something to the test report. This is exposed more for
troubleshooting than normal use

### `*LAST-SHIELD-GROUP*`

an object representing the last SHIELD-GROUP created as part of a test run


### `*LAST-FAILED-SHIELD*`

an object representing the last SHIELD that failed a test


### `SHIELD-DESCRIBE-PATH (&key (shield *last-failed-shield*))`

Useful to look up the filter path to a shield object, for use with WITH-SHIELD-CONFIG :FILTER


## Advanced usage

## Using an LLM to generate SHIELDWALL unit tests

The [SHORT-README.txt](SHORT-README.txt) is good for LLMS (& human programmers who don't like
long documentation) to learn how to write unit tests with SHIELDWALL. Add it to the context 
ahead of your prompt.


### ASDF integration

For convenience, the source directory of ASDF systems can be referenced as the keyword name of
that system


```lisp
(shieldwall:with-shield-group ("My system tests" :directory :my-system)
  (shieldwall:shield-file "test/test1.lisp")
  (shieldwall:shield-file "test/test2.lisp")]
```

## Redirect test report output to a file

if you are running a very large test suites test with lot of failures, it's possible that the
output may be so verbose that it overwhelmes slime/EMACS. This can be worked around by redirecting
the test report output to a file:


```lisp
(shieldwall:with-shield-config (:output "~/test-run.txt" :suppress-errors-p t)
  (shieldwall:shield-file "test.lisp"))
```

### Inspecting test failures in the REPL

```lisp
> (shieldwall:with-shield-config (:stop-on-first-fail-p t)
    (shieldwall:shield-file "test2.lisp"))
#<shieldewall-test-run ...>

> (shieldwall:shieldwall-test-form shieldwall:*last-failed-shield*)
(+ 1 2 3)

> (shieldwall:shieldwall-test-got shieldwall:*last-failed-shield*)
6
```

## License

[MIT license](LICENSE) by Nick Allen <nallen05@gmail.com>
