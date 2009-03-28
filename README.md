# PORTCH
 
_Portch_ is a small framework for organizing and running tests written with the
_Ptester_ library. It is especially useful when each test or group of tests
requires its own files or directories

## OVERVIEW
 
A "group folder" is a folder containing zero or more "test folders". Each of these
test folders is considered to contain some tests that need to be run in a specific
way.
 
If the name of subfolder of a group folder contains the substring "group" then that
subfolder is considered to be a nested group folder, not a test folder
 
A group folder might have a file "load.lisp" (which contains lisp code to be loaded
before any of the tests in its test folders can be run) and/or a file "run.lisp"
(which contains a single function designator for a function that takes a pathname
pointing to a test folder and runs all the tests in it).
  
the function `LOAD-TEST-GROUP` loads the "load.lisp" forms in a group folder and its
nested group folders. the function `RUN-TEST-GROUP` runs all the tests in a group 
folder and its nested group folders with the appropriate "run.lisp" form
("appropriate" as in nearest ancestor or sibling).
 
if the name of a test folder contains the string "dont-run", or is prepended with an
underscore (`_`), then the folder is skipped by both `LOAD-TEST-GROUP` and
`RUN-TEST-GROUP`.
 
note: _portch_ also exports all the symbols from _ptester_ the portable version of
Franz's [_tester_](http://www.franz.com/support/documentation/6.1/doc/test-harness.htm)
library.
 
## API
 
* `LOAD-TEST-GROUP (FOLDER)`

- - -

   _Function_. Runs all the "load.lisp" files `FOLDER` or any of `FOLDER`s ancenstors
   that are recognized as being a group folder. `GROUP-FOLDER` is searched in
   depth-first, alphabetical order. "load.lisp" files in group folders marked
   "dont-run" are not loaded.
 
* `RUN-TEST-GROUP (FOLDER &REST PATTERNS)`

- - -
 
   _Function_. Runs all the tests in the group-folder `FOLDER` that match `PATTERNS`
   _(I will  document `PATTERNS' when I get a chance --nick)_. Tests are run in depth
   first, alphabetical order.
 
   Running `!RUN-TESTS` with no `PATTERNS'

    (!run-tests folder)

   runs all the tests in `FOLDER`, in order.
 
* `DEF-TEST-GROUP (FOLDER RUN-FUNCTION-NAME LOAD-FUNCTION-NAME &KEY RUN-TESTS)`

- - -

  Concenience _macro_. creates a function named `RUN-FUNCTION-NAME` that
  runs the tests in `FOLDER`.
  
  if `LOAD-FUNCTION-NAME` is non-`NULL`, it also creates a function named `LOAD-FUNCTION-NAME`
  that loads all the tests in `FOLDER`.

  `DEF-TEST-GROUP` loads all the tests in `FOLDER` when encountered.

  If `RUN-TESTS` it T, then `DEF-TEST-GROUP` also _runs_ all the tests in `FOLDER` as well.
