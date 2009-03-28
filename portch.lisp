;
; http://github.com/nallen05/portch
;
; todo: -regexps instead of strings for patterns
;       -regexps for directory boring-ness, etc

(defpackage :portch
  (:use :cl :ptester)
  (:export :load-test-group
	   :run-test-group
	   :def-test-group))

(do-external-symbols (s :ptester)
  (export s :portch))

(in-package :portch)

; util

(defun .foldername (folder)
"
   (.foldername \"/foo/bar/baz/\")

   -> \"baz\"
"
  (first (last (pathname-directory folder))))

(defun .boring-folder-p (folder)
  (equal (mismatch "_" (.foldername folder))
	 1))

(defun .sort-paths-alphabetically (paths)
  (sort paths
	#'string<
	:key (lambda (p)
	       (if (cl-fad:directory-pathname-p p)
		   (.foldername p)
		   (pathname-name p)))))

(defun .alphabetical-directories (folder)
  (.sort-paths-alphabetically (remove-if-not 'cl-fad:directory-pathname-p
					     (cl-fad:list-directory folder))))

; special

(defvar *group*)

(defvar *subgroup-stack*)

(defvar *run-stack*)

; dont run

(defun .marked-dont-run-p (folder)
  (search "dont-run" (.foldername folder)))

; reckognizing groups

(defun .group-folder-p (folder)
  (search "group" (.foldername folder)))

; load-test-group

(defun .get-load-file (folder)
  (cl-fad:file-exists-p (merge-pathnames "load.lisp" folder)))

(defun load-test-group (folder)
  (if (.get-load-file folder)
      (load (compile-file (.get-load-file folder))))
  (mapc 'load-test-group
	(remove-if '.marked-dont-run-p
		   (remove-if-not '.group-folder-p
				  (.alphabetical-directories folder)))))

; run-test-group

(defun .get-run-form (group-folder)
  (let ((f (cl-fad:file-exists-p (merge-pathnames "run.lisp" group-folder))))
    (when f
      (with-open-file (in f)
	(read in)))))

(defun .folder-matches-pattern-p (pattern folder)
  (typecase pattern
    (list (some (lambda (p) (.folder-matches-pattern-p p folder))
		pattern))
    (string (search pattern (.foldername folder)))
    (otherwise (eql pattern t))))

(defun .nice-path-for-debugging (path)
  (enough-namestring path *group*))

(defun .format-test (fmt-string &rest fmt-args)
  (format t
	  "~%~A~A"
	  (make-string (* 3 (length *subgroup-stack*)) :initial-element #\space)
	  (apply 'format nil fmt-string fmt-args)))

(defun .run-test-group (folder &rest patterns)
  (if #1=(search "group" (.foldername folder))
      (.format-test "~A" (replace (copy-seq (.foldername folder))
				  "GROUP"
				  :start1 #1#)))
  (let ((*subgroup-stack* (cons folder *subgroup-stack*)))
    (let* ((new-run (.get-run-form folder))
	   (*run-stack* (if new-run
			    (cons new-run *run-stack*)
			    *run-stack*)))
      (dolist (f (.alphabetical-directories folder))
	(if (.boring-folder-p f)
	    (.format-test "<<<ignoring boring directory ~S>>>"
			  (.nice-path-for-debugging f))
	    (if (.group-folder-p f)
		(apply '.maybe-run-test-group f patterns)
		(when (or (null patterns)
			  (.folder-matches-pattern-p (first patterns) f))
		  (.format-test "~A" (.foldername f))
		  (if (not (first *run-stack*))
		      (.format-test "<<<hmm... ~S must not be a test because I haven't seen a \"run.lisp\" files yet>>>"
				    (.nice-path-for-debugging f))
		      (if (.marked-dont-run-p f)
			  (.format-test "<<<Not running test ~S>>>"
					(replace (copy-seq (.foldername f))
						 "DONT-RUN"
						 :start1 (search "dont-run" (.foldername f))))
			  (funcall (first *run-stack*) f))))))))))

(defun .maybe-run-test-group (folder &optional (pattern t) &rest patterns)
  (if (.folder-matches-pattern-p pattern folder)
      (if (.marked-dont-run-p folder)
	  (.format-test "<<<Not running test group ~A because it's marked \"dont-run\">>>" (.foldername folder))
	  (apply '.run-test-group folder patterns))))

(defun run-test-group (group-folder &rest patterns)
  (let ((*group* group-folder)
	*subgroup-stack*
	*run-stack*)
    (ptester:with-tests (:name (namestring group-folder))
      (apply '.run-test-group group-folder patterns))))

; def-test-group

(defun def-test-group (folder run-function-name load-function-name &key run-tests)

    ; define run-function
    (setf (symbol-function run-function-name)
	  (lambda (&rest patterns) (apply 'run-test-group folder patterns)))

    ; maybe define load-function
    (if load-function-name
	(setf (symbol-function load-function-name)
	      (lambda () (load-test-group folder))))

    ; load test group
    (load-test-group folder)

    ; maybe run tests
    (if run-tests
	(run-test-group folder)))