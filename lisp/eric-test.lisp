;; Very simple unit test infrastructure meant to be used with SLIME

(defpackage :eric-test
  (:use :cl)
  (:export :*deftests*
	   :deftest
	   :run-test
	   :run-all-tests))

(in-package :eric-test)

;; Compiling a DEFTEST form (with C-c C-c) adds it to the *DEFTESTS* plist and runs it
;; so you should see the output immediately. This also means that compiling the whole
;; file will run all the unit tests.

(defvar *deftests* nil "Tests defined by deftest in this session.")

(defun run-test (tested-function)
  (funcall (getf *deftests* tested-function)))

(defun run-all-tests ()
  (loop :for test :in *deftests* :by #'cddr :collect (funcall (getf *deftests* test))))

(defmacro deftest (tested-function args &body body)
  "DEFTEST helps define very simple unit tests.

The key elements are it puts the test in the *deftest* plist so you can run it with (run-test '+),
or (run-all-tests), and it runs the test function immediately upon compilation.

TESTED-FUNCTION is just the symbol the test is identified by.

ARGS is currently unused, but provided for forward compatibility.
"
  (let ((fname (gensym)))
    `(let ((,fname ',tested-function))
       (setf (getf *deftests* ,fname)
	   (lambda ,args
	     (format t "~&Testing ~a: " ,fname)
	     ,@body
	     (format t "~a~%" t)
	     T))
       (run-test ,fname))))

;; Unit test unit test.
(deftest + ()
  (assert (= (+ 1 1) 2))
  (assert (= (+ 2 3) 5))
  (assert (not (= (+ 0 4) 5))))
