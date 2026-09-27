;;;; Lisp bindings and convenience functions for strptime(3)
;;;; Standard definition:
;;;; https://pubs.opengroup.org/onlinepubs/9699919799/functions/strptime.html

(eval-when (:load-toplevel :compile-toplevel :execute)
  (mapcar #'require '(:eric-test
		      :local-time)))

(defpackage :strptime
  (:use :cl :sb-alien)
  (:export :strptime))

(in-package :strptime)

(pushnew :strptime eric-test:*silent-packages*)

(define-alien-type nil
  (struct tm
    (tm-sec int)
    (tm-min int)
    (tm-hour int)
    (tm-mday int)
    (tm-mon int)
    (tm-year int)
    (tm-wday int)
    (tm-yday int)
    (tm-isdst int)
    (tm-gmtoff long)
    (tm-zone c-string)))

(define-alien-routine ("strptime" %strptime) (* char)
  (buf c-string)
  (format c-string)
  (tm (* (struct tm))))

(defun strptime (date-string format-string)
  (with-alien ((time-struct (struct tm)))
    (setf (slot time-struct 'tm-sec) 0
          (slot time-struct 'tm-min) 0
          (slot time-struct 'tm-hour) 0
          (slot time-struct 'tm-mday) 1
          (slot time-struct 'tm-mon) 0
          (slot time-struct 'tm-year) 0
          (slot time-struct 'tm-isdst) -1)
    (let ((result-ptr (%strptime date-string format-string (addr time-struct))))
      (if (null-alien result-ptr)
	  nil
	  (local-time:encode-timestamp
	   0 ;; nsec
	   (slot time-struct 'tm-sec)
	   (slot time-struct 'tm-min)
	   (slot time-struct 'tm-hour)
           (slot time-struct 'tm-mday)
           (1+ (slot time-struct 'tm-mon)) ; strptime(3) months are 0-11
           (+ 1900 (slot time-struct 'tm-year))))))) ; strptime(3) years are since 1900

(eric-test:deftest strptime ()
  (assert (local-time:timestamp=
	   (strptime "2026-09-27 15:42:30" "%Y-%m-%d %H:%M:%S")
	   (local-time:encode-timestamp
	    0
	    30
	    42
	    15
	    27
	    9
	    2026)))
  (assert (null (strptime "Hello" "%Y-%m-%d %H:%M:%S"))))

