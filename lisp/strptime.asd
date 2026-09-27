;;;; -*- Mode: Lisp -*-

(defpackage #:strptime-asd
  (:use :cl :asdf))

(in-package :strptime-asd)

(defsystem strptime
  :name "strptime"
  :version "0.1"
  :maintainer "Eric Willisson"
  :author "Eric Willisson"
  :description "Lisp bindings for strptime(3) and LOCAL-TIME wrapper."
  :components ((:file "strptime"))
  :depends-on (eric-test local-time))
