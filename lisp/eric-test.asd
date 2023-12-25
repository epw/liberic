;;;; -*- Mode: Lisp -*-

(defpackage #:eric-test-asd
  (:use :cl :asdf))

(in-package :eric-test-asd)

(defsystem eric-test
  :name "Eric Test"
  :version "1.0"
  :maintainer "Eric Willisson"
  :author "Eric Willisson"
  :description "Simple unit testing utility"
  :components ((:file "eric-test")))

