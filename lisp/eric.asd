;;;; -*- Mode: Lisp -*-

;; Copyright (C) Eric Willisson 2011
;; This library uses the GPL; see http://www.gnu.org/copyleft/gpl.html
;; for details

(defpackage #:eric-asd
  (:use :cl :asdf))

(in-package :eric-asd)

(defsystem eric
  :name "Eric"
  :version "0.1"
  :maintainer "Eric Willisson"
  :author "Eric Willisson"
  :description "Library of useful functions"
  :components ((:file "eric"))
  :depends-on (cl-ppcre usocket))
