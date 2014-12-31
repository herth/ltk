;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage #:ltk-remote-asd
  (:use :cl :asdf))

(in-package :ltk-remote-asd)

(defsystem ltk-remote
  :name "LTK-REMOTE"
  :version "0.8.0"
  :author "Peter Herth"
  :licence "LGPL"
  :description "LTK remote"
  :long-description "Remote Lisp bindings for the Tk toolkit"
  :components ((:file "ltk-remote"))
  :depends-on ("ltk"
               #+sbcl "sb-bsd-sockets"
               ))

