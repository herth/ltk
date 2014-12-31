;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage #:ltk-asd
  (:use :cl :asdf))

(in-package :ltk-asd)

(defsystem :rltk 
  :name "LTK"
  :version "0.8.0"
  :author "Peter Herth"
  :licence "LGPL"
  :description "LTK"
  :depends-on (:ltk-remote)
  :long-description "Lisp bindings for the Tk toolkit"
  :components ((:file "repl-ltk")
               ;(:file "ltk-remote" :depends-on ("repl-ltk"))
               (:file "ltk-mw" :depends-on ("repl-ltk")))
  )

