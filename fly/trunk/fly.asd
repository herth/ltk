;; -*- lisp -*-

(defsystem :fly
  :depends-on (:ltk :ltk-mw)
  :components
  ((:file "defpackage")
   (:file "fly" :depends-on ("defpackage"))
   (:file "fly-debugger" :depends-on ("fly"))
   (:file "browser" :depends-on ("defpackage"))))
