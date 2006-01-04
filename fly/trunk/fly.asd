;; -*- lisp -*-

(defsystem :fly
  :depends-on (:ltk :ltk-mw)
  :components
  ((:file "defpackage")
   (:file "fly-debugger" :depends-on ("defpackage"))
   (:file "browser" :depends-on ("defpackage"))))
