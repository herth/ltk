;;; Copyright (c) 2005 Thomas F. Burdick.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :fly)

(declaim (function *inspector*))
(defvar *inspector* #'inspect)

(defvar *fly-ltk* (make-ltk-connection))
(defvar *application-ltk* nil)

(defun wish-okay? (wish)
  (values
   (ignore-errors
     (terpri (wish-stream wish))
     (finish-output (wish-stream wish))
     t)))

(defun fly-ltk ()
  (unless (wish-okay? *fly-ltk*)
    (setf (wish-stream *wish*) nil))
  *fly-ltk*)

(defvar *fly-toplevels* ())

(defclass fly-toplevel (toplevel) ())

(defmethod initialize-instance :after ((self fly-toplevel) &rest ignore)
  (declare (ignore ignore))
  (push self *fly-toplevels*))

(defmacro with-fly-ltk ((var title &key (debugger t)) &body body)
  `(let ((*application-ltk* *wish*)
         (*wish* (fly-ltk)))
     (unwind-protect
          (progn
            (unless (wish-stream *wish*)
              (start-wish :handle-errors nil :debugger ,debugger)
              (withdraw *tk*))
            (let ((,var (make-instance 'fly-toplevel)))
              (withdraw ,var)
              (wm-title ,var ,title)
              (prog1 (progn ,@body)
                (mainloop))))
       (setf *fly-toplevels* (map nil #'destroy *fly-toplevels*)))))
