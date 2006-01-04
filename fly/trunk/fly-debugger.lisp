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

(defvar *fly-toplevels* ())

(defclass fly-toplevel (toplevel) ())

(defmethod initialize-instance :after ((self fly-toplevel) &rest ignore)
  (declare (ignore ignore))
  (push self *fly-toplevels*))

(defmacro with-fly-ltk ((var title) &body body)
  `(let ((*wish* *fly-ltk*))
     (unwind-protect
          (progn
            (unless (wish-stream *wish*)
              (start-wish :handle-errors t)
              (withdraw *tk*))
            (let ((,var (make-instance 'fly-toplevel)))
              (withdraw ,var)
              (wm-title ,var ,title)
              ,@body)
            (mainloop))
       (mapc #'withdraw *fly-toplevels*))))

(defun debugger (condition debugger)
  (declare (ignore debugger))
  (with-fly-ltk (tl "A Fly Debugger")
    (let ((frame (make-instance 'frame :master tl)))
      (pack (display-condition condition frame))
      (pack (display-stack frame))
      (pack (display-restarts condition frame))
      (pack frame)
      (normalize tl))))

(defun display-condition (condition master)
  (list (make-instance 'label :master master :text (princ-to-string condition))
	(make-instance 'button
	  :master master :text (format nil "[Condition of type ~S]"
				       (class-name (class-of condition)))
	  :command (lambda () (funcall *inspector* condition)))))

(defun display-stack (master)
  (make-instance 'treelist
    :master master :depth 1
    :data (sb-debug:backtrace-as-list)))

(defun display-restarts (condition master)
  (let* ((applicable (compute-restarts condition))
	 (others (compute-restarts))
	 (not-applicable (set-difference others applicable))
	 (all (loop with all = (nreverse applicable)
		    for r in all
		    do (pushnew r all)
		    finally (return (nreverse all)))))
    (loop with frame = (make-instance 'frame :master master)
	  for r in all
	  for fun = (lambda () (invoke-restart r))
	  for (class . args) = (if (member r not-applicable)
				 '(label)
				 `(button :command ,fun))
	  for this = (make-instance 'frame :master frame)
	  for name = (apply #'make-instance class
			    :master this :text (string (restart-name r))
			    args)
	  for desc = (make-instance 'label
		       :master this :text (princ-to-string r))
	  do (pack this :anchor :w)
	     (pack (list name desc) :side :left)
	  finally (return frame))))
