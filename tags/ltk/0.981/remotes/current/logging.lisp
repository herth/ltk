(in-package :ltk-remote)

#-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This is SBCL-only at the moment."))

;;; code to connect to the logging server


(defvar *logging-port* 8888)

(defun connect-to-logserver (prefix &optional id)
  (multiple-value-bind (socket stream)
      (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
	(ignore-errors
	  (socket-connect socket #(127 0 0 1) *logging-port*)
	  (values socket
		  (socket-make-stream socket :input t :output t
				      :element-type 'character :external-format :latin1))))
    (when stream
      (cond
        (id (format stream "username: ~a/~a~%" id prefix))
        (t (format stream "username: ~a/~a/~a"
		   (sb-bsd-sockets:socket-peername socket)
		   (sb-posix:getpid)
		   prefix)))
      stream)))

(defun set-logging (out err)
  (setf *standard-output* out
	*error-output* err
	*debug-io* err
	;; *debugging-output* err
	))

(defmacro with-logging ((&key id out err) &rest code)
  (let ((out= (gensym))
        (err= (gensym))
	(id= (gensym))
	(closep (not (or out err))))
    `(let* ((,id= ,id)
	    (,out= (or ,out (connect-to-logserver "out" ,id=)))
	    (,err= (or ,err (connect-to-logserver "err" ,id=))))
       (cond
         ((and ,out= ,err=)
          (let ((*standard-output* ,out=)
                (*error-output* ,err=)
		(*debug-io* ,err=)
		;; (*debugging-output* ,err=)
		)
            (unwind-protect
                 (progn
                   ,@code)
              (progn
                (finish-output ,out=)
                (finish-output ,err=)
                ,@(when closep
		    (list `(close ,out=)
			  `(close ,err=)))))))
         (t
          (format t "Cannot connect to the logging server!~%")
          ,@code)))))

(defun logging-test ()
  (with-logging ()
      (format t "test~%")
    (format *error-output* "an error message ~%")
    (format t "test2~%") (error "bah!~%This is not working!")
    (format t "test3~%")
    (format *error-output* "errortest~%")
    ))
