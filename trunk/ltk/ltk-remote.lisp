(in-package ltk)


       

;;; system dependant part

#+:cmu
(defun ip-address-string (address)
  (format nil "~D.~D.~D.~D"
          (ldb (byte 8 24) address)
          (ldb (byte 8 16) address)
          (ldb (byte 8 8)  address)
          (ldb (byte 8 0)  address)))


(defvar *stop-remote* nil)


#+:cmu
(defmacro with-remote-ltk (port &rest body)
  `(multiprocessing::make-process
    (lambda () 
      (format t "mark 1~&")(force-output)
      (setf *stop-remote* nil)
	(let ((fd (ext:create-inet-listener ,port)))
	  (format t "mark 2~&")(force-output)
	  (format t "fd: ~a ~&" fd) (force-output)
	  (unless (> fd 0) (error "Cannot bind port ~D." ,port))
	  (force-output)
	  (loop
	    (when *stop-remote*
	      (close fd)
	      (return))
	    ;; Wait for new connection
	    (multiprocessing::process-wait-until-fd-usable fd :input)
	    (format t "~&; At ~D Got Connection...~%" (get-internal-real-time))
	    (force-output)
	    (multiple-value-bind (new-fd remote-host) (ext:accept-tcp-connection fd)
	      (format t "~&; At ~D Have Connection...~%" (get-internal-real-time))
	      (let* ((host-entry (ext:lookup-host-entry remote-host))
		     (stream (sys:make-fd-stream new-fd :input t :output t)))
		(format t "Connection from ~A" (if host-entry
						   (ext:host-entry-name host-entry)
						 (ip-address-string remote-host)))
		(format t "~&; At ~D Established Connection.~%" (get-internal-real-time))
					;(format stream "Hallo Du!~&")
					;(force-output stream)
					;(close stream)
		(multiprocessing::make-process
		 (lambda ()
		   (let ((*w* stream))
		     ,@body
		     (mainloop)
		     (format t "closing connection~&")
		     (force-output)
		     (close stream)
		     ))
		 :name (format nil "LTK connection from ~A"
			       (if host-entry
				   (ext:host-entry-name host-entry)
				 (ip-address-string remote-host))))
		)))
	  (close fd)))))


(defun stop-server ()
  (setf *stop-remote* t))

#+:cmu
(defun start-mp ()
  (setf mp::*idle-process* mp::*initial-process*))

#+:cmu
(defun start-remote (port)
  (multiprocessing::make-process #'(lambda () (ltk-remote-server port))))



#+:sbcl
(use-package :sb-thread)
#+:sbcl
(require :sb-bsd-sockets)
#+:sbcl
(use-package :sb-bsd-sockets)

#+:sbcl
(defun make-socket-server (port)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-bind socket #(0 0 0 0) port)
    (socket-listen socket 100)
    socket))

#+:sbcl
(defun get-connection-stream (server-socket)
  (let* ((s (socket-accept server-socket))
	     (stream (socket-make-stream s :input t :output t)))
    stream)) ;; do we need to return s as well ?

#+:sbcl
(defmacro with-remote-ltk (port &rest body)
  `(make-thread
    (lambda () 
      (setf *stop-remote* nil)      
      (let ((socket (make-socket-server ,port)))
	(loop
	  (when *stop-remote*
	    (socket-close socket)
	    (return))
	  (let* ((s (socket-accept socket))
		 (stream (socket-make-stream s :input t :output t)))
	    (make-thread (lambda ()
			   (let ((*w* stream))
			     ,@body
			     (mainloop)
			     
			     (force-output)
			     (close stream)
			     (socket-close s)
			     )))))			  
	  (socket-close socket)))))


(defun b-callback (txt)
  (append-text txt (format nil "Halloele~%")))

(defun lrt2setup ()
  (let* ((txt (make-text nil :width 40 :height 10))
	 (f (make-frame nil))
	 (b (make-button f "Hallo" (lambda ()
				     (b-callback txt ))))
	 (b2 (make-button f "Quit" (lambda ()
				     (setf *exit-mainloop* t))))
	 (b3 (make-button f "Clear" (lambda ()
				       (clear-text txt ))))
	  )
    (pack b :side "left")
    (pack b3 :side "left")
    (pack b2 :side "left")
    (pack f :side "top")
    (pack txt :side "bottom")
    )
  )

(defun lrt2 (port)
  (with-remote-ltk port
		   (lrt2setup)))

(defun lrtest (port)
  (with-remote-ltk
   port
   (let* ((txt (make-text nil :width 40 :height 10))
	  (f (make-frame nil))
	  (b (make-button f "Hallo" (lambda ()
					(append-text txt (format nil "Hallo pressed~&")))))
	  (b2 (make-button f "Quit" (lambda ()
					(setf *exit-mainloop* t))))
	  (b3 (make-button f "Clear" (lambda ()
				       (clear-text txt ))))
	  )
     (pack b :side "left")
     (pack b3 :side "left")
     (pack b2 :side "left")
     (pack f :side "top")
     (pack txt :side "bottom")
     )))


