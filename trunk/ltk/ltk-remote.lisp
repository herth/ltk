(defpackage "LTK-REMOTE"
  (:use "COMMON-LISP"
	"COMMON-LISP-USER"
	"LTK"
	"UNIX"
	"MULTIPROCESSING"
	)

  (:export "WITH-REMOTE-LTK"
	   
	   ))

;(in-package ltk-remote)
(in-package ltk)


       

;;; multiprocessing version

(defun ip-address-string (address)
  (format nil "~D.~D.~D.~D"
          (ldb (byte 8 24) address)
          (ldb (byte 8 16) address)
          (ldb (byte 8 8)  address)
          (ldb (byte 8 0)  address)))

(defvar *stop-remote* nil)



(defun ltk-remote-server (port)
  (format t "mark 1~&")(force-output)
  (setf *stop-remote* nil)
  (let ((fd (ext:create-inet-listener port)))
    (format t "mark 2~&")(force-output)
    (unless (> fd 0) (error "Cannot bind port ~D." port))
    (force-output)
    (loop
      (when *stop-remote*
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
;	  (close stream)
	  (multiprocessing::make-process
	   #'(lambda ()
	       (ltk-remote-serve2 stream)) 
	   :name (format nil "HTTP connection from ~A"
			 (if host-entry
			     (ext:host-entry-name host-entry)
			   (ip-address-string remote-host))))
	  )))
    (close fd)))


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

(defun start-mp ()
  (setf mp::*idle-process* mp::*initial-process*))

(defun start-remote (port)
  (multiprocessing::make-process #'(lambda () (ltk-remote-server port))))


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


(defun ltk-remote-serve (stream)
  "function running in a thread per connection to pick up players
    input and put it into buffer queue"
  (format t "ltk-remote-serve ~a ~&" stream)
  (force-output)
  (loop
    (multiple-value-bind (line cond)
	(ignore-errors
	  (string-trim '(#\Newline #\Linefeed #\Return)
		       (read-line stream)))
      (if line
	  (progn
	      (princ "read:")
	      (princ line)
	      (terpri)
	      (force-output)
	      ;(add-input (pc connection) line)
	      )
	  (progn
	    (format t "An error occured: ~A~%" cond)
	    (return-from ltk-remote-serve))))))



(defvar *angle* 0)
(defvar *angle2* 0)
(defvar *angle3* 0)


(defun rotate()
  (let ((*debug-tk* nil))
    (let ((lines nil)
	  (dx (* 50 (sin *angle2*)))
	  (dy (* 50 (cos *angle2*)))
	  (wx (sin *angle3*))
;	  (wy (cos *angle3*))
	  )
      (incf *angle* 0.1)
      (incf *angle2* 0.03)
      (incf *angle3* 0.01)
      
      (dotimes (i 100)
	(let ((w (+ *angle* (* i 2.8001))))
	  (let ((x (+ dx 250 (* 150 (sin w) wx)))
		(y (+ dy 200 (* 150 (cos w)))))
	    (push y lines)
	    (push x lines)
	    )))    
      (set-coords *demo-canvas* *demo-line* lines))
    (if *do-rotate*
	(after 25 #'rotate))))

(defun test-rotation()
  (setf *debug-tk* nil)
  (time (dotimes (i 100)
	  (rotate)))
  )
(defun start-rotation()
  (setf *debug-tk* nil)
  (setf *do-rotate* t)
  (rotate)
  )
(defun stop-rotation()
  (setf *debug-tk* t)
  (setf *do-rotate* nil)
  )

(defun ltk-remote-serve2 (stream)
  "function running in a thread per connection to pick up players
    input and put it into buffer queue"
  (format t "ltk-remote-serve ~a ~&" stream)
  (force-output)
  (let ((*w* stream)

	)
    (let* ((bar (make-frame nil))
	   (fr (make-frame bar))
	   (lr (make-label fr "Rotation:"))
	   (bstart (make-button fr "Start" 'start-rotation))
	   (bstop  (make-button fr "Stop"  'stop-rotation))
	   (b1 (make-button bar "Hallo" (lambda () (format T "Hallo~%"))))
	   (b2 (make-button bar "Welt!" (lambda () (format T "Welt~%"))))
	   (f (make-frame bar))
	   (l (make-label f "Test:"))
	   (b3 (make-button f "Ok." 'test-rotation))
	   (e (make-entry bar))
	   (b4 (make-button bar "get!" (lambda () (format T "content of entry:~A~%" (get-content e)))))
	   (b5 (make-button bar "set!" (lambda () (set-content e "test of set"))))
	   (sc (make-scrolled-canvas nil)); :width 500 :height 500))
	   (c (canvas sc))
	   (lines nil)
	   mb mfile mf-load mf-save mf-export mfe-jpg mfe-gif mf-exit mf-print
	   )
      (setf mb (make-menubar))
      (setf mfile (make-menu mb "File"))
      (setf mf-load (make-menubutton mfile "Load" (lambda () (format t "Load pressed~&"))))
      (setf mf-save (make-menubutton mfile "Save" (lambda () (format t "Save pressed~&"))))
      (add-separator mfile)
      (setf mf-export (make-menu mfile "Export..."))
      (add-separator mfile)
      (setf mf-print (make-menubutton mfile "Print" (lambda () (postscript c "wt.ps"))))
      (add-separator mfile)
      (setf mfe-jpg (make-menubutton mf-export "jpeg" (lambda () (format t "Jpeg pressed~&"))))
      (setf mfe-gif (make-menubutton mf-export "png" (lambda () (format t "Png pressed~&"))))
      (setf mf-exit (make-menubutton mfile "Exit" (lambda () (setf *exit-mainloop* t))))
      
      (configure c "borderwidth" "2")
      (configure c "relief" "sunken")
      (pack sc :side "top" :fill "both" :expand 1)
      (pack bar :side "bottom")
      (scrollregion c 0 0 500 400)
      (pack fr)
      (pack lr)
      (configure fr "borderwidth" "2")
      (configure fr "relief" "sunken")
      (pack bstart)
      (pack bstop)
      (pack b1)
      (pack b2)
      (configure f "borderwidth" "2")
      (configure f "relief" "sunken")
      (pack f :fill "x")
      (pack l)
      (pack b3)
      (pack e)
      (pack b4)
      (pack b5)
      (dotimes (i 100)
	(let ((w (* i 2.8001)))
	  (let ((x (+ 250 (* 150 (sin w))))
		(y (+ 200 (* 150 (cos w)))))
	    (push y lines)
	    (push x lines)
	    )))
      (setf *demo-line* (create-line c lines))
      (setf *demo-canvas* c)
      (create-text c 10 10 "Ltk Demonstration")
      )
    (mainloop)
    (close stream)))



;;; file handler version (select)

(defun do-inputs ()
  (sys:serve-all-events 0))

(defun do-output (fd s)
  "write string s to the file descriptor fd"
  (unix:unix-write fd (sys:vector-sap s) 0 (length s)))

;(defun accept-handler (fd)
;  (let ((from-fd (ext:accept-tcp-connection fd))
;	;(pc (make-instance 'pc))
;	)
;    ;(setf (pc-fd pc) from-fd)
;    ;(setf (handler pc) 'ask-name)
;;    (setf (pc-output pc) (make-string-output-stream))
;;    (push pc *pc-list*)

;;    (send pc "Hallo...")
;;    (setf last-socket from-fd)
;    (setf last-socket-handler
;	  (sys:add-fd-handler
;	   from-fd
;	   :input (lambda (from-fd)
;		    (declare (type integer from-fd))
;		    (multiple-value-bind (count err)
;			(unix-read from-fd (sys:vector-sap *read-buffer*) 100)
;		      ;(format t "read ~A bytes from ~A~%" count from-fd)
;		      (setf (pc-input pc) (concatenate 'string (pc-input pc)
;						       (subseq *read-buffer* 0 count)))
;		      (if (or (null count) (zerop count) *quit*)
;			  (progn
;			    (unless count
;			      (format *debug-io* "Error reading from file descriptor ~d: ~a"
;				      from-fd (GET-UNIX-ERROR-MSG err)))
;			    (format t "removing a pc ~%")
;			    ;(setf *pc-list* (remove pc *pc-list*))
;			    (unix-close from-fd)
;			    (sys:invalidate-descriptor from-fd))
;					;(unix-write (pc-fd pc) (sys:vector-sap def-msg) 0 24)
;			)))
;	   ))
;    ;(send pc (prompt pc))
;	))

;(defun start-listener()
;  (system:default-interrupt SIGPIPE)
;  (setf fd (ext:create-inet-listener  +port+))
;  (sys:add-fd-handler fd :input #'accept-handler))

;(defun stop-listener()
;  (sys:remove-fd-handler #'accept-handler)
;  (unix-close fd))

