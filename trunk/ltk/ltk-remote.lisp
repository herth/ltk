#|
 Ltk-remote  networking support for the Ltk library

 This software is Copyright (c) 2003 Peter Herth <herth@peter-herth.de>

 Peter Herth grants you the rights to distribute
 and use this software as governed by the terms
 of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html),
 known as the LLGPL.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
|#

#+:sbcl (require :sb-bsd-sockets)

(defpackage "LTK-REMOTE"
  (:use "COMMON-LISP" "LTK"
	#+:cmu "EXT"
	#+:sbcl "SB-EXT"
	#+:sbcl "SB-THREAD"
	#+:sbcl  "SB-BSD-SOCKETS")
	
  (:export
   "WITH-REMOTE-LTK"
   ))


(in-package ltk-remote)


;;; cmu version

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
		   (let ((ltk::*wish* stream)
			 (*callbacks* (make-hash-table :test #'equal))
			 (*counter* 1))
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

#+:cmu
(defun stop-server ()
  (setf *stop-remote* t))

#+:cmu
(defun start-mp ()
  (setf mp::*idle-process* mp::*initial-process*))

#+:cmu
(defun start-remote (port)
  (multiprocessing::make-process #'(lambda () (ltk-remote-server port))))


;;; sbcl version

#+:sbcl
(defun stop-server ()
  (setf *stop-remote* t))


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
			   (let ((ltk::*wish* stream)
				 (*callbacks* (make-hash-table :test #'equal))
				 (*counter* 1))
			     ,@body
			     (mainloop)
			     
			     (force-output)
			     (close stream)
			     (socket-close s)
			     )))))			  
	  (socket-close socket)))))


;; lispworks version

(defvar *server* nil)
#+:lispworks
(defun stop-server ()
 (mp:process-kill ltk::*server*))

#+:lispworks
(require "comm")

#+:lispworks
(defmacro with-remote-ltk (port &rest body)
  `(setf *server*
         (comm:start-up-server :function 
                               (lambda (handle)
                                 (let ((stream (make-instance 'comm:socket-stream
                                                              :socket handle
                                                              :direction :io
                                                              :element-type
                                                              'base-char)))
                                   (mp:process-run-function (format nil "ltk-remove ~D"
                                                                    handle)
                                                            '()
                                                            (lambda ()
                                                             (let ((ltk::*wish* stream)
								   (*callbacks* (make-hash-table :test #'equal))
								   (*counter* 1))
                                                               ,@body
                                                               (mainloop)
                                                               (close stream)
                                                               
                                                               )))))
                               :service ,port)))



;;; simple test function

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


(defun rlb-test2 ()
  (with-remote-ltk 8080
   (let* ((last nil)
	  (l (make-instance 'listbox))
	  (wf (make-instance 'frame))
	  (lbl (make-instance 'label :master wf :text "Widget:"))
	  (f (make-instance 'frame :master wf))
	  (canv (make-instance 'canvas :master f :width 100 :height 100))
	  (scanv (make-instance 'scrolled-canvas :master f))
	  (widgets (list
		    (make-instance 'button :master f :text "Button")
		    (make-instance 'label :master f :text "Label")
		    canv
		    scanv
		    ))
	;  (b (make-instance 'button :text "Show" :command ))
	  )
     (bind l "<Button-1>" (lambda ()
			    (let ((sel (listbox-get-selection l)))
			      (format t "selection: ~a~%" sel)
			      (force-output)
			      (if (first sel)
				  (let ((w (nth (first (listbox-get-selection l)) widgets)))
				    (when last
				      (pack-forget last))
				    (pack w)
				    (setf last w))))))
     (pack l :expand 1 :fill "y")
     (pack wf :expand 1 :fill "both")
     ;(grid l 0 0)
     ;(grid wf 0 1)

     (pack lbl :side "top")
     (pack f :expand 1 :fill "both")
     (configure wf "borderwidth" 2)
     (configure wf "relief" "sunken")
     
     ;(pack b)
     (create-line canv (list 0 0 40 40 60 20 80 80 60 60 40 80 20 60 0 80 0 0))
     (create-line (canvas scanv) (mapcar (lambda (x)
					   (* x 10))
					 (list 0 0 40 40 60 20 80 80 60 60 40 80 20 60 0 80 0 0)))
     (scrollregion (canvas scanv) 0 0 800 800)
     (listbox-append l (mapcar (lambda (x) (type-of x)) widgets))

     )))

