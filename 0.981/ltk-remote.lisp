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

#+:sbcl (require 'sb-bsd-sockets)

(defpackage :ltk-remote
  (:use :common-lisp :ltk
        #+(or :cmu :scl) :ext
       #+:sbcl :sb-ext
       #+:sbcl :sb-thread
       #+:sbcl :sb-bsd-sockets)
  (:export
   #:stop-server
   #:with-remote-ltk))

(in-package ltk-remote)

(defmacro with-remote-ltk (port bindings form &rest cleanups)
  (let ((vars (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
        (vals (mapcar (lambda (b) (when (consp b) (cadr b))) bindings)))
    `(invoke-remote-ltk ,port 
                        ',vars
                        (list ,@vals)
                        (lambda () ,form) 
                        (lambda () ,@cleanups))))

;;; IMPLEMENTATIONS OF INVOKE-REMOTE-LTK

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
(defun invoke-remote-ltk (port vars vals form cleanup)
  (mp:make-process
   (lambda ()
     (setf *stop-remote* nil)
     (let ((fd (ext:create-inet-listener port :stream :reuse-address t)))
       (unwind-protect
           (loop
            (when (or *stop-remote* mp::*quitting-lisp*)
              (return))
            (let ((winp (mp:process-wait-until-fd-usable fd :input 2)))
              (when (or *stop-remote* mp::*quitting-lisp*)
                (return))
              (when winp
                (let ((new-fd (ignore-errors (ext:accept-tcp-connection fd))))
                  (when new-fd
                    (mp:make-process
                     (lambda ()
                       (multiple-value-bind (server-address server-port)
                           (ext:get-socket-host-and-port new-fd)
                         (multiple-value-bind (remote-address remote-port)
                             (ext:get-peer-host-and-port new-fd)
                           (flet ((host-name (address)
                                    (let ((host-entry (ext:lookup-host-entry address)))
                                      (if host-entry
                                          (ext:host-entry-name host-entry)
                                        (ip-address-string address)))))
                             (let ((stream (sys:make-fd-stream new-fd :input t :output t))
                                   (server-name (host-name server-address))
                                   (remote-name (host-name remote-address)))
                               (format t "Connection to ~A:~D from ~A:~D at "
                                       server-name server-port
                                       remote-name remote-port)
                               (ext:format-universal-time t (get-universal-time)
                                                          :style :rfc1123)
                               (setf (mp:process-name mp:*current-process*)
                                     (format nil "LTK connection to ~A:~D from ~A:~D"
                                             server-name server-port
                                             remote-name remote-port))
                               (progv vars vals
                                 (ltk::call-with-ltk form
                                                     :stream stream
                                                     :remotep t)
                                 (when cleanup
                                   (funcall cleanup))))))))))))))
         (unix:unix-close fd))))
   :name (format nil "LTK connection listener on port ~D" port)))

#+:cmu
(defun stop-server ()
  (setf *stop-remote* t))

#+:cmu
(defun start-mp ()
   #+nil (setf mp::*idle-process* mp::*initial-process*)
   (mp::startup-idle-and-top-level-loops))


#+:cmu
(defun start-remote (port)
  (multiprocessing::make-process #'(lambda () (ltk-remote-server port))))

;;; SCL version

#+:scl
(defun invoke-remote-ltk (port vars vals form cleanup)
  `(thread:thread-create
    (lambda ()
      (setf *stop-remote* nil)
      (let ((fd (ext:create-inet-listener port :stream :reuse-address t)))
        (unwind-protect
             (loop
                (when (or *stop-remote* thread:*quitting-lisp*)
                  (return))
                (let ((winp (sys:wait-until-fd-usable fd :input 2)))
                  (when (or *stop-remote* thread:*quitting-lisp*)
                    (return))
                  (when winp
                    (let ((new-fd (ignore-errors (ext:accept-tcp-connection fd))))
                      (when new-fd
                        (thread:thread-create
                         (lambda ()
                           (multiple-value-bind (server-address server-port)
                               (ext:get-socket-host-and-port new-fd)
                             (multiple-value-bind (remote-address remote-port)
                                 (ext:get-peer-host-and-port new-fd)
                               (flet ((host-name (address)
                                        (let ((host-entry (ext:lookup-host-entry address)))
                                          (if host-entry
                                              (ext:host-entry-name host-entry)
                                              (ext:ip-address-string address)))))
                                 (let ((stream (sys:make-fd-stream new-fd :input t :output t))
                                       (server-name (host-name server-address))
                                       (remote-name (host-name remote-address)))
                                   (format t "Connection to ~A:~D from ~A:~D at "
                                           server-name server-port
                                           remote-name remote-port)
                                   (ext:format-universal-time t (get-universal-time)
                                                              :style :rfc1123)
                                   (setf (thread:thread-name thread:*thread*)
                                         (format nil "LTK connection to ~A:~D from ~A:~D"
                                                 server-name server-port
                                                 remote-name remote-port))
                                   (progv vars vals
                                     (ltk::call-with-ltk form
                                                         :stream stream
                                                         :remotep t)
                                     (when cleanup
                                       (funcall cleanup))))))))))))))
          (unix:unix-close fd))))
    :name (format nil "LTK connection listener on port ~D" port)))

#+:scl
(defun stop-server ()
  (setf *stop-remote* t))


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
(defun invoke-remote-ltk (port vars vals form cleanup)
  (make-thread
   (lambda () 
     (setf *stop-remote* nil)      
     (let ((socket (make-socket-server port)))
       (loop
        (when *stop-remote*
          (socket-close socket)
          (return))
        (let* ((s (socket-accept socket))
               (stream (socket-make-stream s :input t :output t)))
          (make-thread
           (lambda ()
             (progv vars vals
               (ltk::call-with-ltk form
                                   :stream stream
                                   :remotep t)
               (when cleanup
                 (funcall cleanup)))))))
       (socket-close socket)))))


;; lispworks version

#+:lispworks
(require "comm")

#+:lispworks
(defvar *server* nil)

#+:lispworks
(defun stop-server ()
  (when *server*
    (mp:process-kill *server*)))


#+:lispworks
(defun invoke-remote-ltk (port vars vals form cleanup)
  (setf *server*
        (comm:start-up-server :function (lambda (handle)
                                          (handle-client-connection handle vars vals form cleanup))
                              :service port)))

#+:lispworks
(defun handle-client-connection (handle vars vals form cleanup)
  (let ((stream (make-instance 'comm:socket-stream
                               :socket handle
                               :direction :io
                               :element-type 'base-char)))
    (mp:process-run-function (format nil "ltk-remote ~d" handle)
                             nil
                             'thread-invoke-ltk
                             stream vars vals form cleanup)))

#+:lispworks
(defun thread-invoke-ltk (stream vars vals form cleanup)
  (catch 'exit-with-remote-ltk
    (progv vars vals
      (call-with-ltk form
                     :stream stream
                     :remotep t)
      (when cleanup
        (funcall cleanup)))))


;; allegro version

#+:allegro
(progn
  (require :sock)
  (use-package :socket))

#+:allegro
(defvar *server* nil)

#+:allegro
(defun stop-server ()
  (setf *stop-remote* t))


#+:allegro
(defun invoke-remote-ltk (port vars vals form cleanup)
  (setf *server*
        (mp:process-run-function
         (format nil "ltk remote server [~a]" port)
         (lambda ()
           (let ((server (make-socket :type :stream :address-family :internet :connect :passive
                                      :local-host "0.0.0.0" :local-port port
                                      :reuse-address t :keepalive t)))
             (restart-case 
                 (unwind-protect
                     (loop
                      (let ((connection (accept-connection server)))
                        (mp:process-run-function
                         (format nil "ltk remote connection <~s>"  (ipaddr-to-hostname
                                                                    (remote-host connection)))
                         (lambda ()
                           (progv vars vals
                             (ltk::call-with-ltk form
                                                 :stream connection
                                                 :remotep t)
                             (when cleanup
                               (funcall cleanup)))))))
                   (close server))
               (quit ()
                 :report "Shutdown ltk remote server"
                 nil)))))))

;;;;;;;;;;;;;;;;; END OF IMPLEMENTATION DEPENDANCIES


;;; simple test function

(defun lrtest (port)
  (with-remote-ltk
   port ()
   (let* ((txt (make-text nil :width 40 :height 10))
 	  (f (make-instance 'frame ))
 	  (b (make-instance 'button :master f :text  "Hallo"
 			    :command (lambda ()
					(append-text txt (format nil "Hallo pressed~&")))))
 	  (b2 (make-instance 'button :master f :text "Quit"
 			     :command (lambda ()
					(setf *exit-mainloop* t))))
 	  (b3 (make-instance 'button :master f :text "Clear"
 			     :command (lambda ()
				       (clear-text txt ))))
	  )
     (pack b :side :left)
     (pack b3 :side :left)
     (pack b2 :side :left)
     (pack f :side :top)
     (pack txt :side :bottom)
     )))


(defun rlb-test2 ()
  (with-remote-ltk 19790 ()
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
     (bind l "<Button-1>" (lambda (event)
                            (declare (ignore event))
			    (let ((sel (listbox-get-selection l)))
			      (format t "selection: ~a~%" sel)
			      (force-output)
			      (if (first sel)
				  (let ((w (nth (first (listbox-get-selection l)) widgets)))
				    (when last
				      (pack-forget last))
				    (pack w)
				    (setf last w))))))
     (pack l :expand 1 :fill :y)
     (pack wf :expand 1 :fill :both)
     ;(grid l 0 0)
     ;(grid wf 0 1)

     (pack lbl :side "top")
     (pack f :expand 1 :fill :both)
     (configure wf :borderwidth 2)
     (configure wf :relief :sunken)
     
     ;(pack b)
     (create-line canv (list 0 0 40 40 60 20 80 80 60 60 40 80 20 60 0 80 0 0))
     (create-line (canvas scanv) (mapcar (lambda (x)
					   (* x 10))
					 (list 0 0 40 40 60 20 80 80 60 60 40 80 20 60 0 80 0 0)))
     (scrollregion (canvas scanv) 0 0 800 800)
     (listbox-append l (mapcar (lambda (x) (type-of x)) widgets))

     )))

;;;

(pushnew :ltk-remote *features*)
