#|
 Ltk-remote  networking support for the Ltk library

 This software is Copyright (c) 2003 Peter Herth <herth@peter-herth.de>

 Portions of this software are Copyright (C) 2006, Cadence Design Systems, GmbH

 The authors grant you the rights to distribute
 and use this software as governed by the terms
 of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html),
 known as the LLGPL.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
|#

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets)
  (require :sb-posix))

(defpackage "LTK-REMOTE"
  (:use "COMMON-LISP" "LTK"
	#+:cmu "EXT"
	#+:sbcl "SB-EXT"
        #+:sbcl "SB-SYS"
	#+:sbcl  "SB-BSD-SOCKETS")
  (:export "WITH-REMOTE-LTK"))

(in-package :ltk-remote)

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
                   (let ,bindings
                     (ltk::call-with-ltk (lambda ()
                                           ,form)
                                         :stream stream)
                     ,@cleanup))
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

;; lispworks version
(defvar *server* nil)
#+:lispworks
(defun stop-server ()
 (mp:process-kill ltk-remote::*server*))
#+:lispworks
(require "comm")
#+:lispworks
(defmacro with-remote-ltk (port bindings form &rest cleanup)
  `(setf ltk-remote::*server*
         (comm:start-up-server :function 
                               (lambda (handle)
                                 (let ((stream (make-instance 'comm:socket-stream
                                                              :socket handle
                                                              :direction :io
                                                              :element-type
                                                              'base-char)))
                                   (mp:process-run-function
                                    (format nil "ltk-remote ~D" handle)
                                    '()
                                    (lambda ()
                                      (let ,bindings
                                        (ltk::call-with-ltk (lambda ()
                                                              ,form)
                                                            :stream stream)
                                        ,@cleanup)))))
                               :service ,port)))

;;; forking sbcl version

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensyms ((&rest vars) &body body)
    "Evaluate BODY with each var in VARS bound to a (nicely named) gensym."
    `(let ,(loop for var in vars collect `(,var (gensym ,(string var))))
       ,@body)))

#+sbcl
(progn
  (defstruct (remote-server (:constructor make-remote-server
                                          (port socket handler cleanup)))
    port
    socket
    handler
    cleanup)
  
  (defvar *ltk-remote-servers* nil)
  
  (defvar *children* ())
  
  (defun stop-server (&optional (servers :all) (cleanup? t))
    (let ((servers (if (eql servers :all)
                       *ltk-remote-servers*
                       servers)))
      (dolist (server *ltk-remote-servers*)
        (socket-close (remote-server-socket server))
        (remove-fd-handler (remote-server-handler server))
        (when cleanup? (funcall (remote-server-cleanup server))))
      (setf *ltk-remote-servers* (set-difference *ltk-remote-servers* servers))
      servers))
  
  (defun active-children ()
    (setf *children*
          (remove-if-not (lambda (pid)
                           (ignore-errors
                             (zerop (sb-posix:waitpid pid sb-unix:wnohang))))
                         *children*
                         :key #'car)))
  
  (defvar *on-fork-hook* '(stop-serving-requests))
  
  (defun stop-serving-requests ()
    (stop-server :all nil)
    (setf *children* nil))
  
  (defun make-server (port interface backlog child-function cleanup)
    (labels ((server (server-socket)
               (lambda (fd)
                 (declare (ignore fd))
                 (multiple-value-bind (client peer) (socket-accept server-socket)
                   (if (> (length (active-children)) 500)
                       (socket-close client)
                       (let ((pid (sb-posix:fork)))
                         (if (zerop pid)
                             (progn (unwind-protect
                                         (ignore-errors
                                           (mapc #'funcall (reverse *on-fork-hook*))
                                           (funcall child-function client peer))
                                      (sb-ext:quit :recklessly-p t)))
                             (progn
                               (socket-close client)
                               (setf *children*
                                     (acons pid peer (active-children)))))))))))
      (let ((server (make-instance 'inet-socket :type :stream :protocol :tcp)))
        (setf (sockopt-reuse-address server) t)
        (socket-bind server interface port)
        (socket-listen server backlog)
        (let ((server (make-remote-server
                       port
                       server
                       (add-fd-handler (socket-file-descriptor server)
                                       :input
                                       (server server))
                       cleanup)))
          (push server *ltk-remote-servers*)
          server))))
  
  (defmacro with-remote-ltk ((&key (port (parse-integer "ltk" :radix 30))
                                   (interface #(0 0 0 0))
                                   (backlog 64)
                                   (ltk-keys ()))
                             (&rest bindings)
                             form
                             &rest cleanup)
    (with-gensyms (socket peer stream keys)
      `(let ((,keys ,ltk-keys))
        (make-server ,port ,interface ,backlog
                     (lambda (,socket ,peer)
                       (let ((,stream (socket-make-stream
                                       ,socket :input t :output t
                                       :external-format :iso-8859-1
                                       :buffering :line)))
                         (apply #'ltk:call-with-ltk
                                (lambda () (let ,bindings ,form))
                                :stream ,stream
                                ,keys)))
                     (lambda () ,@cleanup)))))
)
  
;; allegro version

#+:allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sock)
  (use-package :socket))

#+:allegro
(defmacro with-remote-ltk (port bindings form &rest cleanup)
  `(setf ltk-remote::*server*
         (mp:process-run-function
          (format nil "ltk remote server [~a]" ,port)
          (lambda ()
            (let ((server (make-socket :type :stream :address-family :internet :connect :passive
                                       :local-host "0.0.0.0" :local-port ,port
                                       :reuse-address t :keepalive t)))
              (restart-case 
                  (unwind-protect
                       (loop
                          (let ((connection (accept-connection server)))
                            (mp:process-run-function
                             (format nil "ltk remote connection <~s>"  (ipaddr-to-hostname
                                                                        (remote-host connection)))
                             (lambda ()
                               (let ,bindings
                                 (ltk:call-with-ltk (lambda ()
                                                      ,form)
                                                    :stream connection)
                                 ,@cleanup)))))
                    (close server))
                (quit ()
                  :report "Shutdown ltk remote server"
                  nil)))))))

;;; simple test function

;; Commented out to remove spurious compiler warning.
#+nil
(defun lrtest (port)
  (with-remote-ltk
   (:port port) ()
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
     (pack b :side "left")
     (pack b3 :side "left")
     (pack b2 :side "left")
     (pack f :side "top")
     (pack txt :side "bottom")
     )))


#+nil
(defun rlb-test2 ()
  (with-remote-ltk (:port 8080) ()
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

