;; 
;;     +---_-----------+   Copyright (c) 2007
;;     | c a d e n c e |   Cadence Design Systems                           
;;     +---------------+   VCAD Services Europe.                            
;;                         All Rights Reserved                              
;; 
;; THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF CADENCE DESIGN SYSTEMS 
;; The copyright notice above does not evidence any actual or intended   
;; publication of such source code.                                      
;; 
;; This code is the Intellectual Property of Cadence and a license has been
;; acquired through purchase from Cadence.                           
;; Unless otherwise contractually defined, the Customer may use this IP code 
;; exclusively for the target design environment for which it was acquired
;; from Cadence. The code may be modified for maintenance purposes due   
;; to changes in the target design environment.                          
;; 
;; This code remains Cadence intellectual property and may not 
;; be given to third parties, neither in original nor in modified
;; versions, without explicit written permission from Cadence.
;; 

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

#-(or allegro sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This file has only been ported to Allegro and SBCL"))

;;; We use a relatively sophisticated forking protocol, explained below:
;;;
;;;; 1. FORK allows you to specify input, output and error FDs for the
;;;; child, defaulting to /dev/null (see FORK's docstring for details).
;;;; These are opened if needed in the parent process.
;;;;
;;;; 2. The process is actually forked.
;;;;
;;;; Parent:
;;;; 
;;;; 3a. The parent process closes the input, output, and error FDs.
;;;;
;;;; 4a. The parent process calls *after-fork-parent-hook* in all its threads.
;;;;
;;;; 5a. FORK returns the child PID in the parent process.
;;;;
;;;; Child:
;;;;
;;;; 3b. The child process dup2's the input, output, and error FDs to 0, 1, and 2.
;;;;
;;;; 4b. The child process uses *survive-fork-p* to determine which
;;;; threads to kill, with the current thread always surviving.
;;;;
;;;; 5b. The child process calls *after-fork-child-hook* in all its surviving threads.
;;;;
;;;; 6b. FORK returns 0 in the child  process.
;;;
;;; The reason for the complexity is to provide a centralized
;;; mechanism of hooks that should be sufficient for controlling which
;;; threads will be running in forked processes, and to allow us to
;;; easily write library-specific code to clean up resources (ie, to
;;; prevent a confusing situation where two gradually diverging lisp
;;; processes are talking to the same slime on the same socket).
;;;
;;; All forking-related variables are bound in new threads to their
;;; default values.

(defvar *survive-fork-p* nil
  "A functional semipredicate to determine if this thread will continue in the child of a fork.")

(defvar *after-fork-parent-hook* ())
(defvar *after-fork-child-hook* ())

#+allegro
(progn
  (pushnew (cons '*survive-fork-p* nil) excl:*cl-default-special-bindings* :key #'car)
  (pushnew (cons '*after-fork-parent-hook* nil) excl:*cl-default-special-bindings* :key #'car)
  (pushnew (cons '*after-fork-child-hook* nil) excl:*cl-default-special-bindings* :key #'car))

(defun call-hook (hook)
  (if (listp hook)
      (mapc #'funcall hook)
      (funcall hook))
  nil)

(defun eval-semipred (semi)
  "Evaluate a functional semipredicate.  A functional semipredicate
can be NIL to indicate false, any non-function true value to indicate
true, or a function which will be called with no arguments, which will
evaluate to a functional semipredicate."
  (cond
    ((null semi) nil)
    ((functionp semi) (eval-semipred (funcall semi)))
    (t t)))

#+allegro
(defun terrorize-threads ()
  (dolist (proc (remove mp:*current-process* mp:*all-processes*))
    (ignore-errors
      (if (eval-semipred (mp:symeval-in-process '*survive-fork-p* proc))
          (mp:process-interrupt proc (lambda () (call-hook *after-fork-child-hook*)))
          (mp:process-kill proc)))))

#+sbcl
(defun terrorize-threads ()
  nil)

#+allegro
(progn
  #+sun
  (progn
    (ff:def-foreign-call (posix-open "open") ((path (* :char)) (oflag :int) (mode :unsigned-int))
      :strings-convert t)
    (ff:def-foreign-call (posix-dup2 "dup2") (source-fd dest-fd))
    (ff:def-foreign-call (posix-close "close") (fd))
    ;; From grovel.c
    (defconstant +o-rdonly+ 0)
    (defconstant +o-wronly+ 1)
    (defconstant +o-rdwr+ 2)
    (defconstant +o-append+ 8)
    (defconstant +o-creat+ 256))

  #+linux
  (progn
    (ff:def-foreign-call (posix-open "open") ((path (* :char)) (oflag :int) (mode :unsigned-int))
      :strings-convert t)
    (ff:def-foreign-call (posix-dup2 "dup2") (source-fd dest-fd))
    (ff:def-foreign-call (posix-close "close") (fd))
    (defconstant +o-rdonly+ 0)
    (defconstant +o-wronly+ 1)
    (defconstant +o-rdwr+ 2)
    (defconstant +o-append+ 1024)
    (defconstant +o-creat+ 64)))

#+sbcl
(progn
  (declaim (inline posix-open posix-dup2 posix-close))
  (defun posix-open (path oflag mode)
    (sb-posix:open path oflag mode))
  (defun posix-dup2 (source-fd dest-fd)
    (sb-posix:dup2 source-fd dest-fd))
  (defun posix-close (fd)
    (sb-posix:close fd))
  (defconstant +o-rdonly+ sb-posix:o-rdonly)
  (defconstant +o-wronly+ sb-posix:o-wronly)
  (defconstant +o-rdwr+ sb-posix:o-rdwr)
  (defconstant +o-append+ sb-posix:o-append)
  (defconstant +o-creat+ sb-posix:o-creat))

#+(and allegro (not (or sun linux)))
(error "FFI grovelling still needs to be done for this platform")

;;; returns 0 for the child process, the childs pid in the parent process
;;; 
(defun fork (&key (input "/dev/null") (output "/dev/null") (error "/dev/null"))
  (labels ((as-fd (spec direction)
             (if (typep spec '(integer 0))
                 spec
                 (posix-open spec
                             (ecase direction
                               (:input +o-rdonly+)
                               (:output (logior +o-creat+ +o-wronly+ +o-append+)))
                             0)))
           (child (in out err)
             (posix-dup2 in 0)
             (posix-dup2 out 1)
             (posix-dup2 err 2)
             (terrorize-threads))
           (parent (in out err)
             (posix-close in)
             (posix-close out)
             (posix-close err)
	     #+allegro
             (dolist (proc (remove mp:*current-process* mp:*all-processes*))
               (mp:process-interrupt proc (lambda () (call-hook *after-fork-parent-hook*))))
             (call-hook *after-fork-parent-hook*))
	   (really-fork ()
	     #+allegro (excl.osi:fork)
	     #+sbcl (sb-posix:fork)))
    (let ((in (as-fd input :input))
          (out (as-fd output :output))
          (err (as-fd error :output)))
      (let ((child (really-fork)))
        (if (zerop child)
            (child in out err)
            (parent in out err))
        child))))



(defun disconnect-slime ()
  (when (find-package "SWANK")
    (let ((close (find-symbol "CLOSE-CONNECTION" "SWANK"))
	  (connections (find-symbol "*CONNECTIONS*" "SWANK")))
      (mapc (symbol-function close) (symbol-value connections)))))

(pushnew 'disconnect-slime *after-fork-child-hook*)
