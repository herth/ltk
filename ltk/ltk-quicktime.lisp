;;; quicktime specific extension to ltk
;;; requires the QuickTimeTcl tk extension installed (the full distribution
;;; of TkAqua includes it)

(defpackage :ltk-quicktime
  (:use :common-lisp :ltk)
  (:export
   #:quicktime
   #:play-movie
   #:stop-movie))

(in-package :ltk-quicktime)

(eval-when (:load-toplevel)
  (setf *init-wish-hook* (append *init-wish-hook*
				 (list (lambda ()
				   (send-wish "package require QuickTimeTcl"))
				       ))))

(defclass quicktime (widget)
  (
   ))

(defmethod initialize-instance :after ((m quicktime) &key file url width height resizable)
  (format-wish "movie ~a~@[ -file {~a}~]~@[ -url {~a}~]~@[ -width ~a~]~
                ~@[ -height ~a~]~@[~* -resizable 1~]"
	       (widget-path m) file url width height resizable))

(defun play-movie (quicktime)
  (format-wish "~a play" (widget-path quicktime)))

(defun stop-movie (quicktime)
  (format-wish "~a stop" (widget-path quicktime)))


#|
package require QuickTimeTcl
movie .m -file U137.mov
pack .m

package require QuickTimeTcl
movie .m -url "http://www.apple.com/bbc.mov"
pack .m
|#