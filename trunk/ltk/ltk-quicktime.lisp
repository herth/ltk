;;; quicktime specific extension to ltk

(defpackage "LTK-QUICKTIME"
  (:use "COMMON-LISP"
	"LTK"
	)
  (:export
   "MOVIE"
   "PLAY-MOVIE"
   "STOP-MOVIE"
   ))

(in-package ltk-quicktime)

(eval-when (:load-toplevel)
  (setf *init-wish-hook* (append *init-wish-hook*
				 (list (lambda ()
				   (send-wish "package require QuickTimeTcl"))
				       ))))

(defclass movie (widget)
  (
   ))

(defmethod initialize-instance :after ((m movie) &key file url width height resizable)
  (format-wish "movie ~a~@[ -file {~a}~]~@[ -url {~a}~]~@[ -width ~a~]~
                ~@[ -height ~a~]~@[~* -resizable 1~]"
	       (path m) file url width height resizable))

(defun play-movie (movie)
  (format-wish "~a play" (path movie)))

(defun stop-movie (movie)
  (format-wish "~a stop" (path movie)))


#|
package require QuickTimeTcl
movie .m -file U137.mov
pack .m

package require QuickTimeTcl
movie .m -url "http://www.apple.com/bbc.mov"
pack .m
|#