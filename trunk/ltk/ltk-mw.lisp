#|

 This software is Copyright (c) 2004 Peter Herth <herth@peter-herth.de>

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

#|

This is the Ltk megawidgets package. It consists of widgets usable
for Ltk, written in Lisp/tcl. So wherever the Ltk package runs, this
extensing package should run as well.


Widgets offered are:

o progress
    A widget displaying a progress bar
   



|#

(defpackage "LTK-MW"
  (:use "COMMON-LISP"
	"LTK"
	)
  (:export "PROGRESS"
	   "PERCENT"
	   "BAR-COLOR"
	   ))

(in-package :ltk-mw)

;;; progress bar

(defclass progress (frame)
  ((canvas :accessor canvas)
   (rect :accessor rect)
   (color :accessor bar-color :initarg :color :initform :blue)
   (percent :accessor percent :initform 0 :initarg :percent)
   ))

(defmethod initialize-instance :after ((progress progress) &key)
  (let ((canvas (make-instance 'canvas :master progress :height 20)))
    (configure progress :borderwidth 2 :relief :sunken)
    (setf (canvas progress) canvas)
    (pack canvas :expand t :fill :both)
    (setf (rect progress) (create-rectangle canvas 0 0 0 20))
    (itemconfigure canvas (rect progress) :fill (bar-color progress))
    (itemconfigure canvas (rect progress) :outline (bar-color progress))))

(defmethod (setf bar-color) :after (val (progress progress))
  (itemconfigure (canvas progress) (rect progress) :fill (bar-color progress))
  (itemconfigure (canvas progress) (rect progress) :outline (bar-color progress)))

(defmethod (setf percent) :after (val (progress progress))
  (let ((width (window-width (canvas progress)))
	(height (window-height (canvas progress))))
    (set-coords (canvas progress) (rect progress)
		(list 0 0 (truncate (* val width) 100) height))))
