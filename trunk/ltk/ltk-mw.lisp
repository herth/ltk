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


|#

;;; progress bar

(defclass progress (frame)
  ((canvas :accessor canvas)
   (rect :accessor rect)
   (color :accessor color :initarg :color :initform :blue)
   (percent :accessor percent :initform 0 :initarg :percent)
   ))

(defmethod initialize-instance :after ((p progress) &key)
  (let ((c (make-instance 'canvas :master p :height 20)))
    (configure p :borderwidth 2 :relief :sunken)
    (setf (canvas p) c)
    (pack c)
    (setf (rect p) (create-rectangle c 0 0 0 20))
    (itemconfigure c (rect p) :fill (color p))
    (itemconfigure c (rect p) :outline (color p))))

(defmethod (setf color) :after (val (p progress))
  (itemconfigure (canvas p) (rect p) :fill (color p))
  (itemconfigure (canvas p) (rect p) :outline (color p)))

(defmethod (setf percent) :after (val (p progress))
  (let ((width (window-width (canvas p))))
    (set-coords (canvas p) (rect p) (list 0 0 (truncate (* val width) 100) 20))))
