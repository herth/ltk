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

o history-entry
    An entry widget keeping the history of previous input (which can be
    browsed through with cursor up and down)

o treelist
    A widget to display a tree-like structure by a set of listboxes.

o tooltip
    Shows tooltips for registered widgets
   
|#

(defpackage "LTK-MW"
  (:use "COMMON-LISP"
	"LTK"
	)
  (:export "PROGRESS"
	   "PERCENT"
	   "BAR-COLOR"
	   "REDRAW-ON-RESIZE"
	   "HISTORY-ENTRY"
	   "HISTORY"
	   "CLEAR-HISTORY"
	   "MENU-ENTRY"
	   "APPEND-ITEM"
	   "DELETE-ITEM"
	   "TREELIST"
	   "TREELIST-HAS-CHILDREN"
	   "TREELIST-CHILDREN"
	   "TREELIST-NAME"
	   "TREELIST-SELECT"
	   ))

(in-package :ltk-mw)
 

;;;; mixin class for widget construction
;;;; for widgets inheriting from redraw-on-resize the generic function
;;;; redraw is called, whenever the widget is resized (e.g. by window resize)
;;;;


(defgeneric redraw (widget))


(defclass redraw-on-resize ()
  ())

(defmethod initialize-instance :after ((r redraw-on-resize) &key)
  (bind r "<Configure>" (lambda (evt) (declare (ignore evt))
			  (redraw r))))


;;;; progress bar

(defclass progress (redraw-on-resize canvas)
  ((rect :accessor rect)
   (color :accessor bar-color :initarg :color :initform :blue)
   (percent :accessor percent :initform 0 :initarg :percent)
   ))

(defmethod redraw ((progress progress))
  (let ((width (window-width progress))
	(height (window-height progress)))
    (set-coords progress (rect progress)
		(list 0 0 (truncate (* (percent progress) width) 100) height))))

(defmethod initialize-instance :after ((progress progress) &key)
  (configure progress :borderwidth 2 :relief :sunken)
  (setf (rect progress) (create-rectangle progress 0 0 0 20))
  (itemconfigure progress (rect progress) :fill    (bar-color progress))
  (itemconfigure progress (rect progress) :outline (bar-color progress)))

(defmethod (setf bar-color) :after (val (progress progress))
  (itemconfigure progress (rect progress) :fill (bar-color progress))
  (itemconfigure progress (rect progress) :outline (bar-color progress)))

(defmethod (setf percent) :after (val (progress progress))
  (redraw progress))


;;;; history entry widget
;;;;
;;;; Entry widget with history of all text entered.
;;;; 


(defclass history-entry (entry)
  ((history :accessor history :initform (list))
   (history-pos :accessor history-pos :initform -1)
   (keepinput :accessor keepinput :initform nil :initarg :keepinput)
   ))

(defmethod add-history ((entry history-entry) txt)
  (if (> (length txt) 0)
      (push txt (history entry)))
  (setf (history-pos entry) -1))

(defmethod clear-history ((entry history-entry))
  (setf (history entry) nil)
  (setf (history-pos entry) -1))

(defmethod initialize-instance :after ((entry history-entry) &key command)
  
  (bind entry "<KeyPress-Return>"
	(lambda (event)
	  (declare (ignore event))	  
	  (let ((txt (text entry)))
	    (add-history entry txt)
	    (if (keepinput entry)
		(entry-select entry 0 "end")
		(setf (text entry) ""))
	    (ltk::callback (ltk::name entry) (list txt))	   
	    )))
  
  (bind entry "<KeyPress-Up>"
	(lambda (event)
	  (declare (ignore event))
	  (when (< (history-pos entry) (1- (length (history entry))))
	    (incf (history-pos entry))
	    (let ((val (nth (history-pos entry) (history entry))))
	    (when val
	      (setf (text entry) val)
		    )))))
  
  (bind entry "<KeyPress-Down>"
	(lambda (event)
	  (declare (ignore event))					
	  (if (>= (history-pos entry) 0)
	      (progn
		(decf (history-pos entry))
		(if (>= (history-pos entry) 0)
		    (setf (text entry) (nth (history-pos entry) (history entry)))
		  (setf (text entry) "")))
	    (progn	    
	      (setf (text entry) "")))))

  (when command (setf (command entry) command))
  )

(defmethod (setf command) (val (entry history-entry))
  (ltk::add-callback (ltk::name entry) val))

;;;;

;;;; menu entry

(defclass menu-entry (entry)
  ((menu :accessor menu)
   (entries :accessor entries :initform nil))
  )

(defmethod initialize-instance :after ((entry menu-entry) &key command content)
  (bind entry "<KeyPress-Return>"
	(lambda (event)
	  (declare (ignore event))
	  (ltk::callback (ltk::name entry) (list (text entry)))))

  (let ((mp (make-menu nil "Popup")))
    (setf (menu entry) mp)
    (dolist (c content)
      (append-item entry c))

    
    (bind entry "<1>" (lambda (event)
			(declare (ignore event))
			(popup mp (+ 3 (window-x entry))  (+ 3 (window-y entry))))))
  (when command
    (setf (command entry) command)))

(defmethod (setf command) (val (entry menu-entry))
  (ltk::add-callback (ltk::name entry) val))

(defgeneric append-item (entry item))
(defmethod append-item ((entry menu-entry) item)
  (setf (entries entry) (append (entries entry) (list item)))
  (make-menubutton (menu entry) item (lambda ()
				       (setf (text entry) item)			      
				       (ltk::callback (ltk::name entry) (list item))
				       
				       )))
(defun remove-nth (n list)
  (concatenate 'list (subseq list 0 n) (subseq list (1+ n))))

(defmethod delete-item ((entry menu-entry) index)
  (when (< index (length (entries entry)))
    (setf (entries entry) (remove-nth index (entries entry)))
    (menu-delete (menu entry) index))
  )


(defun demo ()
  (with-ltk ()
   (let* ((status '(("critical" 10 "red")
		    ("severe"  20 "orange")
		    ("normal" 50 "darkgreen")
		    ))
	  (f1 (make-instance 'frame))
	  (lstatus (make-instance 'label :master f1 :text "Status: "))
	  (bar (make-instance 'progress :master f1))
	  (f2 (make-instance 'frame))
	  (entry (make-instance 'menu-entry :master f2 :content (mapcar #'first status)))
	  )
     (pack f1 :side :top)
     (pack lstatus :side :left)
     (pack bar :side :left)
     (pack f2 :side :top)
     (pack entry :side :left)
     )))


;;; tree list widget

(defparameter *tree*
  '(nil
    ("BMW"
     ("3er"
      "318"
      "320"
      "325")
     ("5er"
      "520"
      "530"
      "535"
      "M5"))
    ("Mercedes"
     ("A-Klasse"
      "A 160"
      "A 180")
     ("C-Klasse"
      "C 200"
      "C 250")
     ("S-Klasse"
      "400 S"
      "500 S"
      "600 S"))
    ("VW"
     ("Golf"
      ("TDI"
       "1.8"
       "2.0"
       "16 V")
      "GTI"))))
       
      

(defclass treelist (frame)
  ((depth   :reader depth :initarg :depth :initform 3
	    :documentation "number of listboxes to display")
   (listbox :accessor listbox :initform nil
	    :documentation "array with the displayed listboxes")
   (data    :accessor data :initarg :data :initform nil
	    :documentation "data to be displayed")
   (lists   :accessor lists
	    :documentation "array of the lists displayed in the listbox")
   (offset  :accessor offset :initform 0
	    :documentation "index difference between data depth position and listbox position")
   (selection :accessor selection :initform nil
	      :documentation "list of selected values")
   ))

(defmethod initialize-instance :after ((tree treelist) &key listwidth listheight (background :white) )
  (setf (listbox tree) (make-array (depth tree)))
  (setf (lists tree) (make-array (depth tree)))
  (dotimes (i (depth tree))
    (let ((nr i)
	  (sb (make-instance 'scrolled-listbox :master tree :width listwidth :height listheight )))
      (grid-forget (ltk::hscroll sb))
      (setf (aref (listbox tree) nr) (listbox sb))
      (configure (listbox sb) :background background :selectforeground :white :selectbackground :blue)
      (pack sb :side :left :expand t :fill :both)
      (bind (aref (listbox tree) nr) "<<ListboxSelect>>"
	    (lambda (event)
	      (declare (ignore event))
	      (treelist-listbox-select tree (aref (listbox tree) nr) nr)))
    ))
  (when (data tree)
    (treelist-setlist tree (data tree) 0)))

(defmethod treelist-setlist ((tree treelist) data nr)
  (listbox-append (aref (listbox tree) nr) 
		  (mapcar #'treelist-name (treelist-children tree data)))
  (setf (aref (lists tree) nr) (treelist-children tree data)))

(defmethod treelist-listbox-select ((tree treelist) (listbox listbox) nr)
  (let ((sel (car (listbox-get-selection listbox))))
    (when sel
      (loop for i from (1+ nr) below (depth tree)
	    do 
	    (listbox-clear (aref (listbox tree) i)))
      (let ((selected-node (nth sel (aref (lists tree) nr))))
	(treelist-select tree selected-node)
	(when (treelist-has-children tree selected-node)
	  (listbox-append (aref (listbox tree) (1+ nr))
			  (mapcar #'treelist-name (treelist-children tree selected-node)))
	  (setf (aref (lists tree) (1+ nr)) (treelist-children tree selected-node)))))))

(defmethod treelist-select (tree node)
  )

(defmethod treelist-children (tree node)
  nil)

(defmethod treelist-has-children (tree node)
  (> (length (treelist-children tree node)) 0))

(defmethod treelist-has-children (tree (node string))
  nil)

(defmethod treelist-has-children (tree (node list))
  (rest node))

(defmethod treelist-children (tree (node string))
  nil)

(defmethod treelist-children (tree (node list))
  (rest node))

(defmethod treelist-name ((node string))
  node)

(defmethod treelist-name ((node list))
  (car node))


;;;; tooltip widget

(defclass tooltip (toplevel)
  ((label :accessor tooltip-label :initarg :label)
   ))

(defmethod initialize-instance :after ((tooltip tooltip) &key)
  (withdraw tooltip)
  (setf (tooltip-label tooltip) (make-instance 'label :text "" :background :yellow3))
  (set-wm-overrideredirect tooltip 1)
  (pack (tooltip-label tooltip) :side :left :expand t :fill :both))

(defmethod show ((tooltip tooltip) text x y)
  (setf (text (tooltip-label tooltip)) text)
  (set-geometry-xy tooltip x y)
  (normalize tooltip))

(defmethod clear ((tooltip tooltip))
  (withdraw tooltip))

