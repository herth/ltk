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

(defpackage :ltk-mw
  (:use :common-lisp :ltk)
  (:export
   #:progress
   #:percent
   #:bar-color
   #:redraw-on-resize
   #:history-entry
   #:history
   #:clear-history
   #:menu-entry
   #:append-item
   #:delete-item
   #:treelist
   #:treelist-has-children
   #:treelist-children
   #:treelist-name
   #:treelist-select
   #:gtree
   #:tooltip
   #:show
   #:clear
   #:cancel-tooltip
   #:popup-time
   #:register-tooltip
   #:schedule-tooltip

   ;; list-select widget
   #:list-select
   #:data
   #:list-select-display
   #:selected-elements
   #:ltk-mw-demo
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
   (text-display :accessor text-display :initform nil :initarg :text-display)
   ))

(defmethod redraw ((progress progress))
  (let ((width (window-width progress))
        
	(height (window-height progress)))
    (set-coords progress  (text-display progress) (list (truncate width 2) (truncate height 2)))
    (set-coords progress (rect progress)
		(list 0 0 (truncate (* (percent progress) width) 100) height))))

(defmethod initialize-instance :after ((progress progress) &key)
  (configure progress :borderwidth 2 :relief :sunken)
  (setf (rect progress) (create-rectangle progress 0 0 0 20))
  (setf (text-display progress) (make-instance 'canvas-text :canvas progress :x 0 :y 0 :text ""))
  (configure (text-display progress) :anchor :center :fill :yellow)
  (itemconfigure progress (rect progress) :fill    (bar-color progress))
  (itemconfigure progress (rect progress) :outline (bar-color progress)))

(defmethod (setf bar-color) :after (val (progress progress))
  (declare (ignore val))
  (itemconfigure progress (rect progress) :fill (bar-color progress))
  (itemconfigure progress (rect progress) :outline (bar-color progress)))

(defmethod (setf percent) :after (val (progress progress))
  (declare (ignore val))
  (redraw progress))

(defmethod (setf text) (value (progress progress))
  (configure (text-display progress) :text value))

;;;; history entry widget
;;;;
;;;; Entry widget with history of all text entered.
;;;; 


(defclass history-entry (entry)
  ((history :accessor history :initform (list))
   (history-pos :accessor history-pos :initform -1)
   (keepinput :accessor keepinput :initform nil :initarg :keepinput)
   ))

(defgeneric add-history (entry txt))
(defmethod add-history ((entry history-entry) txt)
  (if (> (length txt) 0)
      (push txt (history entry)))
  (setf (history-pos entry) -1))

(defgeneric clear-history (entry))
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

(defgeneric delete-item (entry index))
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

(defclass treelist (frame)
  ((depth   :reader depth :initarg :depth :initform 3
	    :documentation "number of listboxes to display")
   (listbox :accessor listbox :initform nil
	    :documentation "array with the displayed listboxes")
   (data    :accessor data :initarg :data :initform nil
	    :documentation "root node to be displayed (its children fill the first box)")
   (entries :accessor entries
	    :documentation "array of the lists displayed in the listbox")
   (offset  :accessor offset :initform 0
	    :documentation "index difference between data depth position and listbox position")
   (selection :accessor selection :initform nil
	      :documentation "list of selected values")
   ))

(defclass tree-entry ()
  ((nodes :accessor nodes :initform nil :initarg :nodes)
   (index :accessor index :initform nil :initarg :index)
   (parent-node :accessor parent-node :initform nil :initarg :parent-node)
   (selected-node :accessor selected-node :initform nil :initarg :selected-node)))

(defmethod initialize-instance :after ((tree treelist) &key listwidth listheight (background :white))
  (setf (listbox tree) (make-array (depth tree)))
  (setf (entries tree) (make-array (depth tree) :adjustable t :fill-pointer 0))
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
	      (treelist-listbox-select tree nr)))))
  (when (data tree)
    (treelist-set-root-node tree (data tree)))
  )

(defgeneric treelist-set-root-node (tree node))
(defmethod treelist-set-root-node ((tree treelist) node)
  (setf (data tree) node)
  (treelist-setlist tree node 0))

(defgeneric treelist-clearlist (tree index))
(defmethod treelist-clearlist ((tree treelist) index)
  (when (< index (depth tree))
    (setf (aref (entries tree) index) nil)
    (listbox-clear (aref (listbox tree) index))
    (treelist-clearlist tree (1+ index))))

(defgeneric treelist-setlist (tree parent-node nr))
(defmethod treelist-setlist ((tree treelist) parent-node nr)
  (when (< nr (depth tree))
    (treelist-clearlist tree nr)
    (let ((entry (make-instance 'tree-entry
				:nodes (treelist-children tree parent-node)
				:index nr
				:parent-node parent-node)))
      (setf (aref (entries tree) nr) entry)
      (listbox-append (aref (listbox tree) nr) 
                      (mapcar (lambda (node)
                                (treelist-name tree node)) (nodes entry))))))

(defgeneric treelist-listbox-select (tree nr))
(defmethod treelist-listbox-select ((tree treelist) nr)
  (let* ((listbox (aref (listbox tree) nr))
	 (oldsel (selected-node (aref (entries tree) nr)))
	 (sel (car (listbox-get-selection listbox))))
    (when oldsel
      (listbox-configure listbox oldsel :background :white :foreground :black))
    (setf (selected-node (aref (entries tree) nr)) sel)
    (when sel
      (listbox-configure listbox sel :background :blue :foreground :white)
      (let* ((entry (aref (entries tree) nr))
	     (selected-node (nth sel (nodes entry))))
        (listbox-configure listbox sel :background :blue :foreground :white)
        (treelist-select tree selected-node)
        (treelist-setlist tree selected-node (1+ nr))
        ))))
  
(defgeneric treelist-select (tree node)
  (:documentation "callback for selecting a tree node"))

(defmethod treelist-select (tree node)
    (declare (ignore tree node)))

(defgeneric treelist-children (tree node)
  (:documentation "list of children for a node in a tree"))

(defmethod treelist-children (tree node)
  (declare (ignore tree node))
  nil)

(defgeneric treelist-has-children (tree node)
  (:documentation "is non-nil, if the node has children"))

(defmethod treelist-has-children (tree node)
  (treelist-children tree node))

(defgeneric treelist-name (tree node)
  (:documentation "String to display in the tree list for a node"))

(defmethod treelist-name (tree (node string))
  (declare (ignore tree)))

;;; demo tree widget

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

(defclass demo-tree (treelist)
  ())

(defmethod treelist-name ((tree demo-tree) (node list))
  (car node))

(defmethod treelist-children ((tree demo-tree) (node string))
  nil)

(defmethod treelist-children ((tree demo-tree) (node list))
  (rest node))

(defun treelist-test ()
  (with-ltk ()
    (pack (make-instance 'demo-tree :data *tree*) :expand t :fill :both)))

;;;; tooltip widget

(defclass tooltip (toplevel)
  ((label :accessor tooltip-label :initarg :label)
   (popup-time :accessor popup-time :initform 1000 :initarg :popup-time)
   ))

(defparameter *tooltip-afterid* nil)

(defmethod initialize-instance :after ((tooltip tooltip) &key)
  (withdraw tooltip)
  (setf (tooltip-label tooltip) (make-instance 'label :text "" :background :yellow3 :master tooltip :justify :left))
  (set-wm-overrideredirect tooltip 1)
  (pack (tooltip-label tooltip) :side :left :expand t :fill :both))

(defgeneric show (tooltip text x y))
(defmethod show ((tooltip tooltip) text x y)
  (let ((txt (typecase text
               (function
                (with-output-to-string (s)
                  (funcall text s)))
               (string
                text)
               (t
                (format nil "~a" text)))))
    (when (and txt (> (length txt) 0))
      (setf (text (tooltip-label tooltip)) txt)
      (set-geometry-xy tooltip (truncate x)  (truncate y))
      (normalize tooltip)
      (raise tooltip))))

(defgeneric popup-tooltip (tooltip))
(defmethod popup-tooltip ((tooltip tooltip))
  (normalize tooltip)
  (raise tooltip))
 
(defgeneric schedule-tooltip (tooltip text x y time)
  )

(defmethod schedule-tooltip (tooltip text x y time)
  (cancel-tooltip tooltip)
  (setf *tooltip-afterid*
 	(after time (lambda ()
 		      (show tooltip text x y)))))

(defgeneric cancel-tooltip (tooltip))
(defmethod cancel-tooltip ((tooltip tooltip))
  (when *tooltip-afterid*
    (after-cancel *tooltip-afterid*)
    (setf *tooltip-afterid* nil)))

(defmethod clear ((tooltip tooltip))
  (withdraw tooltip))

(defgeneric register-tooltip (tooltip widget content))
(defmethod register-tooltip ((tooltip tooltip) (widget widget) content)
  (bind widget "<Leave>" (lambda (event)
			   (declare (ignore event))
			   (clear tooltip)
			   (cancel-tooltip tooltip))
	:append t)
  (bind widget "<Motion>" (lambda (event)
			    (clear tooltip)
			    (cancel-tooltip tooltip)
			    (schedule-tooltip tooltip
					      content
					      (+ 30 (event-root-x event))
					      (+ 10 (event-root-y event))
					      (popup-time tooltip)))
	:append t)
  widget)

(defmethod configure ((tooltip tooltip) option value &rest others)
  (apply #'configure (tooltip-label tooltip) option value others))

(defun tooltip-test ()
  (with-ltk ()
    (let ((b (make-instance 'button :text "Tooltip"))
	  (tooltip (make-instance 'tooltip)))
      (pack b)
      (configure tooltip :borderwidth 2 :relief :ridge)
      (register-tooltip tooltip b (lambda (s) (format s "~d" (random 100)))))))

;;;; graphical tree widget

(defclass gtree (canvas)
  ((data :accessor data :initform nil :initarg :data)
   ))

(defgeneric render-tree (g d x y))
(defmethod render-tree ((g gtree) data x y)
  (let ((h 0))
    (when (gtree-content g data)
      (if (gtree-children g data)
	(dolist (c (gtree-children g data))
	  (incf h (render-tree g c (+ x 100) (+ y h))))
	(incf h 30))
      (let* ((c (gtree-render-node g (gtree-content g data)))
	     (w (create-window g x (+ y (truncate h 2)) c)))
        (declare (ignore w))
	))
    h))
  

(defmethod initialize-instance :after ((g gtree) &key)
  (render-tree g (data g) 0 0)
  )

(defgeneric gtree-children (gtree node)
  )

(defgeneric gtree-content (gtree node)
  )

(defgeneric gtree-render-node (gtree node))


(defclass gtree-demo (gtree)
  ())

(defmethod gtree-children ((d gtree-demo) (node list))
  (rest node))

(defmethod gtree-content ((d gtree-demo) (node list))
  (first node))

(defmethod gtree-render-node ((d gtree-demo) node )
  (make-instance 'label :master d :text node :borderwidth 3 :relief :raised :background :grey :height 1 :width 10))


(defun gtree-demo ()
  (with-ltk
   ()
   (let* ((tree (make-instance 'gtree-demo
			       :data '(a (b (d (h)
					       (i))
					    (e (j)
					       (k)))
					 (c (f)
					    (g))))))
     (pack tree :side :left :expand t :fill :both)
     (format t "data: ~s~%" (data tree)) (force-output)
     )))
	    
;;; list-select box widget

(defclass list-select (listbox)
  ((data :accessor data :initarg :data :initform nil)
   ))

(defgeneric list-select-display (select item))

(defmethod list-select-display ((select list-select) item)
  (format nil "~a" item))

(defgeneric selected-elements (select))

(defmethod selected-elements ((select list-select))
  (let ((selection (listbox-get-selection select)))
    (when selection
      (mapcar (lambda (index)
                (nth index (data select)))
              selection))))

(defmethod (setf data) :after (val (select list-select))
  (listbox-clear select)
  (listbox-append select (mapcar (lambda (item)
                                   (list-select-display select item))
                                 (data select))))


;;; demo

(defclass list-select-demo-entry ()
  ((file :accessor file :initarg :file :initform nil)
   (size :accessor size :initarg :size :initform 0)))

(defmethod list-select-display ((ls list-select) (entry list-select-demo-entry))
  (format nil "~a ~d Bytes" (namestring (file entry)) (size entry)))

(defun make-list-select-demo (&optional (master nil))
  (let* ((f (make-instance 'frame :master master))
         (ls (make-instance 'list-select :master f :selectmode :multiple))
         (f2 (make-instance 'frame :master f))
         (lsize (make-instance 'label :master f2 :text "Total Size:"))
         (bsize (make-instance 'button :text "Calc" :master f2
                               :command (lambda ()
                                          (setf (text lsize)
                                                (format nil "Total Size: ~a" (loop for e in (selected-elements ls)
                                                                                  summing (size e))))))))
    (pack ls :side :top :expand t :fill :both)
    (pack f2 :side :top :fill :x)
    (pack bsize :side :left)
    (pack lsize :side :left)
    (setf (data ls)
          (mapcar (lambda (p)
                    (make-instance 'list-select-demo-entry
                                   :file p
                                   :size (with-open-file (s p)
                                           (file-length s))))
                  (directory (make-pathname :name :wild :type :wild))))
    f))

(defun list-select-demo ()
  (with-ltk ()
    (let ((f (make-list-select-demo)))
      (pack f :side :top :expand t :fill :both))))


(defun ltk-mw-demo ()
  (with-ltk ()
    (pack (make-list-select-demo) :side :top :expand t :fill :both)
    ))
