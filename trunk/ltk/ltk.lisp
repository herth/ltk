;;;; Copyright 2003 Peter Herth

; communication with wish

;;; this ist the only function to adapted to other lisps

(defun do-execute (program args &optional (wt nil))
  (let ((fullstring program))
    (dolist (a args)
      (setf fullstring (concatenate 'string fullstring " " a)))
    #+:cmupty (let ((proc (run-program program args :input t :output t :wait wt :pty :stream :error :output)))
             (unless proc
               (error "Cannot create process."))
	     (ext:process-pty proc)
             )
    #+:cmu (let ((proc (run-program program args :input :stream :output :stream :wait wt)))
             (unless proc
               (error "Cannot create process."))
             (make-two-way-stream
              (ext:process-output proc)
              (ext:process-input proc))
             )
    #+:clisp (let ((proc (run-program program :arguments args :input :stream :output :stream :wait t)))
             (unless proc
               (error "Cannot create process."))
	     proc
             )
    #+:sbcl (let ((proc (run-program program args :input :stream :output :stream :wait wt)))
             (unless proc
               (error "Cannot create process."))
	     (make-two-way-stream 
	      (process-output proc)              
	      (process-input proc))	     
             )
    #+:lispworks(system:open-pipe fullstring :direction :io)
    ))


;;; global var for holding the communication stream
(defvar *w* nil)

;;; verbosity of debug messages
(defvar *debug-tk* t)

;;; start wish and set *w*
(defun start-w ()
  #+:sbcl (setf *w* (do-execute "/usr/bin/wish" '("-name" "w.lisp")))
  #-:sbcl (setf *w* (do-execute "wish" '("-name" "w.lisp"))))

;;; send a string to wish
(defun send-w(text)
  (if *debug-tk*
      (format t "~A~%" text))
  (format *w* "~A~%" text)
  (force-output *w*))

(defun read-all(stream)
  (let ((c (read-char-no-hang stream nil nil))
        (s (make-array 256 :adjustable t :element-type 'character :fill-pointer 0)))
    (loop
     while c
     do
     (vector-push-extend c s)
     (setf c (read-char-no-hang stream nil nil))
     )
    (coerce s 'simple-string)
    ))

;;; read a line from wish
(defun read-w()
  (read-line *w* nil nil))


;;; call to convert untility
(defun convert(from to)
  (close (do-execute "convert" (list from to) t)))

;;; table used for callback every callback consists of a name of a widget and
;;; a function to call

(defvar *callbacks* (make-hash-table :test #'equal))

(defun add-callback(sym fun)
  ;(format t "add-callback (~A ~A)~%" sym fun)
  (setf (gethash sym *callbacks*) fun))

(defun callback(sym arg)
  (let ((fun (gethash sym *callbacks*)))
    ;(format t "sym:~A fun:~A~%" sym fun)
    (force-output)
    (when fun
      (apply fun arg))))


(defun after(time fun)
  (add-callback "after" fun)
  (send-w (format nil "after ~a {puts {(\"~A\")};flush stdout}" time "after")))

;; tool functions used by the objects

(let ((counter 1))
  (defun get-counter()
    (incf counter)))

(defun create-name ()
  (format nil "w~A" (get-counter)))

(defun create-path (master name)
  (let ((master-path (if master
			 (path master)
		       "")))
    (format nil "~A.~A" master-path name)))

;;; the library implementation 

(defclass tkobject ()
  ((name :accessor name :initarg :name :initform nil)
   (created :accessor created :initform nil))
  )

(defclass widget(tkobject)
  ((master :accessor master :initarg :master :initform nil)
   (path :reader path :initarg :path :initform nil)
   ))

(defmethod initialize-instance :after ((w widget) &key)
  (unless (name w)
    (setf (name w) (create-name)))
  (unless (path w)
    (setf (slot-value w 'path) (create-path (master w) (name w))))
  (create w)
  )

(defmethod create ((w widget))
  )

(defmethod bind ((w widget) tag fun)
  (add-callback (name w) fun)
  (send-w (format nil "bind  ~a ~a {puts {(\"~A\")};flush stdout}" (path w) tag (name w)))
  )


(defvar *tk* (make-instance 'widget :name "." :path "."))

(defclass menubar(widget)
  ())

(defun make-menubar(&optional (master nil))
 (make-instance 'menubar :master master :name "menubar"))


(defmethod create ((mb menubar))
  (send-w (format nil "menu ~a -tearoff 0 -type menubar" (path mb)))
  (send-w (format nil "~a configure -menu ~a" (if (master mb)
						  (path (master mb))
						".")
		  (path mb)))
  (setf (created mb) t))

(defclass menu(widget)
  ((text :accessor text :initarg :text))
  )

(defmethod create ((m menu))
   (send-w (format nil "menu ~A -tearoff 0" (path m)))
   (send-w (format nil "~A add cascade -label {~A} -menu ~a" (path (master m)) (text m) (path m))))

(defun make-menu(menu text)
  (make-instance 'menu :master menu :text text))

(defun add-separator (menu)
   (send-w (format nil "~A add separator" (path menu))))

(defclass menubutton(widget)
  ((text :accessor text :initarg :text)
   (command :accessor command :initarg :command :initform nil)))

(defmethod create ((m menubutton))
   (add-callback (name m) (command m))
   (send-w (format nil "~A add command -label {~A} -command {puts {(\"~A\")};flush stdout}" (path (master m)) (text m) (name m)))
   )

(defun make-menubutton(menu text command)
  (let* ((mb (make-instance 'menubutton :master menu :text text :command command)))
    mb))

(defclass button(widget)
  ((command :accessor command :initarg :command :initform nil)
   (text :accessor text :initarg :text :initform "")
   ))

(defmethod create ((bt button))
  (add-callback (name bt) (command bt))
  (send-w (format nil "button ~A -text {~A} -command {puts {(\"~A\")};flush stdout}" (path bt) (text bt) (name bt)))
  (setf (created bt) t))

(defun make-button (master text command)
  (let* ((b (make-instance 'button :master master :text text :command command)))
    b))

;; text entry widget

(defclass entry(widget) ())

(defmethod create ((e entry))
  (send-w (format nil "entry ~A " (path e)))
  (setf (created e) t))

(defun make-entry (master)
  (make-instance 'entry :master master))

(defmethod get-content ((e entry))  
  (send-w (format nil "puts [~A get]; flush stdout" (path e)))
  #+:sbcl (read-all *w*)

  #-:sbcl (let ((c (read-line *w*)))
    (if *debug-tk*
	(format t "gc: =>~a<=~&" c))
    ;(read-w)
    c)
  )

(defmethod set-content ((e entry) txt)
  (send-w (format nil "~A delete 0 end;~A insert end {~A}" (path e) (path e) txt)))


;;; frame widget 

(defclass frame(widget)  ())

(defmethod create ((f frame))
  (send-w (format nil "frame ~A " (path f)))
  (setf (created f) t))

(defun make-frame (master)
  (make-instance 'frame :master master))

;;; toplevel (window) widget 

(defclass toplevel (widget)  ())

(defmethod create ((w toplevel))
  (send-w (format nil "toplevel ~A " (path w)))
  (setf (created w) t))

(defun make-toplevel (master)
  (make-instance 'toplevel :master master))


;;; label widget

(defclass label(widget)
  ((text :accessor text :initarg :text :initform "")
   ))

(defmethod create ((l label))
  (send-w (format nil "label ~A -text {~A} " (path l) (text l)))
  (setf (created l) t))

(defun make-label (master text)
  (make-instance 'label :master master  :text text))

;;; scrollbar

(defclass scrollbar (widget)
  ((orientation :accessor orientation :initarg :orientation :initform "vertical")
   ))

(defun make-scrollbar(master &key (orientation "vertical"))
  (make-instance 'scrollbar :master master :orientation orientation))

(defmethod create ((sb scrollbar))
  (send-w (format nil "scrollbar ~a -orient ~a" (path sb) (orientation sb)))
  (setf (created sb) t))

(defclass scrolled-canvas (frame)
  ((canvas :accessor canvas)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)))

(defun make-scrolled-canvas (master &key )
  (make-instance 'scrolled-canvas :master master  ))

(defmethod create ((sc scrolled-canvas))
  (call-next-method)
  (setf (hscroll sc) (make-scrollbar sc :orientation "horizontal"))
  (setf (vscroll sc) (make-scrollbar sc))
  (setf (canvas sc) (make-canvas sc :xscroll (hscroll sc) :yscroll (vscroll sc)))
  (grid (canvas sc) 0 0 :sticky "news")
  (grid (hscroll sc) 1 0 :sticky "we")
  (grid (vscroll sc) 0 1 :sticky "ns")
  (grid-columnconfigure sc 0 "weight" 1)
  (grid-columnconfigure sc 1 "weight" 0)
  (grid-rowconfigure sc 0 "weight" 1)
  (grid-rowconfigure sc 1 "weight" 0)
 
  (configure (hscroll sc) "command" (concatenate 'string (path (canvas sc)) " xview"))
  (configure (vscroll sc) "command" (concatenate 'string (path (canvas sc)) " yview"))
  (configure (canvas sc) "xscrollcommand" (concatenate 'string (path (hscroll sc)) " set"))
  (configure (canvas sc) "yscrollcommand" (concatenate 'string (path (vscroll sc)) " set"))
  (setf (created sc) t)
  )


;;; canvas widget

(defclass canvas (widget)
  ((width  :accessor width  :initarg :width  :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   ))

(defmethod create ((c canvas))
  (send-w (format nil "canvas ~A~A~A" (path c)
		  (if (width c)
		      (format nil " -width ~A" (width c))
		    "")
		  (if (height c)
		      (format nil " -height ~A" (height c))
		    "")
		  ))
  (setf (created c) t))

(defun make-canvas (master &key (width nil) (height nil) (xscroll nil) (yscroll nil))
  (make-instance 'canvas :master master :width width :height height :xscroll xscroll :yscroll yscroll))

(defmethod scrollregion ((c canvas) x0 y0 x1 y1)
  (configure c "scrollregion" (format nil "~a ~a ~a ~a" x0 y0 x1 y1)))
;; canvas item functions

(defun create-line (canvas coords)
  (send-w (format nil "puts [~a create line ~{ ~a~}]" (path canvas) coords))
  (read *w* nil nil)
  )

(defun create-polygon (canvas coords)
  (send-w (format nil "puts [~a create polygon ~{ ~a~}]" (path canvas) coords))
  (read *w* nil nil)
  )

(defun create-text (canvas x y text)
  (send-w (format nil "puts [~a create text ~a ~a -anchor nw -text {~a}]" (path canvas) x y text))
  (read *w* nil nil)
  )

(defun create-image (canvas x y &key (image nil))
  (send-w (format nil "puts [~a create image ~a ~a -anchor nw ~a]" (path canvas) x y
		  (if image
		      (format nil "-image {~a}" (name image))
		    "")))
  (read *w* nil nil)
  )
  

(defun set-coords (canvas item coords)
  (send-w (format nil "~a coords ~a ~{ ~a~}" (path canvas) item coords))
  )

(defun postscript (canvas filename)
  (send-w (format nil "~a postscript -file ~a" (path canvas) filename)))

;;; text widget
(defclass text (widget)
  ((width  :accessor width  :initarg :width  :initform nil)
   (height :accessor height :initarg :height :initform nil))
  )

(defmethod create ((txt text))
  (send-w (format nil "text ~a~a~a" (path txt)
		  (if (width txt)
		      (format nil " -width ~A" (width txt))
		    "")
		  (if (height txt)
		      (format nil " -height ~A" (height txt))
		    "")
		  ))
  (setf (created txt) t))

(defun make-text (master &key (width nil) (height nil))
  (make-instance 'text :master master :width width :height height )
  )

(defmethod append-text ((txt text) text)
  (send-w (format nil "~a insert end {~a}" (path txt) text))
  )

(defmethod see((txt text) pos)
  (send-w (format nil "~a see ~a" (path txt) pos)))

;;; photo image object

(defclass photo-image(tkobject)
  ()
  )

(defmethod create ((p photo-image))
  (send-w (format nil "image create photo ~A" (name p)))
  (setf (created p) t)
  )

(defun make-image ()
  (let* ((name (create-name))
	 (i (make-instance 'photo-image :name name)))
    (create i)
    i))

(defmethod image-load((p photo-image) filename)
  ;(format t "loading file ~a~&" filename)
  (send-w (format nil "~A read {~A} -shrink" (name p) filename))
  )

(defmethod ishow((p photo-image) name)
  (convert (concatenate 'string name ".jpg")
	   "ishow.ppm")
  (image-load p "ishow.ppm"))

;;;; generic methods on widgets

;;; pack method for widget arrangement in container

(defmethod pack ((w widget) &key (side "left") (fill nil) (expand nil))
  (unless (created w)
    (create w))
  (send-w (format nil "pack ~A -side {~A}~A~A" (path w) side
		  (if fill
		      (format nil " -fill ~A" fill)
		    "")
		  (if expand
		      (format nil " -expand ~A" expand)
		    ""))))


;;; grid manager


(defmethod grid ((w widget) row column &key (sticky nil))
  (send-w (format nil "grid ~a -row ~a -column ~a ~a" (path w) row column
		  (if sticky
		      (format nil " -sticky ~a" sticky)
		    ""))))

(defmethod grid-columnconfigure (widget column option value)
  (send-w (format nil "grid columnconfigure ~a ~a -~a {~a}" (path widget) column option value)))

(defmethod grid-rowconfigure (widget row option value)
  (send-w (format nil "grid rowconfigure ~a ~a -~a {~a}" (path widget) row option value)))



;;; configure a widget parameter

(defmethod configure (widgt option value)
  ;(format t "normal config~&")
  (send-w (format nil "~A configure -~A {~A}" (path widgt) option value)))

;;; for tkobjects, the name of the widget is taken
(defmethod configure (wid option (value tkobject))
  ;(format t "config2:~&")
  (send-w (format nil "~A configure -~A {~A}" (path wid) option (name value))))

(defmethod itemconfigure (widget item option value)
  ;(format t "itemconfig ~&")
  (send-w (format nil "~A itemconfigure ~A -~A {~A}" (path widget) item option value)))

;;; for tkobjects, the name of the widget is taken
(defmethod itemconfigure (widget item option (value tkobject))
  ;(format t "itemconfig widget~&")
  (send-w (format nil "~A itemconfigure ~A -~A {~A}" (path widget) item option (name value))))




;;; wm functions

(defmethod wm-title ((w widget) title)
  (send-w (format nil "wm title ~a {~a}" (path w) title)))


;;; Dialog functions

(defun get-open-file(&optional (options '(("All Files" "*"))))
  (let ((files (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)))
    (with-output-to-string
      (s files)
      (format s "{")
      (dolist (type options)
	(let ((name (first type))
	      (wildcard (second type)))
	  (format s "{{~a} {~a}} " name wildcard)))
      (format s "}"))
    (send-w (format nil "puts [tk_getOpenFile -filetypes ~a]"  files))
    (read-all *w*)
    (read-line *w*)
  ))




(defun get-save-file(&optional (options '(("All Files" "*"))))
  (let ((files (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)))
    (with-output-to-string
      (s files)
      (format s "{")
      (dolist (type options)
	(let ((name (first type))
	      (wildcard (second type)))
	  (format s "{{~a} {~a}} " name wildcard)))
      (format s "}"))
    (send-w (format nil "puts [tk_getSaveFile -filetypes ~a]"  files))
    (read-all *w*)
    (read-line *w*)
  ))

;;; see make-string-output-string/get-output-stream-string
(defun message-box (message title type icon)
  ;;; tk_messageBox function
  (send-w (format nil "puts [tk_messageBox -message {~a} -title {~a} -type {~a} -icon {~a}]" message title type icon))
  (read-line *w*)
;  (read *w* nil nil)
  )

(defun ask-yesno(message &optional (title ""))
  (equal (message-box message title "yesno" "question") "yes"))

(defun ask-okcancel(message &optional (title ""))
  (equal (message-box message title "okcancel" "question") "ok"))

(defun do-msg(message  &optional (title ""))
  (message-box message title "ok" "info"))

#|
-type predefinedType
              Arranges for a predefined set of buttons to be dis
              played. The following values are possible for  pre
              definedType:

              abortretryignore  Displays three buttons whose sym
                                bolic names are abort, retry  and
                                ignore.

              ok                Displays  one  button  whose sym
                                bolic name is ok.

              okcancel          Displays two buttons  whose  sym
                                bolic names are ok and cancel.

              retrycancel       Displays  two  buttons whose sym
                                bolic names are retry and cancel.

              yesno             Displays  two  buttons whose sym
                                bolic names are yes and no.

              yesnocancel       Displays three buttons whose sym
                                bolic  names are yes, no and can
                                cel.
     -icon iconImage
              Specifies an icon to display. IconImage must be one
              of the following: error, info, question or warning.
              If this option is not specified, then the info icon
              will be displayed.

|#

;;;

(defun cm (tree path)
  (cond
   ((eq tree :separator)
    (send-w (format nil "~A add separator" path)))
   ((listp (second tree))
    (let ((newpath (format nil "~A.~A" path (create-name))))
      (when (and (equal path ".menubar")
		 (or (equal (first tree) "Help")
		     (equal (first tree) "help")
		     (equal (first tree) "Hilfe")))
	(setf newpath ".menubar.help"))
      (send-w (format nil "menu ~A -tearoff 0" newpath))
      (send-w (format nil "~a add cascade -label \"~a\" -menu ~a" path (first tree) newpath))
      (dolist (entry (second tree))
	(cm entry newpath))))
   (t
    (let* ((name (create-name)))
      (add-callback name (second tree))		     
      (send-w (format nil "~A add command -label {~A} -command {puts {(\"~A\")};flush stdout}" path (first tree) name))
      ))))

(defun create-menu2 (menutree)
  (send-w (format nil "menu .menubar -tearoff 0 -type menubar"))
  (dolist (e menutree)
    (cm e ".menubar"))
  (send-w (format nil ". configure -menu .menubar"))
  )  

;;;; main event loop, runs until stream is closed by wish (wish exited) or
;;;; the variable *exit-mainloop* is set

(defvar *exit-mainloop* nil)

(defun mainloop()
  (setf *exit-mainloop* nil)
  (loop
    (let* ((l (read *w* nil nil)))
      (when (null l) (return))
      (if *debug-tk*
	  (format t "l:~A<=~%" l))
      (force-output)
      (callback (first l) (rest l))
;      (ignore-errors   (callback (first l) (rest l))	    )
      (multiple-value-bind (erg cond)
	  (ignore-errors
	    ;(callback (first l) (rest l))
	    t)
	;(format t "erg:~a cond:~s<=" erg cond)
	(if (not erg)
	    (format t "error while executing callback:~s~&" cond)))
      (when *exit-mainloop*
	(send-w "exit")
	(return)))))

;;; another way to terminate the running app

(defun exit-wish()
  (send-w "exit"))


;;;; testing functions

(defvar *do-rotate* nil)
(defvar *demo-line* nil)
(defvar *demo-canvas* nil)

(defun wt()
  (start-w)
  (let* ((bar (make-frame nil))
	 (fr (make-frame bar))
	 (lr (make-label fr "Rotation:"))
	 (bstart (make-button fr "Start" 'start-rotation))
	 (bstop  (make-button fr "Stop"  'stop-rotation))
	 (b1 (make-button bar "Hallo" (lambda () (format T "Hallo~%"))))
	 (b2 (make-button bar "Welt!" (lambda () (format T "Welt~%"))))
	 (f (make-frame bar))
	 (l (make-label f "Test:"))
	 (b3 (make-button f "Ok." 'test-rotation)); (setf *exit-mainloop* t))))
	 (e (make-entry bar))
	 (b4 (make-button bar "get!" (lambda () (format T "content of entry:~A~%" (get-content e)))))
	 (b5 (make-button bar "set!" (lambda () (set-content e "test of set"))))
	 (sc (make-scrolled-canvas nil)); :width 500 :height 500))
	 (c (canvas sc))
	 (lines nil)
	 mb mfile mf-load mf-save mf-export mfe-jpg mfe-gif mf-exit mf-print
	 )
    (setf mb (make-menubar))
    (setf mfile (make-menu mb "File"))
    (setf mf-load (make-menubutton mfile "Load" (lambda () (format t "Load pressed~&"))))
    (setf mf-save (make-menubutton mfile "Save" (lambda () (format t "Save pressed~&"))))
    (add-separator mfile)
    (setf mf-export (make-menu mfile "Export..."))
    (add-separator mfile)
    (setf mf-print (make-menubutton mfile "Print" (lambda () (postscript c "wt.ps"))))
    (add-separator mfile)
    (setf mfe-jpg (make-menubutton mf-export "jpeg" (lambda () (format t "Jpeg pressed~&"))))
    (setf mfe-gif (make-menubutton mf-export "png" (lambda () (format t "Png pressed~&"))))
    (setf mf-exit (make-menubutton mfile "Exit" (lambda () (setf *exit-mainloop* t))))

    (configure c "borderwidth" "2")
    (configure c "relief" "sunken")
    (pack sc :side "top" :fill "both" :expand 1)
    (pack bar :side "bottom")
    (scrollregion c 0 0 500 400)
    (pack fr)
    (pack lr)
    (configure fr "borderwidth" "2")
    (configure fr "relief" "sunken")
    (pack bstart)
    (pack bstop)
    (pack b1)
    (pack b2)
    (configure f "borderwidth" "2")
    (configure f "relief" "sunken")
    (pack f :fill "x")
    (pack l)
    (pack b3)
    (pack e)
    (pack b4)
    (pack b5)
    (dotimes (i 100)
      (let ((w (* i 2.8001)))
	(let ((x (+ 250 (* 150 (sin w))))
	      (y (+ 200 (* 150 (cos w)))))
	  (push y lines)
	  (push x lines)
	)))
    (setf *demo-line* (create-line c lines))
    (setf *demo-canvas* c)
    (create-text c 10 10 "Ltk Demonstration")
    ))

(defvar *angle* 0)
(defvar *angle2* 0)
(defvar *angle3* 0)


(defun rotate()
  (let ((*debug-tk* nil))
    (let ((lines nil)
	  (dx (* 50 (sin *angle2*)))
	  (dy (* 50 (cos *angle2*)))
	  (wx (sin *angle3*))
;	  (wy (cos *angle3*))
	  )
      (incf *angle* 0.1)
      (incf *angle2* 0.03)
      (incf *angle3* 0.01)
      
      (dotimes (i 100)
	(let ((w (+ *angle* (* i 2.8001))))
	  (let ((x (+ dx 250 (* 150 (sin w) wx)))
		(y (+ dy 200 (* 150 (cos w)))))
	    (push y lines)
	    (push x lines)
	    )))    
      (set-coords *demo-canvas* *demo-line* lines))
    (if *do-rotate*
	(after 25 #'rotate))))

(defun test-rotation()
  (setf *debug-tk* nil)
  (time (dotimes (i 100)
	  (rotate)))
  )
(defun start-rotation()
  (setf *debug-tk* nil)
  (setf *do-rotate* t)
  (rotate)
  )
(defun stop-rotation()
  (setf *debug-tk* t)
  (setf *do-rotate* nil)
  )

(defun test()
  (wt)
;  (read-all *w*)
  (mainloop))

