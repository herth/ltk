#|
  Copyright 2003 Peter Herth <herth@peter-herth.de>

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

|#

(defpackage "LTK"
  (:use "COMMON-LISP"
	#+:cmu "EXT"
	#+:sbcl "SB-EXT"
	)
  (:export "TEST"
	   "AFTER"
	   "APPEND-TEXT"
	   "ASK-YESNO"
	   "ASK-OKCANCEL"
	   "BIND"
	   "BUTTON"
	   "CANVAS"
	   "CREATE-IMAGE"
	   "CREATE-LINE"
	   "CREATE-MENU2"
	   "CREATE-POLYGON"
	   "CREATE-TEXT"
	   "EXIT-WISH"
	   "DO-EXECUTE"
	   "DO-MSG"
	   "ENTRY"
	   "*EXIT-MAINLOOP*"
	   "FRAME"
	   "GET-CONTENT"
	   "GET-OPEN-FILE"
	   "GET-SAVE-FILE"
	   "GET-TEXT"
	   "GRID"
	   "GRID-COLUMNCONFIGURE"
	   "GRID-ROWCONFIGURE"
	   "IMAGE-LOAD"
	   "ITEMCONFIGURE"
	   "LABEL"
	   "LOAD-TEXT"
	   "MAINLOOP"
	   "MAKE-BUTTON"
	   "MAKE-CANVAS"
	   "MAKE-ENTRY"
	   "MAKE-LABEL"
	   "MAKE-FRAME"
	   "MAKE-IMAGE"
	   "MAKE-MENU"
	   "MAKE-MENUBAR"
	   "MAKE-MENUBUTTON"
	   "MAKE-SCROLLED-CANVAS"
	   "MAKE-SCROLLBAR"
	   "MAKE-TEXT"
	   "MAKE-TOPLEVEL"
	   "MENU"
	   "MENUBAR"
	   "MENUBUTTON"
	   "MESSAGE-BOX"
	   "PACK"
	   "PHOTO-IMAGE"
	   "POSTSCRIPT"
	   "CONFIGURE"
	   "SAVE-TEXT"
	   "SCROLLBAR"
	   "SCROLLED-CANVAS"
	   "SCROLLREGION"
	   "SEE"
	   "SET-CONTENT"
	   "SET-COORDS"
	   "START-W"
	   "TAG-BIND"
	   "TAG-CONFIGURE"
	   "TEXT"
	   "*TK*"
	   "TKOBJECT"
	   "TOPLEVEL"
	   "WIDGET"
	   "WITH-LTK"
	   "WM-TITLE"
	   ))

(in-package ltk)

;communication with wish
;;; this ist the only function to adapted to other lisps

(defun do-execute (program args &optional (wt nil))
  "execute program with args a list containing the arguments passed to the program
   if wt is non-nil, the function will wait for the execution of the program to return.
   returns a two way stream connected to stdin/stdout of the program"
  
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
    #+:sbcl (let ((proc (sb-ext:run-program program args :input :stream :output :stream :wait wt)))
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

;;; verbosity of debug messages, if true, then all communication
;;; with tk is echoed to stdout
(defvar *debug-tk* t)

;;; start wish and set *w*
(defun start-w ()
  #+:sbcl (setf *w* (do-execute "/usr/bin/wish" '("-name" "LTK")))
  #-:sbcl (setf *w* (do-execute "wish" '("-name" "LTK"))))

;;; send a string to wish
(defun send-w(text)
  (if *debug-tk*
      (format t "~A~%" text))
  (format *w* "~A~%" text)
  (force-output *w*))

;;; wrapper around read-line to compensate for slight differences between lisp versions
(defun do-read-line()
  (let ((c (read-line *w*)))
    #+:lispworks (setf c (string-right-trim '(#\Newline #\Return #\Linefeed) c))
    c))

;; differences:
;; cmucl/sbcl READ expressions only if there is one more character in the stream, if
;; it is a whitespace its discarded. Lispworks READs the expression as soon as it can
;; be fully read from the stream - no character is discarded
;; so I am printing an additional space after every READable expression printed from tcl,
;; this has to be eaten for read-line from the stream in lispworks (which returns the line
;; ending character, cmucl/sbcl dont

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

;;; after <time> msec call function <fun>
(defun after(time fun)
  (add-callback "after" fun)
  (send-w (format nil "after ~a {puts -nonewline {(\"~A\") };flush stdout}" time "after")))

;; tool functions used by the objects

;; incremental counter to create unique numbers
(let ((counter 1))
  (defun get-counter()
    (incf counter)))

;; create unique widget name, append unique number to "w"
(defun create-name ()
  (format nil "w~A" (get-counter)))

;; create pathname from master widget <master> and widget name <name>
(defun create-path (master name)
  (let ((master-path (if master
			 (path master)
		       "")))
    (format nil "~A.~A" master-path name)))

;;; the library implementation 

;; basic tk object
(defclass tkobject ()
  ((name :accessor name :initarg :name :initform nil)
   (created :accessor created :initform nil))
  )

;; basic class for all widgets 
(defclass widget(tkobject)
  ((master :accessor master :initarg :master :initform nil) ;; parent widget or nil
   (path :reader path :initarg :path :initform nil)         ;; pathname to refer to the widget
   ))

;; creating of the tk widget after creating the clos object
(defmethod initialize-instance :after ((w widget) &key)
  (unless (name w)			; generate name if not given 
    (setf (name w) (create-name)))
  (unless (path w)			; and pathname
    (setf (slot-value w 'path) (create-path (master w) (name w))))
  (create w)				; call the widget specific creation method - every 
  )					; widget class needs to overload that

(defmethod create ((w widget))
  )

(defmethod bind ((w widget) event fun)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (send-w (format nil "bind  ~a ~a {puts -nonewline {(\"~A\")};flush stdout}"
		    (path w) event name ))))



(defvar *tk* (make-instance 'widget :name "." :path "."))

;;; window menu bar

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

;;; menues

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

;;; menu button

(defclass menubutton(widget) 
  ((text :accessor text :initarg :text)
   (command :accessor command :initarg :command :initform nil)))

(defmethod create ((m menubutton))
   (add-callback (name m) (command m))
   (send-w (format nil "~A add command -label {~A} -command {puts -nonewline {(\"~A\")};flush stdout}" (path (master m)) (text m) (name m)))
   )

(defun make-menubutton(menu text command)
  (let* ((mb (make-instance 'menubutton :master menu :text text :command command)))
    mb))

;;; standard button widget

(defclass button(widget)
  ((command :accessor command :initarg :command :initform nil)
   (text :accessor text :initarg :text :initform "")
   ))

(defmethod create ((bt button))
  (add-callback (name bt) (command bt))
  (send-w (format nil "button ~A -text {~A} -command {puts -nonewline {(\"~A\")};flush stdout}" (path bt) (text bt) (name bt)))
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
  ;#+:sbcl (read-line *w*)
  ;#+:lispworks (read-line *w*)
  (let ((c (do-read-line)))
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
   (vscroll :accessor vscroll)
   ))

(defun make-scrolled-canvas (master)
  (make-instance 'scrolled-canvas :master master ))

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
   (scrollregion-x0 :accessor scrollregion-x0 :initform nil)
   (scrollregion-y0 :accessor scrollregion-y0 :initform nil)
   (scrollregion-x1 :accessor scrollregion-x1 :initform nil)
   (scrollregion-y1 :accessor scrollregion-y1 :initform nil)
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
  (setf (scrollregion-x0 c) x0)
  (setf (scrollregion-y0 c) y0)
  (setf (scrollregion-x1 c) x1)
  (setf (scrollregion-y1 c) y1)
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
  (if (and (scrollregion-x0 canvas)
	   (scrollregion-x1 canvas)
	   (scrollregion-y0 canvas)
	   (scrollregion-y1 canvas))
      (send-w (format nil "~a postscript -file ~a -x ~a -y ~a -width ~a -height ~a"
		      (path canvas) filename
		      (scrollregion-x0 canvas) (scrollregion-y0 canvas)
		      (- (scrollregion-x1 canvas) (scrollregion-x0 canvas))
		      (- (scrollregion-y1 canvas) (scrollregion-y0 canvas))
		      ))
    (send-w (format nil "~a postscript -file ~a" (path canvas) filename))))

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

(defmethod append-text ((txt text) text &optional (tag nil))
  (send-w (format nil "~a insert end {~a} ~a" (path txt) text (if tag
								  tag
								""))))
(defmethod clear-text ((txt text))
  (send-w (format nil "~A delete 0.0 end" (path txt))))

(defmethod set-text ((txt text) content)
  (send-w (format nil "~A delete 0.0 end;~A insert end {~A}" (path txt) content)))


(defmethod see((txt text) pos)
  (send-w (format nil "~a see ~a" (path txt) pos)))

(defmethod tag-configure ((txt text) tag option value)
  (send-w (format nil "~a tag configure ~a -~a {~a}" (path txt) tag option value)))

(defmethod tag-bind ((txt text) tag event fun)
  "bind fun to event of the tag of the text widget txt"
  (let ((name (create-name)))
    (add-callback name fun)
    (send-w (format nil "~a tag bind ~a ~a {puts -nonewline {(\"~A\")};flush stdout}"
		    (path txt) tag event name))))


(defmethod get-text((txt text))
  (send-w (format nil "set file [open \"/tmp/ltk\" \"w\"];puts $file [~a get 1.0 end];close $file;puts \"asdf\"" (path txt)))
  (read-line *w*)
  (let (erg)
    (with-open-file (stream "/tmp/ltk" :direction :input)
      (setf erg (read-all stream)))
    (delete-file "/tmp/ltk")
    erg)
  )

(defmethod save-text((txt text) filename)
  "save the content of the text widget into the file <filename>"
  (send-w (format nil "set file [open {~a} \"w\"];puts $file [~a get 1.0 end];close $file;puts \"asdf\"" filename (path txt)))
  (read-line *w*)
  )

(defmethod load-text((txt text) filename)
  "load the content of the file <filename>"
  (send-w (format nil "set file [open {~a} \"r\"];~a delete 1.0 end;~a insert end [read $file];close $file;puts \"asdf\"" filename (path txt) (path txt)))
  (read-line *w*)
  )

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
    (send-w (format nil "puts [tk_getOpenFile -filetypes ~a];flush stdout"  files))
    ;(read-all *w*)
    (string-right-trim '(#\Newline #\Return #\Linefeed) (do-read-line))
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
    (do-read-line)
    ;(read-all *w*)
    ;(string-trim '(#\Newline #\Return #\Linefeed) (read-line *w*))
  ))

;;; see make-string-output-string/get-output-stream-string
(defun message-box (message title type icon)
  ;;; tk_messageBox function
  (send-w (format nil "puts [tk_messageBox -message {~a} -title {~a} -type {~a} -icon {~a}]" message title type icon))
  (do-read-line)
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
      (send-w (format nil "~A add command -label {~A} -command {puts -nonewline  {(\"~A\")};flush stdout}" path (first tree) name))
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
  (let ((*exit-mainloop* nil))
  (loop
    (let* ((l (read-preserving-whitespace *w* nil nil)))
      (when (null l) (return))
      (if *debug-tk*
	  (format t "l:~A<=~%" l))
      (force-output)
      (callback (first l) (rest l))
;      (ignore-errors   (callback (first l) (rest l))	    )
;      (multiple-value-bind (erg cond)
;	  (ignore-errors
	    ;(callback (first l) (rest l))
;	    t)
					;(format t "erg:~a cond:~s<=" erg cond)
;	(if (not erg)
;	    (format t "error while executing callback:~s~&" cond)))
      (when *exit-mainloop*
	(send-w "exit")
	(return))))))

;;; another way to terminate the running app, send exit command to wish

(defun exit-wish()
  (send-w "exit"))


;;; wrapper macro - initializes everything, calls body and then mainloop
;;; since 
(defmacro with-ltk (&rest body)
  `(progn
     (start-w)
     ,@body
     (mainloop)))
       

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


(defun wt2()
  (start-w)
  (let* ((text (make-text nil))
	 (f (make-frame nil))
	 (b (make-button f "get text" (lambda () (format t "=>~a<=~&" (get-text text)))))
	 (b2 (make-button f "save text" (lambda () (save-text text "/tmp/ltktest.txt"))))
	 (b3 (make-button f "load text" (lambda () (load-text text "ltk.lisp"))))
	 )
    (pack text :side "top" :expand 1 :fill "both")
    (pack f :side "bottom"  :fill "x")
    (pack b :side "left")
    (pack b2 :side "left")
    (pack b3 :side "left")
    (configure text "font" "Courier 12")
    (tag-configure text "t1" "font" "Courier 20")
    (append-text text "asdf")
    (append-text text #\Newline)    
    (append-text text "xyz" "t1")
    
    ))

(defun test2()
  (wt2)
  (mainloop))

(defun hello-1()
  (with-ltk
   (let ((b (make-button nil "Press Me"
			 (lambda ()
			   (format t "Hello World!~&")))))
     (pack b))))

(defun hello-2()
  (with-ltk
   (let* ((f (make-frame nil))
	  (b1 (make-button f "Button 1"
			   (lambda () (format t "Button1~&"))))
	  (b2 (make-button f "Button 2"
			   (lambda () (format t "Button2~&")))))
     (pack f)
     (pack b1)
     (pack b2)
     (configure f "borderwidth" 3)
     (configure f "relief" "sunken")
     )))

(defun canvastest()
  (with-ltk
   (let* ((sc (make-scrolled-canvas nil))
	  (c (canvas sc))
	  (line (create-line c  (list 100 100 400 50 700 150)))
	  (polygon (create-polygon c (list 50 150  250 160 250 300 50 330 )))
	  (text (create-text c 260 250 "Canvas test"))
	  )
     (declare (ignore text))
     (pack sc :expand 1 :fill "both")
     (scrollregion c 0 0 800 800)
     )))

(defvar *number-of-particles* 2)

(defstruct particle
  circle
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (vx 0d0 :type double-float)
  (vy 0d0 :type double-float)
  )

(defun particletest ()
  (with-ltk
   (let* ((canvas (make-canvas nil :width 800 :height 800))
	  (particles-1 (make-array *number-of-particles*))
	  (particles-2 (make-array *number-of-particles*))
	  (dt 0.01d0)
	  )
     (labels ((update-particles ()
		(let ((tmp particles-1)) ;switch arrays
		  (setf particles-1 particles-2)
		  (setf particles-2 tmp))		
		(dotimes (i *number-of-particles*)
		  (let ((particle1 (aref particles-1 i))
			(particle-old (aref particles-2 i))
			(ax 0.0d0)
			(ay 0.0d0))
		    (setf (particle-x particle1) (particle-x particle-old))
		    (setf (particle-y particle1) (particle-y particle-old))
		    (setf (particle-vx particle1) (particle-vx particle-old))
		    (setf (particle-vy particle1) (particle-vy particle-old))
				    
		    (dotimes (j *number-of-particles*)
		      (unless (= i j)
			(let* ((particle2 (aref particles-2 j))
			       (dx (- (particle-x particle2) (particle-x particle1)))
			       (dy (- (particle-y particle2) (particle-y particle1)))
			       (d2 (+ (* dx dx) (* dy dy)))
			       (d (sqrt d2)))
			  (incf ax (/ dx (* d d2)))
			  (incf dx (/ dy (* d d2)))
			)))
		    (incf (particle-x particle1) (* ax dt))
		    (incf (particle-y particle1) (* ay dt))
		    (set-coords canvas (particle-circle particle1) (list (particle-x particle1)
									 (particle-y particle1)))))
		))
			  

       (dotimes (i *number-of-particles*)
	 (setf (aref particles-1 i)
	       (make-particle :circle (make-circle canvas)))
	 )
			      
			      
       (after 50 #'update-particles)
     ))))
     