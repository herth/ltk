#|

 This software is Copyright (c) 2003 Peter Herth <herth@peter-herth.de>

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
All tk commads as of version 8.4 with support information. "-" means not
supported by purpose (look comment), "x" means supported, though some
options may not be supported. 

command      supported comment
bell                 x
bind                 x does not pass the event information
bindtags               modifly the tag list of a widget that describes which events it gets
bitmap               - see image
button               x
canvas               x (quite some graphic elements missing)
checkbutton          x
clipboard            x (canvas get missing... tricky...)
colors               - constants only
console              - only on some platforms
cursors              x 
destroy              x
entry                x
event                  create and manage virtual events
focus                  focus management functions
font
frame                x
grab                 - 
grid                 x
image                x photo image only
keysyms              - constants only
label                x
labelframe           x
listbox              x
loadTk               -
lower                x
menu                 x
menubutton           x
message              x 
option               -
options              -
pack                 x
panedwindow          x
photo                - see image
place                x geometry manager using coordinates
radiobutton          x
raise                x
scale                x 
scrollbar            x
selection
send
spinbox              x
text                 x
tk
tk_bisque            -
tk_chooseColor
tk_chooseDirectory
tk_dialog
tk_focusFollowsMouse 
tk_focusNext
tk_focusPrev
tk_getOpenFile       x
tk_getSaveFile       x
tk_menuSetFocus      -
tk_messageBox        x
tk_optionMenu
tk_popup
tk_setPalette        -
tk_textCopy
tk_textCut
tk_textPaste
tkerror              -
tkvars               -
tkwait               
toplevel             x
winfo                x
wm                   x 
|#

(defpackage "LTK"
  (:use "COMMON-LISP"
	#+:cmu "EXT"
	#+:sbcl "SB-EXT"
	)
  (:export "LTKTEST"
	   "*LTK-VERSION*"
	   "*CURSORS*"
	   "*DEBUG-TK*"
	   "*EXIT-MAINLOOP*"
	   "*INIT-WISH-HOOK*"
	   "*MB-ICONS*"
	   "*TK*"
	   "*WISH*"
	   "*WISH-PATHNAME*"
	   "ADD-PANE"
	   "ADD-SEPARATOR"
	   "AFTER"
	   "APPEND-TEXT"
	   "ASK-OKCANCEL"
	   "ASK-YESNO"
	   "BACKGROUND"
	   "BELL"
	   "BIND"
	   "BUTTON"
	   "CANVAS"
	   "CHECK-BUTTON"
	   "CGET"
	   "CLEAR-TEXT"
	   "CLIPBOARD-APPEND"
	   "CLIPBOARD-CLEAR"
	   "CLIPBOARD-GET"
	   "COMMAND"
	   "CONFIGURE"
	   "CREATE-ARC"
	   "CREATE-BITMAP"
	   "CREATE-IMAGE"
	   "CREATE-LINE"
	   "CREATE-LINE*"
	   "CREATE-MENU2"
	   "CREATE-OVAL"
	   "CREATE-POLYGON"
	   "CREATE-RECTANGLE"
	   "CREATE-TEXT"
	   "CREATE-WINDOW"
	   "DEICONIFY"
	   "DESTROY"
	   "DO-EXECUTE"
	   "DO-MSG"
	   "ENTRY"
	   "ENTRY-SELECT"
	   "EXIT-WISH"
	   "EVENT"
	   "EVENT-X"
	   "EVENT-Y"
	   "EVENT-KEYCODE"
	   "EVENT-CHAR"
	   "FORGET-PANE"
	   "FORMAT-WISH"
	   "FRAME"
	   "GEOMETRY"
;	   "GET-CONTENT"
	   "GET-OPEN-FILE"
	   "GET-SAVE-FILE"
;	   "GET-TEXT"
	   "GRID"
	   "GRID-COLUMNCONFIGURE"
	   "GRID-CONFIGURE"
	   "GRID-ROWCONFIGURE"
	   "ICONIFY"
	   "ICONWINDOW"
	   "IMAGE-LOAD"
	   "IMAGE-SETPIXEL"
	   "INTERIOR"
	   "ITEMCONFIGURE"
	   "ITEMLOWER"
	   "ITEMRAISE"
	   "LABEL"
	   "LABELFRAME"
	   "LISTBOX"
	   "LISTBOX-APPEND"
	   "LISTBOX-CLEAR"
	   "LISTBOX-GET-SELECTION"
	   "LISTBOX-SELECT"
	   "LOAD-TEXT"
	   "LOWER"
	   "MAINLOOP"
	   "MAKE-BUTTON"
	   "MAKE-CANVAS"
	   "MAKE-ENTRY"
	   "MAKE-FRAME"
	   "MAKE-IMAGE"
	   "MAKE-LABEL"
	   "MAKE-MENU"
	   "MAKE-MENUBAR"
	   "MAKE-MENUBUTTON"
	   "MAKE-SCROLLBAR"
	   "MAKE-SCROLLED-CANVAS"
	   "MAKE-TEXT"
	   "MAKE-TOPLEVEL"
	   "MAXSIZE"
	   "MENU"
	   "MENUBAR"
	   "MENUBUTTON"
	   "MENUCHECKBUTTON"
	   "MENURADIOBUTTON"
	   "MESSAGE"
	   "MESSAGE-BOX"
	   "MINSIZE"
	   "NORMALIZE"
	   "ON-CLOSE"
	   "ON-FOCUS"
	   "PACK"
	   "PACK-FORGET"
	   "PANE-CONFIGURE"
	   "PANED-WINDOW"
	   "PATH"
	   "PHOTO-IMAGE"
	   "PLACE"
	   "POSTSCRIPT"
	   "RADIO-BUTTON"
	   "RAISE"
	   "SAVE-TEXT"
	   "SCALE"
	   "SCREEN-HEIGHT"
	   "SCREEN-HEIGHT-MM"
	   "SCREEN-MOUSE"
	   "SCREEN-MOUSE-X"
	   "SCREEN-MOUSE-Y"
	   "SCREEN-WIDTH"
	   "SCREEN-WIDTH-MM"
	   "SCROLLBAR"
	   "SCROLLED-CANVAS"
	   "SCROLLED-FRAME"
	   "SCROLLED-LISTBOX"
	   "SCROLLED-TEXT"
	   "SCROLLREGION"
	   "SEE"
	   "SEND-WISH"
;	   "SET-CONTENT"
	   "SET-COORDS"
	   "SET-COORDS*"
	   "SET-GEOMETRY"
;	   "SET-TEXT"
	   "SPINBOX"
	   "START-WISH"
	   "TAG-BIND"
	   "TAG-CONFIGURE"
	   "TEXT"
	   "TEXTBOX"
	   "TKOBJECT"
	   "TOPLEVEL"
	   "VALUE"
	   "WIDGET"
	   "WINDOW-HEIGHT"
	   "WINDOW-WIDTH"
	   "WINDOW-X"
	   "WINDOW-Y"
	   "WITH-LTK"
	   "WITH-REMOTE-LTK"
	   "WITHDRAW"
	   "WM-TITLE"
	   ))

(in-package :ltk)

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
    #+:clisp (let ((proc (ext:run-program program :arguments args :input :stream :output :stream :wait t)))
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
    #+:lispworks (system:open-pipe fullstring :direction :io)
    #+:allegro (let ((proc (excl:run-shell-command
                         (apply #'vector program program args)
                         :input :stream :output :stream :wait wt)))
		(unless proc
		  (error "Cannot create process."))   
		proc
		)
    #+:ecl(ext:run-program program args :input :stream :output :stream
:error :output)
     #+:openmcl (let ((proc (ccl:run-program program args :input
:stream :output :stream :wait wt)))
		  (unless proc
		    (error "Cannot create process."))
		  (make-two-way-stream
		   (ccl:external-process-output-stream proc)
		   (ccl:external-process-input-stream proc)))
    ))

(defvar *ltk-version* 0.86)

;;; global var for holding the communication stream
(defvar *wish* nil)

;;; verbosity of debug messages, if true, then all communication
;;; with tk is echoed to stdout
(defvar *debug-tk* t)

(defvar *wish-pathname*
  #+freebsd			"wish8.4"
  #+(and sbcl (not freebsd))	"/usr/bin/wish"
  #-(or sbcl freebsd)		"wish")

(defvar *wish-args* '("-name" "LTK"))

(defvar *init-wish-hook* nil)

;;; setup of wish
;;; put any tcl function definitions needed for running ltk here
(defun init-wish ()
  ;; print string readable, escaping all " and \
  ;; proc esc {s} {puts "\"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\""}
  (send-wish "proc esc {s} {puts \"\\\"[regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]\\\"\"} ")
  (send-wish "proc escape {s} {return [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]} ")
  ;;; proc senddata {s} {puts "(data \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}
  (send-wish "proc senddata {s} {puts \"(:data [escape $s])\";flush stdout}")
  (send-wish "proc senddatastring {s} {puts \"(:data \\\"[escape $s]\\\")\";flush stdout} ")
  
  ;;; proc sendevent {s} {puts "(event \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}
  (send-wish "proc sendevent {s x y keycode char} {puts \"(:event \\\"$s\\\" $x $y $keycode $char)\"} ")
  ;;; proc callback {s} {puts "(callback \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}

  ;;; callback structure: (:callback "widgetname")          ;; for non-parameter callbacks
  ;;;                     (:callback "widgetname" val)      ;; wideget returns non-string value
  ;;;                     (:callback "widgetname" "string") ;; widget returns string value

  (send-wish "proc callback {s} {puts \"(:callback \\\"$s\\\")\";flush stdout} ")
  (send-wish "proc callbackval {s val} {puts \"(:callback \\\"$s\\\" $val)\"} ")
  (send-wish "proc callbackstring {s val} {puts \"(:callback \\\"$s\\\" \\\"[escape $val]\\\")\"} ")

  (dolist (fun *init-wish-hook*)	; run init hook funktions 
    (funcall fun)))

;;; start wish and set *wish*
(defun start-wish ()
  (setf *wish* (do-execute *wish-pathname* *wish-args*))  ; open subprocess
  (init-wish)				               ; perform tcl initialisations
  )

;;; send a string to wish
(defun send-wish (text)
  (when *debug-tk*
    (format t "~A~%" text)
    (force-output))
  (format *wish* "~A~%" text)
  (force-output *wish*))

(defun format-wish (control &rest args)
  "format args using control as control string to wish"
  (when *debug-tk*
    (apply #'format t control args)
    (format t "~%")
    (force-output))
  (apply #'format *wish* control args)
  (format *wish* "~%")
  (force-output *wish*))

#|
;;; wrapper around read-line to compensate for slight differences between lisp versions
(defun do-read-line()
  (let ((c (read-line *wish*)))
    #+:lispworks (setf c (string-right-trim '(#\Newline #\Return #\Linefeed) c))
    c))
|#


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

;;; read from wish 
(defun read-wish()
  (let ((*read-eval* nil))
    (read *wish* nil nil)))

(defvar *event-queue* nil)

(defun read-event ()
  (let ((pending (pop *event-queue*)))
    ;(format t "re:pending:~a~%" pending) (force-output)
    (unless pending
      (setf pending (read-preserving-whitespace *wish* nil nil)))
    ;(format t "re:returning:~a~%" pending) (force-output)
    pending
    ))

(defun read-data ()
  (let ((d (read-wish)))
    (if (listp d) ; paranoia check when we do not read a list eg. error messages from wish
	(progn
	  (loop while (not (equal (first d) :data))
	    do
	    (setf *event-queue* (append *event-queue* (list d)))
	    (format t "postponing event: ~a ~%" d)
	    (force-output)
	    (setf d (read-wish)))
					;(format t "readdata: ~s~%" d) (force-output)
	  (second d))
      (format t "read-data:~a~a~%" d (read-all *wish*)))
    ))

;;; sanitizing strings: lisp -> tcl (format *wish* "{~a}" string)
;;; in string escaped : {} mit \{ bzw \}  und \ mit \\
#|
(defun replace-char (char with txt)
  (let ((pos (position char txt)))
    (loop
      while pos
      do
      (progn
	(format t "txt: ~a -> " txt)
	(setf txt (concatenate 'string (subseq txt 0 pos) with (subseq txt pos)))
	(format t " ~a~&" txt)
	(force-output)
	(setf pos (position char txt :start (+ pos (length with)))))))
  txt)

(defun sanitize (txt)
  (let ((pos (position #\{ txt)))
    (when pos
      (setf txt (concatenate 'string (subseq txt 0 pos) "\\" (subseq txt pos)))))
  txt
  )
|#
;;; tcl -> lisp: puts "$x" mit \ und " escaped
;;;  puts [regsub {"} [regsub {\\} $x {\\\\}] {\"}]

;;; call to convert untility
(defun convert(from to)
  (close (do-execute "convert" (list from to) t)))

;;; table used for callback every callback consists of a name of a widget and
;;; a function to call

(defvar *callbacks* (make-hash-table :test #'equal))

(defvar *counter* 0)

(defun add-callback (sym fun)
  "create a callback sym is the name to use for storage, fun is the function to call"
  (when *debug-tk*
    (format t "add-callback (~A ~A)~%" sym fun))
  (setf (gethash sym *callbacks*) fun))

(defun callback (sym arg)
  "perform the call of the function associated with sym and the args arg"
  (let ((fun (gethash sym *callbacks*)))
    ;(format t "sym:~A fun:~A~%" sym fun)
    (force-output)
    (when fun
      (apply fun arg))))

;;; after <time> msec call function <fun> <label> is used to have
;;; several events scheduled at the same time

(defun after(time fun &optional label)
  (let ((name (format nil "after~@[~a~]" label)))
    (add-callback name fun)
    ;(format-wish "after ~a {puts -nonewline {(\"~A\") };flush stdout}" time name)))
    (format-wish "after ~a {callback ~A}" time name)))

;; tool functions used by the objects

;; incremental counter to create unique numbers

(defun get-counter()
  (incf *counter*))

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

(defvar *cursors*
  (list
   "X_cursor" "arrow" "based_arrow_down" "based_arrow_up" "boat" "bogosity"
   "bottom_left_corner" "bottom_right_corner" "bottom_side" "bottom_tee"
   "box_spiral" "center_ptr" "circle" "clock" "coffee_mug" "cross"
   "cross_reverse" "crosshair" "diamond_cross" "dot" "dotbox" "double_arrow"
   "draft_large" "draft_small" "draped_box" "exchange" "fleur" "gobbler"
   "gumby" "hand1" "hand2" "heart" "icon" "iron_cross" "left_ptr" "left_side"
   "left_tee" "leftbutton" "ll_angle" "lr_angle" "man" "middlebutton" "mouse"
   "pencil" "pirate" "plus" "question_arrow" "right_ptr" "right_side"
   "right_tee" "rightbutton" "rtl_logo" "sailboat" "sb_down_arrow"
   "sb_h_double_arrow" "sb_left_arrow" "sb_right_arrow" "sb_up_arrow"
   "sb_v_double_arrow" "shuttle" "sizing" "spider" "spraycan" "star"
   "target" "tcross" "top_left_arrow" "top_left_corner" "top_right_corner"
   "top_side" "top_tee" "trek" "ul_angle" "umbrella" "ur_angle" "watch" "xterm"))

(defun bell ()
  (send-wish (format nil "bell")))

(defun lower (widget &optional (other nil))
  (send-wish (format nil "lower ~a~@[ ~a~]" (path widget) (and other (path other)))))

(defun raise (widget &optional (other nil))
  (send-wish (format nil "raise ~a~@[ ~a~]" (path widget) (and other (path other)))))

(defun destroy (widget)
  (send-wish (format nil "destroy ~a" (path widget))))

(defun clipboard-clear ()
  (send-wish "clipboard clear"))

(defun clipboard-get ()
  ;(format-wish "esc [clipboard get]; flush stdout")
  (format-wish "senddatastring [clipboard get]")
  (read-data))

(defun clipboard-append (txt)
  (format-wish "clipboard append {~a}" txt))

;; basic tk object
(defclass tkobject ()
  ((name :accessor name :initarg :name :initform nil)
   ))

(defgeneric path (w))
(defmethod path ((w (eql nil))) nil)

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

(defgeneric create (w))

(defmethod create ((w widget))
  )


(defgeneric (setf command) (value widget))
(defgeneric command (widget))

(defmethod command ((widget widget))
  (gethash (name widget) *callbacks*))


(defstruct event
  x
  y
  keycode
  char
  )

(defgeneric bind (w event fun))
(defmethod bind ((w widget) event fun)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "bind  ~a ~a {sendevent ~A %x %y %k %K}" (path w) event name)))

(defvar *tk* (make-instance 'widget :name "." :path "."))

;;; generic functions

(defgeneric canvas (w))

(defgeneric value (widget)
  (:documentation "reads the value of the variable associated with the widget"))

(defclass tkvariable ()
  ())

;(defmethod init :after ((v tkvariable))

(defmethod initialize-instance :around ((v tkvariable) &key)
  (call-next-method)
  (format-wish "~a configure -variable ~a" (path v) (name v)))

(defmethod value ((v tkvariable))
  (format-wish "senddata $~a" (name v))
  (read-data))

(defgeneric (setf value) (widget val))
(defmethod (setf value) (val (v tkvariable))
  (format-wish "set ~a {~a}" (name v) val))

(defclass tktextvariable ()
  ())

(defgeneric text (widget)
  (:documentation "reads the value of the textvariable associated with the widget")
  )

(defmethod initialize-instance :around ((v tktextvariable) &key)
  (call-next-method)
  (format-wish "~a configure -textvariable text_~a" (path v) (name v)))

(defmethod text ((v tktextvariable))
  (format-wish "senddatastring $text_~a" (name v))
  (read-data))

(defgeneric (setf text) (val variable))

(defmethod (setf text) (val (v tktextvariable))
  (format-wish "set text_~a {~a}" (name v) val))

;;; window menu bar

(defclass menubar(widget)
  ())

(defun make-menubar(&optional (master nil))
 (make-instance 'menubar :master master :name "menubar"))

;(defmethod create ((mb menubar))
(defmethod initialize-instance :after ((mb menubar) &key)
  (format-wish "menu ~a -tearoff 0 -type menubar" (path mb))
  (format-wish "~a configure -menu ~a" (if (master mb)
					(path (master mb))
				      ".")
	    (path mb)))

;;; menues

(defclass menu(widget)
  ((text :accessor text :initarg :text)
   (help :accessor menu-help :initarg :help :initform nil)
   ))

;(defmethod create ((m menu))

(defmethod initialize-instance :after ((m menu) &key)
  (when (menu-help m) ;; special treatment for help menu
    (setf (name m) "help")
    (setf (slot-value m 'path) (create-path (master m) (name m))))
  (format-wish "menu ~A -tearoff 0" (path m))
  (format-wish "~A add cascade -label {~A} -menu ~a" (path (master m)) (text m) (path m)))

(defun make-menu(menu text)
  (make-instance 'menu :master menu :text text))

(defun add-separator (menu)
   (format-wish "~A add separator" (path menu)))

;;; menu button

(defclass menubutton(widget) 
  ((text :accessor text :initarg :text)
   ))

(defmethod initialize-instance :after ((m menubutton) &key command)
   (when command
     (add-callback (name m) command))
   (format-wish "~A add command -label {~A}  -command {callback ~A}" (path (master m)) (text m) (name m))
   )

(defun make-menubutton(menu text command)
  (let* ((mb (make-instance 'menubutton :master menu :text text :command command)))
    mb))

(defclass menucheckbutton(widget) 
  ((text :accessor text :initarg :text)
   (command :accessor command :initarg :command :initform nil)))

(defmethod initialize-instance :after ((m menucheckbutton) &key)
  (when (command m)
    (add-callback (name m) (command m)))
  (format-wish "~A add checkbutton -label {~A} -variable ~a ~@[ -command {callback ~a}~]"
	       (path (master m)) (text m) (name m) (and (command m) (name m)))
  )


(defmethod value ((cb menucheckbutton))
  (format-wish "senddata $~a" (name cb))
  (read-data))

(defmethod (setf value) (val (cb menucheckbutton))
  (format-wish "set ~a ~a" (name cb) val))

(defclass menuradiobutton(widget) 
  ((text :accessor text :initarg :text)
   (command :accessor command :initarg :command :initform nil)
   (group :accessor group :initarg :group :initform nil)))

(defmethod initialize-instance :after ((m menuradiobutton) &key)
  (when (command m)
    (add-callback (name m) (command m)))
  (unless (group m)
    (setf (group m)
	  (name m)))
      (format-wish "~A add radiobutton -label {~A} -value ~a -variable ~a ~@[ -command {callback ~a}~]"
	    (path (master m)) (text m) (name m) (group m)
	    (and (command m) (name m))))
   

(defmethod value ((cb menuradiobutton))
  (format-wish "senddata $~a" (group cb))
  (read-data))

(defmethod (setf value) (val (cb menuradiobutton))
  (format-wish "set ~a ~a" (group cb) val))


;;; standard button widget

(defclass button (tktextvariable widget)
  ((text :accessor button-text :initarg :text :initform "")
   ))

(defmethod initialize-instance :after ((bt button) &key (text "") activebackground command
				       activeforeground anchor background bitmap borderwidth cursor
				       disabledforeground font foreground highlightbackground
				       highlightcolor highlightthickness image justify padx pady
				       relief repeatdelay repeatinterval takefocus underline
				       wraplength compound default height overrelief state width)
  (when command (add-callback (name bt) command))
  (format-wish "button ~A -command {callback ~A} ~
                    -text {~A}~@[ -activebackground ~(~a~)~]~@[ -activeforeground ~(~a~)~]~
                     ~@[ -anchor ~(~a~)~]~@[ -background ~(~a~)~]~@[ -bitmap ~(~a~)~]~
                     ~@[ -borderwidth ~(~a~)~]~@[ -cursor ~(~a~)~]~@[ -disabledforeground ~(~a~)~]~
                     ~@[ -font {~a}~]~@[ -foreground ~(~a~)~]~@[ -highlightbackground ~(~a~)~]~
                     ~@[ -highlightcolor ~(~a~)~]~@[ -highlightthickness ~(~a~)~]~@[ -image ~(~a~)~]~
                     ~@[ -justify ~(~a~)~]~@[ -padx ~(~a~)~]~@[ -pady ~(~a~)~]~@[ -relief ~(~a~)~]~
                     ~@[ -repeatdelay ~(~a~)~]~@[ -repeatinterval ~(~a~)~]~@[ -takefocus ~(~a~)~]~
                     ~@[ -underline ~(~a~)~]~@[ -wraplength ~(~a~)~]~@[ -compound ~(~a~)~]~
                     ~@[ -default ~(~a~)~]~@[ -height ~(~a~)~]~@[ -overrelief ~(~a~)~]~
                     ~@[ -state ~(~a~)~]~@[ -width ~(~a~)~]"
	       (path bt) (name bt) text activebackground activeforeground anchor background
	       bitmap borderwidth cursor disabledforeground font foreground highlightbackground
	       highlightcolor highlightthickness image justify padx pady relief repeatdelay
	       repeatinterval takefocus
	       underline wraplength compound default height overrelief state width
	       ))

(defmethod (setf command) (val (button button))
  (add-callback (name button) val)
  (format-wish "~a configure -command {callback ~a}" (path button) (name button)))

(defun make-button (master text command)
  (let* ((b (make-instance 'button :master master :text text :command command)))
    b))

;;; check button widget

(defclass check-button (tktextvariable tkvariable widget)
  ((text :accessor cb-text :initarg :text :initform "")
   ))

(defmethod initialize-instance :after ((cb check-button) &key command)
  (format-wish "checkbutton ~A -text {~A} -variable ~A" (path cb) (cb-text cb) (name cb))
  (when command
    (setf (command cb) command)))

(defmethod (setf command) (val (check-button check-button))
  (add-callback (name check-button) val)
  (format-wish "~a configure -command {callback ~a}" (path check-button) (name check-button)))


;;; radio button widget

(defclass radio-button (widget)
  ((command :accessor command :initarg :command :initform nil)
   (text :accessor text :initarg :text :initform "")
   (val :accessor radio-button-value :initarg :value :initform nil)
   (var :accessor radio-button-variable :initarg :variable :initform nil)
   ))

(defmethod initialize-instance :after ((rb radio-button) &key)
  (format-wish "radiobutton ~A -text {~A}~@[ -value ~A~]~@[ -variable ~A~]~@[ -command {callback ~a}~]"
	    (path rb) (text rb)
	    (radio-button-value rb)
	    (radio-button-variable rb)
	    (and (command rb) (name rb))))

(defmethod value ((rb radio-button))
  "reads the content of the shared variable of the radio button set"
  (if (radio-button-variable rb)
      (progn
	(format-wish "senddata $~a" (radio-button-variable rb))
	(read-data)
	)
    nil))

(defmethod (setf value) (val (rb radio-button))
  "sets the content of the shared variable of the radio button set"
  (when (radio-button-variable rb)
    (format-wish "set ~a ~a" (radio-button-variable rb) val)))


;; text entry widget

(defclass entry (tktextvariable widget)
  ((width :accessor width :initarg :width :initform nil))
  )

(defmethod initialize-instance :after ((e entry) &key)
  (format-wish "entry ~A~@[ -width ~A~]" (path e) (width e)))

(defun make-entry (master)
  (make-instance 'entry :master master))

(defun entry-select (e from to)
  (format-wish "~a selection range ~a ~a" (path e) from to))

;;; frame widget 

(defclass frame(widget)  ())

(defmethod initialize-instance :after ((f frame) &key borderwidth relief)
   (format-wish "frame ~A~@[ -borderwidth ~a~]~@[ -relief ~(~a~)~]" (path f) borderwidth relief))

(defun make-frame (master)
  (make-instance 'frame :master master))

;;; labelframe widget 

(defclass labelframe(widget)
  ((text :accessor text :initarg :text :initform "")
   ))

(defmethod initialize-instance :after ((l labelframe) &key)  
  (format-wish "labelframe ~A -text {~A} " (path l) (text l)))

(defmethod (setf text) :after (val (l labelframe))
  (format-wish "~a configure -text {~a}" (path l) val))

;;; panedwindow widget

(defclass paned-window (widget)
  ((orient :accessor pane-orient :initarg :orient  :initform nil)))

(defmethod initialize-instance :after ((pw paned-window) &key)  
  (format-wish "panedwindow ~a~@[ -orient ~(~a~)~]" (path pw) (pane-orient pw)))

(defgeneric pane-configure (window option value))
(defmethod pane-configure ((pw paned-window) option value)
  (format-wish "~a paneconfigure ~a {~a}" (path pw) option value))

(defgeneric add-pane (window widget))
(defmethod add-pane ((pw paned-window) (w widget))
  (format-wish "~a add ~a" (path pw) (path w)))

(defgeneric forget-pane (window widget))
(defmethod forget-pane ((pw paned-window) (w widget))
  (format-wish "~a forget ~a" (path pw) (path w)))

;;; listbox widget

(defclass listbox (widget)
  ((width  :accessor width  :initarg :width  :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   ))

(defmethod initialize-instance :after ((l listbox) &key)
  (format-wish "listbox ~a~@[ -width ~a~]~@[ -height ~a~]"
	    (path l) (width l) (height l)))

(defmethod (setf command) (val (listbox listbox))
  (add-callback (name listbox) val)
  (format-wish "bind ~a <<ListboxSelect>> {callbackval ~a ([~a curselection])}" (path listbox) (name listbox)
	       (path listbox)))

(defgeneric listbox-append (l vals))
(defmethod listbox-append ((l listbox) values)
  "append values (which may be a list) to the list box"
  (if (listp values)
      (format-wish "~a insert end ~{ \{~a\}~}" (path l) values)
    (format-wish "~a insert end \{~a\}" (path l) values)))

(defgeneric listbox-get-selection (l))
(defmethod listbox-get-selection ((l listbox))
  (format-wish "puts -nonewline {(};puts -nonewline [~a curselection];puts {)};flush stdout" (path l))
  (read *wish*))

(defgeneric listbox-select (l val))
(defmethod listbox-select ((l listbox) val)
  "modify the selection in listbox, if nil is given, the selection is cleared,
if a number is given the corresponding element is selected, alternatively
a list of numbers may be given"
  (if (null val)
      (format-wish "~a selection clear 0 end" (path l))
    (if (listp val)
	(format-wish "~a selecttion set ~{ ~a~}" (path l) val)
      (format-wish "~a selecttion set ~a" (path l) val))))

(defgeneric listbox-clear (l))

(defmethod listbox-clear ((l listbox))
  (format-wish "~a delete 0 end" (path l)))

(defclass scrolled-listbox (frame)
  ((listbox :accessor listbox)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defmethod initialize-instance :after ((sl scrolled-listbox) &key)
  (setf (hscroll sl) (make-scrollbar sl :orientation "horizontal"))
  (setf (vscroll sl) (make-scrollbar sl))
  (setf (listbox sl) (make-instance 'listbox :master sl :xscroll (hscroll sl) :yscroll (vscroll sl)))
  (grid (listbox sl) 0 0 :sticky "news")
  (grid (hscroll sl) 1 0 :sticky "we")
  (grid (vscroll sl) 0 1 :sticky "ns")
  (grid-columnconfigure sl 0 :weight 1)
  (grid-columnconfigure sl 1 :weight 0)
  (grid-rowconfigure sl 0 :weight 1)
  (grid-rowconfigure sl 1 :weight 0)
 
  (configure (hscroll sl) "command" (concatenate 'string (path (listbox sl)) " xview"))
  (configure (vscroll sl) "command" (concatenate 'string (path (listbox sl)) " yview"))
  (configure (listbox sl) "xscrollcommand" (concatenate 'string (path (hscroll sl)) " set"))
  (configure (listbox sl) "yscrollcommand" (concatenate 'string (path (vscroll sl)) " set"))
  )

(defmethod listbox-append ((l scrolled-listbox) values)
  (listbox-append (listbox l) values))

(defmethod listbox-get-selection ((l scrolled-listbox))
  (listbox-get-selection (listbox l)))

(defmethod listbox-select ((l scrolled-listbox) val)
  (listbox-select (listbox l) val))

;;; scrolled-text

(defclass scrolled-text (frame)
  ((textbox :accessor textbox)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defmethod initialize-instance :after ((st scrolled-text) &key)
  (setf (hscroll st) (make-scrollbar st :orientation "horizontal"))
  (setf (vscroll st) (make-scrollbar st))
  (setf (textbox st) (make-instance 'text :master st :xscroll (hscroll st) :yscroll (vscroll st)))
  (grid (textbox st) 0 0 :sticky "news")
  (grid (hscroll st) 1 0 :sticky "we")
  (grid (vscroll st) 0 1 :sticky "ns")
  (grid-columnconfigure st 0 :weight 1)
  (grid-columnconfigure st 1 :weight 0)
  (grid-rowconfigure st 0 :weight 1)
  (grid-rowconfigure st 1 :weight 0)
 
  (configure (hscroll st) "command" (concatenate 'string (path (textbox st)) " xview"))
  (configure (vscroll st) "command" (concatenate 'string (path (textbox st)) " yview"))
  (configure (textbox st) "xscrollcommand" (concatenate 'string (path (hscroll st)) " set"))
  (configure (textbox st) "yscrollcommand" (concatenate 'string (path (vscroll st)) " set"))
  )

;;; scale widget

(defclass scale (widget)
  ())

(defmethod initialize-instance :after ((sc scale) &key from to orient command)
  (format-wish "scale ~a -variable ~a~@[ -from ~a~]~@[ -to ~a~]~@[ -orient ~(~a~)~]" (path sc) (name sc))
  (when command
    (setf (command sc) command)))q

(defmethod (setf command) (val (scale scale))
  (add-callback (name scale) val)					
  (format-wish "proc ~a-command {val} {callbackval ~a $val}" (name scale) (name scale))
  (format-wish "~a configure -command ~a-command" (path scale) (name scale)))
  

(defmethod value ((sc scale))
  (format-wish "senddata $~a" (name sc))
  (read-data))

(defmethod (setf value) (val (sc scale))
  (format-wish "set ~a ~a" (name sc) val))

;;; spinbox widget

(defclass spinbox (tktextvariable widget)
  ())

(defmethod initialize-instance :after ((sp spinbox) &key from to command)
  (format-wish "spinbox ~a~@[ -from ~a~]~@[ -to ~a~]" (path sp) from to)
  (when command
    (setf (command sp) command)))

(defmethod (setf command) (val (sp spinbox))
  (add-callback (name sp) val)					
  (format-wish "~a configure -command {callbackstring ~a %s}" (path sp) (name sp)))

;;; toplevel (window) widget 

(defclass toplevel (widget)
  ((protocol-destroy :accessor protocol-destroy :initarg :on-close :initform nil)
   ))

(defmethod initialize-instance :after ((w toplevel) &key)
  (format-wish "toplevel ~A" (path w))
  (unless (protocol-destroy w)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {wm withdraw ~a}" (path w) (path w))))

(defun make-toplevel (master)
  (make-instance 'toplevel :master master))

;;; label widget

(defclass label(tktextvariable widget)
  ((text :accessor label-text :initarg :text :initform "")
   ))

(defmethod initialize-instance :after ((l label) &key)
  (format-wish "label ~A -text {~A} " (path l) (label-text l)))

(defun make-label (master text)
  (make-instance 'label :master master  :text text))

;;; message widget

(defclass message (tktextvariable widget)
  ())

(defmethod initialize-instance :after ((m message) &key text aspect justify width)
  (format-wish "message ~A ~@[ -text {~A}~]~@[ -aspect ~A~]~@[ -justify ~(~A~)~]~@[ -width ~A~]"
	    (path m) text aspect justify width))

;;; scrollbar

(defclass scrollbar (widget)
  ((orientation :accessor orientation :initarg :orientation :initform "vertical")
   ))

(defun make-scrollbar(master &key (orientation "vertical"))
  (make-instance 'scrollbar :master master :orientation orientation))

(defmethod initialize-instance :after ((sb scrollbar) &key)
  (send-wish (format nil "scrollbar ~a -orient ~a" (path sb) (orientation sb))))

(defclass scrolled-canvas (frame)
  ((canvas :accessor canvas)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defun make-scrolled-canvas (master)
  (make-instance 'scrolled-canvas :master master ))

(defmethod create ((sc scrolled-canvas)))

(defmethod initialize-instance :after ((sc scrolled-canvas) &key)
  (setf (hscroll sc) (make-scrollbar sc :orientation "horizontal"))
  (setf (vscroll sc) (make-scrollbar sc))
  (setf (canvas sc) (make-canvas sc :xscroll (hscroll sc) :yscroll (vscroll sc)))
  (grid (canvas sc) 0 0 :sticky :news)
  (grid (hscroll sc) 1 0 :sticky :we)
  (grid (vscroll sc) 0 1 :sticky :ns)
  (grid-columnconfigure sc 0 :weight 1)
  (grid-columnconfigure sc 1 :weight 0)
  (grid-rowconfigure sc 0 :weight 1)
  (grid-rowconfigure sc 1 :weight 0)
 
  (configure (hscroll sc) "command" (concatenate 'string (path (canvas sc)) " xview"))
  (configure (vscroll sc) "command" (concatenate 'string (path (canvas sc)) " yview"))
  (configure (canvas sc) "xscrollcommand" (concatenate 'string (path (hscroll sc)) " set"))
  (configure (canvas sc) "yscrollcommand" (concatenate 'string (path (vscroll sc)) " set"))
  )


(defclass scrolled-frame (frame)
  ((inner :accessor interior)
   (displayframe :accessor scrolled-frame-display)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defmethod initialize-instance :after ((sf scrolled-frame) &key)
  (let ((f (make-instance 'frame :master sf)))
    (setf (scrolled-frame-display sf) f)
    (setf (interior sf) (make-instance 'frame :master f))
    (setf (hscroll sf) (make-instance 'scrollbar :master sf :orientation "horizontal"))
    (setf (vscroll sf) (make-instance 'scrollbar :master sf :orientation "vertical"))
    (grid f 0 0 :sticky "news")
    (grid (hscroll sf) 1 0 :sticky "we")
    (grid (vscroll sf) 0 1 :sticky "ns")
    (grid-columnconfigure sf 0 "weight" 1)
    (grid-columnconfigure sf 1 "weight" 0)
    (grid-rowconfigure sf 0 "weight" 1)
    (grid-rowconfigure sf 1 "weight" 0)
    (place (interior sf) 0 0)
    (send-wish (format nil "~a set  0.1 0.5" (path (hscroll sf))))
    (send-wish (format nil "~a set  0.1 0.5" (path (vscroll sf))))
    (send-wish (format nil "~a configure -command ~axv" (path (hscroll sf)) (name sf)))
    (send-wish (format nil "~a configure -command ~ayv" (path (vscroll sf)) (name sf)))
    (send-wish (format nil "
proc ~axv {{com moveto} {val 0} {unit 0}} {
set x [winfo x ~a]
set y [winfo y ~a]
set wx [winfo width ~a]
set w [winfo width ~a]
if {$val < 0} {set val 0}
if {$val > [expr 1.0*($wx-$w)/$wx]} {set val  [expr 1.0*($wx-$w)/$wx]}
place ~a -x [expr -($val * $wx)] -y $y
set x [winfo x ~a]
~a set [expr -1.0*$x/$wx] [expr 1.0*($w-$x)/$wx]
}
proc ~ayv {{com moveto} {val 0} {unit 0}} {
set x [winfo x ~a]
set y [winfo y ~a]
set wy [winfo height ~a]
set h [winfo height ~a]
if {$val < 0} {set val 0}
if {$val > [expr 1.0*($wy-$h)/$wy]} {set val  [expr 1.0*($wy-$h)/$wy]}
place ~a -x $x -y [expr -($val * $wy)]
set y [winfo y ~a]
~a set [expr -1.0*$y/$wy] [expr 1.0*($h-$y)/$wy]
}

"                   (name sf)
		    (path (interior sf))
		    (path (interior sf))
		    (path (interior sf))
		    (path f)
		    (path (interior sf))
		    (path (interior sf))		   
		    (path (hscroll sf))

		    (name sf)
		    (path (interior sf))
		    (path (interior sf))
		    (path (interior sf))
		    (path f)
		    (path (interior sf))
		    (path (interior sf))		   
		    (path (vscroll sf))
		    ))
    ))

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

(defmethod canvas ((canvas canvas)) canvas)

(defmethod initialize-instance :after ((c canvas) &key)
  (format-wish "canvas ~A~@[ -width ~A~]~@[ -height ~A~]" (path c)
	    (width c)
	    (height c)))

(defun make-canvas (master &key (width nil) (height nil) (xscroll nil) (yscroll nil))
  (make-instance 'canvas :master master :width width :height height :xscroll xscroll :yscroll yscroll))

(defgeneric scrollregion (c x0 y0 x1 y1))
(defmethod scrollregion ((c canvas) x0 y0 x1 y1)
  (setf (scrollregion-x0 c) x0)
  (setf (scrollregion-y0 c) y0)
  (setf (scrollregion-x1 c) x1)
  (setf (scrollregion-y1 c) y1)
  (configure c :scrollregion (format nil "~a ~a ~a ~a" x0 y0 x1 y1)))

;; canvas item functions

(defun create-line (canvas coords)
  (format-wish "senddata [~a create line~{ ~a~}]" (path canvas) coords)
  (read-data))

(defun create-line* (canvas &rest coords)
  (funcall #'create-line canvas coords))

(defun create-polygon (canvas coords)
  (format-wish "senddata [~a create polygon~{ ~a~}]" (path canvas) coords)
  (read-data))

(defun create-oval (canvas x0 y0 x1 y1)
  (format-wish "senddata [~a create oval ~a ~a ~a ~a]" (path canvas) x0 y0 x1 y1)
  (read-data))

(defun create-rectangle (canvas x0 y0 x1 y1)
  (format-wish "senddata [~a create rectangle ~a ~a ~a ~a]" (path canvas) x0 y0 x1 y1)
  (read-data))

(defun create-text (canvas x y text)
  (format-wish "senddata [~a create text ~a ~a -anchor nw -text {~a}]" (path canvas) x y text)
  (read-data))

(defun create-image (canvas x y &key image)
  (format-wish "senddata [~a create image ~a ~a -anchor nw~@[ -image ~a~]]" (path canvas) x y
	       (and image (name image)))
  (read-data))

(defun image-setpixel (image data x y &optional x2 y2 )
  (format-wish "~A put {~{{~:{#~2,'0X~2,'0X~2,'0X ~} } ~} } -to ~a ~a~@[ ~a~]~@[ ~a~]" (name image) data x y x2 y2))

(defun create-bitmap (canvas x y &key (bitmap nil))
  (format-wish "senddata [~a create image ~a ~a -anchor nw~@[ -bitmap ~a~]]" (path canvas) x y
	       (and bitmap (name bitmap)))
  (read-data))


(defun create-arc (canvas x0 y0 x1 y1 &key (start 0) (extent 180) (style "pieslice"))
  (format-wish "senddata [~a create arc ~a ~a ~a ~a -start ~a -extent ~a -style ~a]"
	       (path canvas) x0 y0 x1 y1 start extent style)
  (read-data))

(defun create-window (canvas x y widget)
  (format-wish "senddata [~a create window ~a ~a -anchor nw -window ~a]"
	       (path canvas) x y (path widget))
  (read-data))

(defun set-coords (canvas item coords)
  (format-wish "~a coords ~a~{ ~a~}" (path canvas) item coords))

(defun set-coords* (canvas item &rest coords)
  (funcall #'set-coords canvas item coords))

(defun postscript (canvas filename)
  (if (and (scrollregion-x0 canvas)
	   (scrollregion-x1 canvas)
	   (scrollregion-y0 canvas)
	   (scrollregion-y1 canvas))
      (format-wish "~a postscript -file ~a -x ~a -y ~a -width ~a -height ~a"
		(path canvas) filename
		(scrollregion-x0 canvas) (scrollregion-y0 canvas)
		(- (scrollregion-x1 canvas) (scrollregion-x0 canvas))
		(- (scrollregion-y1 canvas) (scrollregion-y0 canvas))
		)
    (format-wish "~a postscript -file ~a" (path canvas) filename)))

;;; text widget

(defclass text (widget)
  (;(width  :accessor width  :initarg :width  :initform nil)
   ;(height :accessor height :initarg :height :initform nil)
   (xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
  ))

(defmethod initialize-instance :after ((txt text) &key width height)
  (format-wish "text ~a~@[ -width ~a~]~@[ -height ~a~]" (path txt) width height))

(defun make-text (master &key (width nil) (height nil))
  (make-instance 'text :master master :width width :height height))

(defgeneric append-text (txt text &optional tag))
(defmethod append-text ((txt text) text &optional (tag nil))
  (format-wish "~a insert end {~a}~@[ ~(~a~)~]" (path txt) text tag))

(defgeneric clear-text (txt))
(defmethod clear-text ((txt text))
  (format-wish "~A delete 0.0 end" (path txt)))

(defgeneric see (txt pos))
(defmethod see((txt text) pos)
  (format-wish "~a see ~a" (path txt) pos))

(defgeneric tag-configure (txt tag option value))
(defmethod tag-configure ((txt text) tag option value)
  (format-wish "~a tag configure ~a -~(~a~) {~(~a~)}" (path txt)
	       (if (stringp tag)
		   tag
		 (format nil "~(~a~)" tag))
	       option value))

(defgeneric tag-bind (txt tag event fun))
(defmethod tag-bind ((txt text) tag event fun)
  "bind fun to event of the tag of the text widget txt"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a tag bind ~a ~a {callback ~A}" (path txt) tag event name)
    ))

(defmethod text ((text text))
  (format-wish "senddatastring [~a get 1.0 end]" (path text))
  (read-data))

(defmethod (setf text) (val (text text))
  (format-wish "~A delete 0.0 end;~A insert end {~A}" (path text) (path text) val))

(defgeneric save-text (txt filename))
(defmethod save-text ((txt text) filename)
  "save the content of the text widget into the file <filename>"
  (format-wish "set file [open {~a} \"w\"];puts $file [~a get 1.0 end];close $file;puts \"asdf\"" filename (path txt))
  (read-line *wish*)
  )

(defgeneric load-text (txt filename))
(defmethod load-text((txt text) filename)
  "load the content of the file <filename>"
  (format-wish "set file [open {~a} \"r\"];~a delete 1.0 end;~a insert end [read $file];close $file;puts \"asdf\"" filename (path txt) (path txt))
  (read-line *wish*))

;;; photo image object

(defclass photo-image(tkobject)
  ()
  )

;(defmethod create ((p photo-image))
(defmethod initialize-instance :after ((p photo-image) &key width height)
  (setf (name p) (create-name))
  (format-wish "image create photo ~A~@[ -width ~a~]~@[ -height ~a~]" (name p) width height))

(defun make-image ()
  (let* ((name (create-name))
	 (i (make-instance 'photo-image :name name)))
    (create i)
    i))

(defgeneric image-load (p filename))
(defmethod image-load((p photo-image) filename)
  ;(format t "loading file ~a~&" filename)
  (send-wish (format nil "~A read {~A} -shrink" (name p) filename))
  )

(defgeneric ishow (p name))
(defmethod ishow((p photo-image) name)
  (convert (concatenate 'string name ".jpg")
	   "ishow.ppm")
  (image-load p "ishow.ppm"))

;;;; generic methods on widgets

;;; pack method for widget arrangement in container

(defgeneric pack (w &key side fill expand after before padx pady ipadx ipady anchor))

(defmethod pack ((w widget) &key (side :top) (fill :none) expand after before padx pady ipadx ipady anchor)
  (cond ((stringp side)
         (warn "Using a string for the :SIDE parameter is deprecated."))
        ((stringp fill)
         (warn "Using a string for the :FILL parameter is deprecated.")))
  (format-wish "pack ~A -side ~(~A~) -fill ~(~A~)~@[~* -expand 1~]~
             ~@[ -after ~A~]~@[ -before ~A~]~@[ -padx ~A~]~
             ~@[ -pady ~A~]~@[ -ipadx ~A~]~@[ -ipady ~A~]~@[ -anchor ~(~A~)~]"
          (path w) side fill expand (and after (path after)) (and before (path before)) padx pady ipadx ipady anchor))


(defgeneric pack-forget (w))
(defmethod pack-forget ((w widget))
  (format-wish "pack forget ~A" (path w)))


;;; place manager

(defgeneric place (w x y &key width height))
(defmethod place (widget x y &key width height)
  (format-wish "place ~A -x ~A -y ~A~@[ -width ~a~]~@[ -height ~a~]" (path widget) x y width height))

;;; grid manager

(defgeneric grid (w r c &key columnspan ipadx ipady padx pady rowspan sticky))
(defmethod grid ((w widget) row column &key columnspan ipadx ipady padx pady rowspan sticky)
  (format-wish "grid ~a -row ~a -column ~a~@[ -columnspan ~a~]~@[ -ipadx ~a~]~
             ~@[ -ipady ~a~]~@[ -padx ~a~]~@[ -pady ~a~]~@[ -rowspan ~a~]~
             ~@[ -sticky ~(~a~)~]" (path w) row column columnspan ipadx ipady padx pady rowspan  sticky))

(defgeneric grid-columnconfigure (w c o v))
(defmethod grid-columnconfigure (widget column option value)
  (format-wish "grid columnconfigure ~a ~a -~(~a~) {~a}" (path widget) column option value))

(defgeneric grid-rowconfigure (w r o v))
(defmethod grid-rowconfigure (widget row option value)
  (format-wish "grid rowconfigure ~a ~a -~(~a~) {~a}" (path widget) row option value))

(defgeneric grid-configure (w o v))
(defmethod grid-configure (widget option value)
  (format-wish "grid configure ~a -~(~a~) {~a}" (path widget) option value))


;;; configure a widget parameter

(defgeneric configure (w o v &rest others))
(defmethod configure (widgt option value &rest others)
  ;(format t "normal config~&")
  (format-wish "~A configure -~(~A~) {~A} ~{ -~(~a~) {~(~a~)}~}" (path widgt) option 
	    (if (stringp value) ;; There may be values that need to be passed as
		value           ;; unmodified strings, so do not downcase strings
	      (format nil "~(~a~)" value)) ;; if its not a string, print it downcased (eg. symbols)
	    others

	    )) 
                                             ;; 

;;; for tkobjects, the name of the widget is taken
(defmethod configure (wid option (value tkobject) &rest others)
  (format-wish "~A configure -~(~A~) {~A} ~{ -~(~a~) {~(~a~)}~}" (path wid) option (path value) others))

(defgeneric cget (w o))
(defmethod cget ((widget widget) option)
  (format-wish "senddatastring [~a cget -~(~a~)]" (path widget) option)
  (read-data))

(defun background (widget)
  (cget widget :background))

#-:gcl
(defun (setf background) (val widget)
  (configure widget :background val))

#|
(defmacro defoption (option)
  `(progn
     (defun ,option (widget)
       (cget widget "asdf"))
     (export ,option)))

(defoption fill)
|#

(defgeneric itemconfigure (w i o v))
(defmethod itemconfigure ((widget canvas) item option value)
  (format-wish "~A itemconfigure ~A -~(~A~) {~A}" (path widget) item option
	    (if (stringp value) ;; There may be values that need to be passed as
		value           ;; unmodified strings, so do not downcase strings
	      (format nil "~(~a~)" value)))) ;; if its not a string, print it downcased


;;; for tkobjects, the name of the widget is taken
(defmethod itemconfigure ((widget canvas) item option (value tkobject))
  (format-wish "~A itemconfigure ~A -~(~A~) {~A}" (path widget) item option (path value)))

(defgeneric itemlower (w i &optional above))
(defmethod itemlower ((widget canvas) item &optional above)
  (format-wish "~A lower ~A ~@[~A~]" (path widget)
	       item above))

(defgeneric itemraise (w i &optional below))
(defmethod itemraise ((widget canvas) item &optional below)
  (format-wish "~A raise ~A ~@[~A~]" (path widget)
	       item below))


;;; wm functions

(defgeneric wm-title (w title))
(defmethod wm-title ((w widget) title)
  (format-wish "wm title ~a {~a}" (path w) title))

(defgeneric minsize (w x y))
(defmethod minsize ((w widget) x y)
  (format-wish "wm minsize ~a ~a ~a" (path w) x y))

(defgeneric maxsize (w x y))
(defmethod maxsize ((w widget) x y)
  (format-wish "wm maxsize ~a ~a ~a" (path w) x y))

(defgeneric withdraw (w))
(defmethod withdraw ((tl toplevel))
  (format-wish "wm withdraw ~a" (path tl)))

(defgeneric normalize (w))
(defmethod normalize ((tl toplevel))
  (format-wish "wm state ~a normal" (path tl)))

(defgeneric iconify (w))
(defmethod iconify ((tl toplevel))
  (format-wish "wm iconify ~a" (path tl)))

(defgeneric deiconify (w))
(defmethod deiconify ((tl toplevel))
  (format-wish "wm deiconify ~a" (path tl)))

(defgeneric geometry (w))
(defmethod geometry ((tl widget))
  (format-wish "senddatastring [wm geometry ~a]" (path tl))
  (read-data))

(defgeneric set-geometry (w width height x y))
(defmethod set-geometry ((tl widget) width height x y)
  (format-wish "wm geometry ~a ~ax~a+~a+~a" (path tl) width height x y))

(defgeneric on-close (w fun))
(defmethod on-close ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {callback ~A}" (path tl) name)))

(defgeneric on-focus (w fun))
(defmethod on-focus ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol WM_TAKE_FOCUS {callback ~A}"
	      name)))

(defun iconwindow (tl wid)
  (format-wish "wm iconwindow ~a ~a" (path tl) (path wid)))  

;;; winfo functions

(defun screen-width (&optional (w nil))
  "give the width of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenwidth ~a]" (if w (path w) "."))
  (read-data))

(defun screen-height (&optional (w nil))
  "give the height of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenheight ~a]" (if w (path w) "."))
  (read-data))

(defun screen-width-mm (&optional (w nil))
  "give the width of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenmmwidth ~a]" (if w (path w) "."))
  (read-data))

(defun screen-heigth-mm (&optional (w nil))
  "give the height of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenmmheigth ~a]" (if w (path w) "."))
  (read-data))

(defun screen-mouse-x (&optional (w nil))
  "give x position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo pointerx ~a]" (if w (path w) "."))
  (read-data))

(defun screen-mouse-y (&optional (w nil))
  "give y position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo pointery ~a]" (if w (path w) "."))
  (read-data))

(defun screen-mouse (&optional (w nil))
  "give the position of the mouse on screen as (x y) (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata \"([winfo pointerxy ~a])\"" (if w (path w) "."))
  (let ((vals (read-data)))
    (values (first vals) (second vals))))

(defun window-width (tl)
  "give the width of the toplevel in pixels"
  (format-wish "senddata [winfo width ~a]" (path tl))
  (read-data))

(defun window-height (tl)
  "give the height of the toplevel in pixels"
  (format-wish "senddata [winfo height ~a]" (path tl))
  (read-data))

(defun window-x (tl)
  "give the x position of the toplevel in pixels"
  (format-wish "senddata [winfo rootx ~a]" (path tl))
  (read-data))

(defun window-y (tl)
  "give the y position of the toplevel in pixels"
  (format-wish "senddata [winfo rooty ~a];flush stdout" (path tl))
  (read-data))

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
    (format-wish "senddatastring [tk_getOpenFile -filetypes ~a]"  files)
    (read-data)))

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
    (format-wish "senddatastring [tk_getSaveFile -filetypes ~a]"  files)
    (read-data)))

(defvar *mb-icons* (list "error" "info" "question" "warning")
  "icon names valid for message-box function")

;;; see make-string-output-string/get-output-stream-string
(defun message-box (message title type icon)
  ;;; tk_messageBox function
  (format-wish "senddata [tk_messageBox -message {~a} -title {~a} -type {~a} -icon {~a}]" message title type icon)
  (read-data))

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
    (send-wish (format nil "~A add separator" path)))
   ((listp (second tree))
    (let ((newpath (format nil "~A.~A" path (create-name))))
      (when (and (equal path ".menubar")
		 (or (equal (first tree) "Help")
		     (equal (first tree) "help")
		     (equal (first tree) "Hilfe")))
	(setf newpath ".menubar.help"))
      (send-wish (format nil "menu ~A -tearoff 0" newpath))
      (send-wish (format nil "~a add cascade -label \"~a\" -menu ~a" path (first tree) newpath))
      (dolist (entry (second tree))
	(cm entry newpath))))
   (t
    (let* ((name (create-name)))
      (add-callback name (second tree))		     
      (send-wish (format nil "~A add command -label {~A} -command {puts -nonewline  {(\"~A\")};flush stdout}" path (first tree) name))
      ))))

(defun create-menu2 (menutree)
  (send-wish (format nil "menu .menubar -tearoff 0 -type menubar"))
  (dolist (e menutree)
    (cm e ".menubar"))
  (send-wish (format nil ". configure -menu .menubar"))
  )  

;;;; main event loop, runs until stream is closed by wish (wish exited) or
;;;; the variable *exit-mainloop* is set

(defvar *exit-mainloop* nil)

(defun mainloop()
  (let ((*exit-mainloop* nil)
	(*read-eval* nil))    ;;safety against malicious clients
  (loop
    (let* ((l (read-event))) ;((l (read-preserving-whitespace *wish* nil nil)))
      (when (null l) (return))
      (if *debug-tk*
	  (format t "l:~s<=~%" l))
      (force-output)
      (if (listp l)
	  (cond  ((eq (first l) :callback)
		  (let ((params (rest l)))
					;(format t "Callback received~%") (force-output)
		    (callback (first params) (rest params))))
		 ((eq (first l) :event)
		  (let* ((params (rest l))
			 (callback (first params))
			 (evp (rest params))
			 (event (make-event :x (first evp) :y (second evp) :keycode (third evp) :char (fourth evp))))
		    (callback callback (list event))
		    ))
		 (t
		  (callback (first l) (rest l))))
	(progn
	  (princ l)
	  (force-output)
	))
;      (ignore-errors   (callback (first l) (rest l))	    )
;      (multiple-value-bind (erg cond)
;	  (ignore-errors
	    ;(callback (first l) (rest l))
;	    t)
					;(format t "erg:~a cond:~s<=" erg cond)
;	(if (not erg)
;	    (format t "error while executing callback:~s~&" cond)))
      (when *exit-mainloop*
	(send-wish "exit")
	(return))))))

;;; another way to terminate the running app, send exit command to wish

(defun exit-wish()
  (send-wish "exit"))

;;; wrapper macro - initializes everything, calls body and then mainloop
;;; since 
(defmacro with-ltk (&rest body)
  `(let ((*wish* nil)
	 (*callbacks* (make-hash-table :test #'equal))
	 (*counter* 1)
	 (*event-queue* nil))
     (start-wish)
     ,@body
     (mainloop)))
       

;;;; testing functions

(defvar *do-rotate* nil)
(defvar *demo-line* nil)
(defvar *demo-canvas* nil)

;;;; default ltk test
(defun ltktest()
  (with-ltk
   (let* ((bar (make-frame nil))
	  (fr (make-frame bar))
	  (lr (make-label fr "Rotation:"))
	  (bstart (make-button fr "Start" 'start-rotation))
	  (bstop  (make-button fr "Stop"  'stop-rotation))
	  (b1 (make-button bar "Hallo" (lambda ()
					 (format T "Hallo~%")
					 (force-output))))
	  (b2 (make-button bar "Welt!" (lambda ()
					 (format T "Welt~%")
					 (force-output))))
	  (f (make-frame bar))
	  (l (make-label f "Test:"))
	  (b3 (make-button f "Ok." 'test-rotation)); (setf *exit-mainloop* t))))
	  (e (make-entry bar))
	  (b4 (make-button bar "get!" (lambda ()
					(format T "content of entry:~A~%" (text e))
					(force-output))))
	  (b5 (make-button bar "set!" (lambda () (setf (text e) "test of set"))))
	  (sc (make-instance 'scrolled-canvas :borderwidth 2 :relief :raised)); :width 500 :height 500))
	  (c (canvas sc))
	  (lines nil)
	  mb mfile mf-load mf-save mf-export mfe-jpg mfe-gif mf-exit mf-print
	  )
     (setf mb (make-menubar))
     (setf mfile (make-menu mb "File"))
     (setf mf-load (make-menubutton mfile "Load" (lambda ()
						   (format t "Load pressed~&")
						   (force-output))))
     (setf mf-save (make-menubutton mfile "Save" (lambda ()
						   (format t "Save pressed~&")
						   (force-output))))
     (add-separator mfile)
     (setf mf-export (make-menu mfile "Export..."))
     (add-separator mfile)
     (setf mf-print (make-menubutton mfile "Print" (lambda () (postscript c "wt.ps"))))
     (add-separator mfile)
     (setf mfe-jpg (make-menubutton mf-export "jpeg" (lambda ()
						       (format t "Jpeg pressed~&")
						       (force-output))))
     (setf mfe-gif (make-menubutton mf-export "png" (lambda ()
						      (format t "Png pressed~&")
						      (force-output))))
     (setf mf-exit (make-menubutton mfile "Exit" (lambda () (setf *exit-mainloop* t))))

     (configure c :borderwidth 2)
     (configure c :relief :sunken)
     (pack sc :side :top :fill :both :expand t)
     (pack bar :side :bottom)
     (scrollregion c 0 0 500 400)
     (pack fr :side :left)
     (pack lr :side :left)
     (configure fr :borderwidth 2)
     (configure fr :relief :sunken)
     (pack bstart :side :left)
     (pack bstop :side :left)
     (pack b1 :side :left)
     (pack b2 :side :left)
     (configure f :borderwidth 2)
     (configure f :relief :sunken)
     (pack f :fill :x :side :left)
     (pack l :side :left)
     (pack b3 :side :left)
     (pack e :side :left)
     (pack b4 :side :left)
     (pack b5 :side :left)
     (dotimes (i 100)
       (let ((w (* i 2.8001)))
	 (let ((x (+ 250 (* 150.0 (sin w))))
	       (y (+ 200 (* 150.0 (cos w)))))
	   (push y lines)
	   (push x lines)
	   )))
     (setf *demo-line* (create-line c lines))
     (setf *demo-canvas* c)
     (create-text c 10 10 "Ltk Demonstration")
    )))

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
  (force-output)
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


;;;; the eyes :)

(defun ltk-eyes ()
  (with-ltk
   (let* ((*debug-tk* nil)
	  (w (screen-width))
	  (h (screen-height))
	  (c (make-instance 'canvas :width 400 :height 300))
	  (e1 (create-oval c 10 10 190 290))
	  (e2 (create-oval c 210 10 390 290))
	  (p1 (create-oval c 10 10 40 40))
	  (p2 (create-oval c 10 10 40 40))
	  (old-x 0)
	  (old-y 0))
     (setf *debug-tk* nil)
     (labels ((update ()
		      (multiple-value-bind (pos-x pos-y) (screen-mouse)
			(let* ((wx (window-x *tk*))
			       (wy (window-y *tk*))
			       (width (window-width *tk*))
			       (height (window-height *tk*))
			       (mx pos-x)
			       (my pos-y)
			       (x (truncate (* width (/ mx w))))
			       (y (truncate (* height (/ my h))))
			       (diam (truncate width 8))
			       (dx1 (- mx (+ wx (truncate width 4))))
			       (dy1 (- my (+ wy (truncate height 2))))
			       (dx2 (- mx (+ wx (* 3 (truncate width 4)))))
			       (dy2 (- my (+ wy (truncate height 2))))
			       (p1x (+ (- (truncate width 4)  (truncate diam 2)) (truncate (* width  dx1) (* 4.5 w))))
			       (p1y (+ (- (truncate height 2) (truncate diam 2)) (truncate (* height dy1) (* 2.3 h))))
			       (p2x (+ (- (* 3 (truncate width 4))  (truncate diam 2)) (truncate (*  width  dx2) (* 4.5 w))))
			       (p2y (+ (- (truncate height 2) (truncate diam 2)) (truncate (* height dy2) (* 2.3 h))))
			       
			       )
			  (setf *debug-tk* nil)
			  (unless (and (= x old-x)
				       (= y old-y))
			    (set-coords c e1 (list 10 10 (- (truncate width 2) 10) (- height 10)))
			    (set-coords c e2 (list (+ (truncate width 2) 10) 10  (- width 10) (- height 10)))
			    (set-coords c p1 (list p1x p1y (+ diam p1x) (+ diam p1y)))
			    (set-coords c p2 (list p2x p2y (+ diam p2x) (+ diam p2y)))
			    (setf old-x x
				  old-y y))
			  ))
			(after 100 #'update)))
     (pack c :expand 1 :fill :both)
     (itemconfigure c e1 "width" 10)
     (itemconfigure c e2 "width" 10)
     (itemconfigure c p1 "fill" "blue")
     (itemconfigure c p2 "fill" "blue")
     (after 100 #'update)
     ))))