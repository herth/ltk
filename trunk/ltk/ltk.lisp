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

#|
All tk commads as of version 8.4 with support information. "-" means not
supported by purpos (look comment), "x" means supported, though some uncommon
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
place                  geometry manager using coordinates
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
  (:export "TEST"
	   "*CURSORS*"
	   "*DEBUG-TK*"
	   "*EXIT-MAINLOOP*"
	   "*MB-ICONS*"
	   "*TK*"
	   "ADD-PANE"
	   "AFTER"
	   "APPEND-TEXT"
	   "ASK-OKCANCEL"
	   "ASK-YESNO"
	   "BELL"
	   "BIND"
	   "BUTTON"
	   "CANVAS"
	   "CHECK-BUTTON"
	   "CLEAR-TEXT"
	   "CLIPBOARD-APPEND"
	   "CLIPBOARD-CLEAR"
	   "CLIPBOARD-GET"
	   "CONFIGURE"
	   "CREATE-IMAGE"
	   "CREATE-LINE"
	   "CREATE-MENU2"
	   "CREATE-OVAL"
	   "CREATE-POLYGON"
	   "CREATE-TEXT"
	   "DEICONIFY"
	   "DESTROY"
	   "DO-EXECUTE"
	   "DO-MSG"
	   "ENTRY"
	   "EXIT-WISH"
	   "FORGET-PANE"
	   "FRAME"
	   "GEOMETRY"
	   "GET-CONTENT"
	   "GET-OPEN-FILE"
	   "GET-SAVE-FILE"
	   "GET-TEXT"
	   "GRID"
	   "GRID-COLUMNCONFIGURE"
	   "GRID-CONFIGURE"
	   "GRID-ROWCONFIGURE"
	   "ICONIFY"
	   "ICONWINDOW"
	   "IMAGE-LOAD"
	   "ITEMCONFIGURE"
	   "LABEL"
	   "LABELFRAME"
	   "LISTBOX"
	   "LISTBOX-APPEND"
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
	   "PHOTO-IMAGE"
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
	   "SCROLLED-LISTBOX"
	   "SCROLLREGION"
	   "SEE"
	   "SET-CONTENT"
	   "SET-COORDS"
	   "SET-GEOMETRY"
	   "SET-TEXT"
	   "SPINBOX"
	   "START-W"
	   "TAG-BIND"
	   "TAG-CONFIGURE"
	   "TEXT"
	   "TKOBJECT"
	   "TOPLEVEL"
	   "VALUE"
	   "WIDGET"
	   "WINDOW-HEIGHT"
	   "WINDOW-WIDTH"
	   "WINDOW-X"
	   "WINDOW-Y"
	   "WITH-LTK"
	   "WITHDRAW"
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
  (send-w (format nil "bell")))

(defun lower (widget &optional (other nil))
  (send-w (format nil "lower ~a ~a" (path widget)
		  (if other
		      (path other)
		    ""))))
(defun raise (widget &optional (other nil))
  (send-w (format nil "raise ~a ~a" (path widget)
		  (if other
		      (path other)
		    ""))))

(defun destroy (widget)
  (send-w (format nil "destroy ~a" (path widget))))


(defun clipboard-clear ()
  (send-w "clipboard clear"))

(defun clipboard-get ()
  (send-w (format nil "puts [clipboard get]; flush stdout"))
  (let ((c (read-all *w*)))
    c)
  )

(defun clipboard-append (txt)
  (send-w (format nil "clipboard append {~a}" txt)))


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

(defgeneric create (w))
(defmethod create ((w widget))
  )

(defgeneric bind (w event fun))
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

;;; check button widget

(defclass check-button (widget)
  ((command :accessor command :initarg :command :initform nil)
   (text :accessor text :initarg :text :initform "")
   ))

(defmethod create ((cb check-button))
  (if (command cb)
      (progn
	(add-callback (name cb) (command cb))
	(send-w (format nil "checkbutton ~A -text {~A} -variable ~A -command {puts -nonewline {(\"~A\")};flush stdout}" (path cb) (text cb) (name cb) (name cb))))
    (send-w (format nil "checkbutton ~A -text {~A} -variable ~A" (path cb) (text cb) (name cb))))
  (setf (created cb) t))

(defgeneric value (widget)
  (:documentation "reads the value of the variable associated with the widget"))

(defmethod value ((cb check-button))
  (send-w (format nil "puts $~a;flush stdout" (name cb)))
  (read *w* nil nil))

(defgeneric (setf value) (widget val))
(defmethod (setf value) (val (cb check-button))
  (send-w (format nil "set ~a ~a" (name cb) val)))


;;; radio button widget

(defclass radio-button (widget)
  ((command :accessor command :initarg :command :initform nil)
   (text :accessor text :initarg :text :initform "")
   (val :accessor radio-button-value :initarg :value :initform nil)
   (var :accessor radio-button-variable :initarg :variable :initform nil)
   ))

(defmethod create ((rb radio-button))
  (send-w (format nil "radiobutton ~A -text {~A} ~A ~A ~A"
		  (path rb) (text rb)
		  (if (radio-button-value rb)
		      (format nil "-value {~a}" (radio-button-value rb))
		    "")
		  (if (radio-button-variable rb)
		      (format nil "-variable {~a}" (radio-button-variable rb))
		    "")		 
		  (if (command rb)
		      (progn
			(add-callback (name rb) (command rb))
			(format nil "-command {puts -nonewline {(\"~A\")};flush stdout}" (name rb)))
		    "")))
  (setf (created rb) t))

(defmethod value ((rb radio-button))
  "reads the content of the shared variable of the radio button set"
  (if (radio-button-variable rb)
      (progn
	(send-w (format nil "puts $~a;flush stdout" (radio-button-variable rb)))
	(read *w*))
    nil))

(defmethod (setf value) (val (rb radio-button))
  "sets the content of the shared variable of the radio button set"
  (when (radio-button-variable rb)
    (send-w (format nil "set ~a ~a" (radio-button-variable rb) val))))


;; text entry widget

(defclass entry(widget)
  ((width :accessor width :initarg :width :initform nil))
  )

(defmethod create ((e entry))
  (send-w (format nil "entry ~A ~A" (path e)
		  (if (width e)
		      (format nil "-width ~a" (width e))
		    "")))
  (setf (created e) t))

(defun make-entry (master)
  (make-instance 'entry :master master))

(defgeneric get-content (e))
(defmethod get-content ((e entry))  
  (send-w (format nil "puts [~A get]; flush stdout" (path e)))
  ;#+:sbcl (read-line *w*)
  ;#+:lispworks (read-line *w*)
  (let ((c (do-read-line)))
    c)
  )

(defgeneric set-content (e txt))
(defmethod set-content ((e entry) txt)
  (send-w (format nil "~A delete 0 end;~A insert end {~A}" (path e) (path e) txt)))


(defun entry-select (e from to)
  (send-w (format nil "~a selection range ~a ~a" (path e) from to)))

;;; frame widget 

(defclass frame(widget)  ())

(defmethod create ((f frame))
  (send-w (format nil "frame ~A " (path f)))
  (setf (created f) t))

(defun make-frame (master)
  (make-instance 'frame :master master))

;;; labelframe widget 

(defclass labelframe(widget)
  ((text :accessor text :initarg :text :initform "")
   ))

(defmethod create ((l labelframe))
  (send-w (format nil "labelframe ~A -text {~A} " (path l) (text l)))
  (setf (created l) t))

;;; panedwindow widget

(defclass paned-window (widget)
  ())

(defmethod create ((pw paned-window))
  (send-w (format nil "panedwindow ~a" (path pw)))
  (setf (created pw) t))

(defgeneric pane-configure (window option value))
(defmethod pane-configure ((pw paned-window) option value)
  (send-w (format nil "~a paneconfigure ~a {~a}" (path pw) option value))
  )

(defgeneric add-pane (window widget))
(defmethod add-pane ((pw paned-window) (w widget))
  (send-w (format nil "~a add ~a" (path pw) (path w))))

(defgeneric forget-pane (window widget))
(defmethod forget-pane ((pw paned-window) (w widget))
  (send-w (format nil "~a forget ~a" (path pw) (path w))))

;;; listbox widget

(defclass listbox (widget)
  ((width  :accessor width  :initarg :width  :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   ))

(defmethod create ((l listbox))
  (send-w (format nil "listbox ~a~a~a" (path l)
		  (if (width l)
		      (format nil " -width ~a" (width l))
		    "")
		  (if (height l)
		      (format nil " -height ~a" (height l))
		    "")
		  ))
  (setf (created l) t))


(defgeneric listbox-append (l vals))
(defmethod listbox-append ((l listbox) values)
  "append values (which may be a list) to the list box"
  (if (listp values)
      (send-w (format nil "~a insert end ~{ \{~a\}~}" (path l) values))
    (send-w (format nil "~a insert end \{~a\}" (path l) values))))

(defgeneric listbox-get-selection (l))
(defmethod listbox-get-selection ((l listbox))
  (send-w (format nil "puts -nonewline {(};puts -nonewline [~a curselection];puts {)};flush stdout" (path l)))
  (read *w*)
  )

(defgeneric listbox-select (l val))
(defmethod listbox-select ((l listbox) val)
  "modify the selection in listbox, if nil is given, the selection is cleared,
if a number is given the corresponding element is selected, alternatively
a list of numbers may be given"
  (if (null val)
      (send-w (format nil "~a selection clear 0 end" (path l)))
    (if (listp val)
	(send-w (format nil "~a selecttion set ~{ ~a~}" (path l) val))
      (send-w (format nil "~a selecttion set ~a" (path l) val)))))



(defclass scrolled-listbox (frame)
  ((listbox :accessor listbox)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defmethod create ((sl scrolled-listbox))
  (call-next-method)
  (setf (hscroll sl) (make-scrollbar sl :orientation "horizontal"))
  (setf (vscroll sl) (make-scrollbar sl))
  (setf (listbox sl) (make-instance 'listbox :master sl :xscroll (hscroll sl) :yscroll (vscroll sl)))
  (grid (listbox sl) 0 0 :sticky "news")
  (grid (hscroll sl) 1 0 :sticky "we")
  (grid (vscroll sl) 0 1 :sticky "ns")
  (grid-columnconfigure sl 0 "weight" 1)
  (grid-columnconfigure sl 1 "weight" 0)
  (grid-rowconfigure sl 0 "weight" 1)
  (grid-rowconfigure sl 1 "weight" 0)
 
  (configure (hscroll sl) "command" (concatenate 'string (path (listbox sl)) " xview"))
  (configure (vscroll sl) "command" (concatenate 'string (path (listbox sl)) " yview"))
  (configure (listbox sl) "xscrollcommand" (concatenate 'string (path (hscroll sl)) " set"))
  (configure (listbox sl) "yscrollcommand" (concatenate 'string (path (vscroll sl)) " set"))
  (setf (created sl) t)
  )

(defmethod listbox-append ((l scrolled-listbox) values)
  (listbox-append (listbox l) values))

(defmethod listbox-get-selection ((l scrolled-listbox))
  (listbox-get-selection (listbox l)))

(defmethod listbox-select ((l scrolled-listbox) val)
  (listbox-select (listbox l) val))

  
;;; scale widget

(defclass scale (widget)
  ((from :accessor scale-from :initarg :from  :initform nil)
   (to :accessor scale-to :initarg :to :initform nil)
   (orient :accessor scale-orient :initarg :orient  :initform nil))
  )

(defmethod create ((sc scale))
  (send-w (format nil "scale ~a -variable ~a ~a~a~a" (path sc) (name sc)
		  (if (scale-from sc)
		      (format nil " -from ~a" (scale-from sc))
		    "")
		  (if (scale-to sc)
		      (format nil " -to ~a" (scale-to sc))
		    "")
		  (if (scale-orient sc)
		      (format nil " -orient ~a" (scale-orient sc))
		    "")
		  ))
  (setf (created sc) t))


(defmethod value ((sc scale))
  (send-w (format nil "puts $~a;flush stdout" (name sc)))
  (read *w* nil nil))

(defmethod (setf value) (val (sc scale))
  (send-w (format nil "set ~a ~a" (name sc) val)))

;;; spinbox widget

(defclass spinbox (widget)
  ((from :accessor spinbox-from :initarg :from  :initform nil)
   (to :accessor spinbox-to :initarg :to :initform nil)
   )
  )

(defmethod create ((sp spinbox))
  (send-w (format nil "spinbox ~a -textvariable ~a ~a~a" (path sp) (name sp)
		  (if (spinbox-from sp)
		      (format nil " -from ~a" (spinbox-from sp))
		    "")
		  (if (spinbox-to sp)
		      (format nil " -to ~a" (spinbox-to sp))
		    "")
		  ))
  (setf (created sp) t))


(defmethod value ((sp spinbox))
  (send-w (format nil "puts $~a;flush stdout" (name sp)))
  (read *w* nil nil))

(defmethod (setf value) (val (sp spinbox))
  (send-w (format nil "set ~a ~a" (name sp) val)))

;;; toplevel (window) widget 

(defclass toplevel (widget)
  ((protocol-destroy :accessor protocol-destroy :initarg :on-close :initform nil)
   ))

(defmethod create ((w toplevel))
  (send-w (format nil "toplevel ~A" (path w)))
  (unless (protocol-destroy w)
    (send-w (format nil "wm protocol ~a WM_DELETE_WINDOW {wm withdraw ~a}" (path w) (path w))))
      
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

;;; message widget

(defclass message (widget)
  ((text    :accessor text            :initarg :text    :initform "")
   (aspect  :accessor message-aspect  :initarg :aspect  :initform nil)
   (justify :accessor message-justify :initarg :justify :initform nil)
   (width   :accessor message-width   :initarg :width   :initform nil)
   ))

(defmethod create ((m message))
  (send-w (format nil "message ~A -text {~A} ~A~A~A"
		  (path m) (text m)
		  (if (message-aspect m)
		      (format nil " -aspect {~a}" (message-aspect m))
		    "")
		  (if (message-justify m)
		      (format nil " -justify {~a}" (message-justify m))
		    "")
		  (if (message-width m)
		      (format nil " -width {~a}" (message-width m))
		    "")))
  (setf (created m) t)
  )


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

(defgeneric scrollregion (c x0 y0 x1 y1))
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

(defun create-oval (canvas x0 y0 x1 y1)
  (send-w (format nil "puts [~a create oval ~a ~a ~a ~a]" (path canvas) x0 y0 x1 y1))
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

(defgeneric append-text (txt text &optional tag))
(defmethod append-text ((txt text) text &optional (tag nil))
  (send-w (format nil "~a insert end {~a} ~a" (path txt) text (if tag
								  tag
								""))))
(defgeneric clear-text (txt))
(defmethod clear-text ((txt text))
  (send-w (format nil "~A delete 0.0 end" (path txt))))

(defgeneric set-text (txt content))
(defmethod set-text ((txt text) content)
  (send-w (format nil "~A delete 0.0 end;~A insert end {~A}" (path txt) (path txt) content)))

(defgeneric see (txt pos))
(defmethod see((txt text) pos)
  (send-w (format nil "~a see ~a" (path txt) pos)))

(defgeneric tag-configure (txt tag option value))
(defmethod tag-configure ((txt text) tag option value)
  (send-w (format nil "~a tag configure ~a -~a {~a}" (path txt) tag option value)))

(defgeneric tag-bind (txt tag event fun))
(defmethod tag-bind ((txt text) tag event fun)
  "bind fun to event of the tag of the text widget txt"
  (let ((name (create-name)))
    (add-callback name fun)
    (send-w (format nil "~a tag bind ~a ~a {puts -nonewline {(\"~A\")};flush stdout}"
		    (path txt) tag event name))))

(defgeneric get-text (txt))
(defmethod get-text((txt text))
  (send-w (format nil "set file [open \"/tmp/ltk\" \"w\"];puts $file [~a get 1.0 end];close $file;puts \"asdf\"" (path txt)))
  (read-line *w*)
  (let (erg)
    (with-open-file (stream "/tmp/ltk" :direction :input)
      (setf erg (read-all stream)))
    (delete-file "/tmp/ltk")
    erg)
  )

(defgeneric save-text (txt filename))
(defmethod save-text ((txt text) filename)
  "save the content of the text widget into the file <filename>"
  (send-w (format nil "set file [open {~a} \"w\"];puts $file [~a get 1.0 end];close $file;puts \"asdf\"" filename (path txt)))
  (read-line *w*)
  )

(defgeneric load-text (txt filename))
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

(defgeneric image-load (p filename))
(defmethod image-load((p photo-image) filename)
  ;(format t "loading file ~a~&" filename)
  (send-w (format nil "~A read {~A} -shrink" (name p) filename))
  )

(defgeneric ishow (p name))
(defmethod ishow((p photo-image) name)
  (convert (concatenate 'string name ".jpg")
	   "ishow.ppm")
  (image-load p "ishow.ppm"))

;;;; generic methods on widgets

;;; pack method for widget arrangement in container

(defgeneric pack (w &key side fill expand))
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

(defgeneric pack-forget (w))
(defmethod pack-forget ((w widget))
  (when (created w)
    (send-w (format nil "pack forget ~A" (path w)))))



;;; grid manager

(defgeneric grid (w r c &key sticky))
(defmethod grid ((w widget) row column &key (sticky nil))
  (send-w (format nil "grid ~a -row ~a -column ~a ~a" (path w) row column
		  (if sticky
		      (format nil " -sticky ~a" sticky)
		    ""))))

(defgeneric grid-columnconfigure (w c o v))
(defmethod grid-columnconfigure (widget column option value)
  (send-w (format nil "grid columnconfigure ~a ~a -~a {~a}" (path widget) column option value)))

(defgeneric grid-rowconfigure (w r o v))
(defmethod grid-rowconfigure (widget row option value)
  (send-w (format nil "grid rowconfigure ~a ~a -~a {~a}" (path widget) row option value)))

(defgeneric grid-configure (w o v))
(defmethod grid-configure (widget option value)
  (send-w (format nil "grid configure ~a -~a {~a}" (path widget) option value)))


;;; configure a widget parameter

(defgeneric configure (w o v))
(defmethod configure (widgt option value)
  ;(format t "normal config~&")
  (send-w (format nil "~A configure -~A {~A}" (path widgt) option value)))

;;; for tkobjects, the name of the widget is taken
(defmethod configure (wid option (value tkobject))
  (send-w (format nil "~A configure -~A {~A}" (path wid) option (name value))))

(defgeneric itemconfigure (w i o v))
(defmethod itemconfigure ((widget canvas) item option value)
  (send-w (format nil "~A itemconfigure ~A -~A {~A}" (path widget) item option value)))

;;; for tkobjects, the name of the widget is taken
(defmethod itemconfigure ((widget canvas) item option (value tkobject))
  (send-w (format nil "~A itemconfigure ~A -~A {~A}" (path widget) item option (name value))))


;;; wm functions

(defgeneric wm-title (w title))
(defmethod wm-title ((w widget) title)
  (send-w (format nil "wm title ~a {~a}" (path w) title)))

(defgeneric minsize (w x y))
(defmethod minsize ((w widget) x y)
  (send-w (format nil "wm minsize ~a ~a ~a" (path w) x y)))

(defgeneric maxsize (w x y))
(defmethod maxsize ((w widget) x y)
  (send-w (format nil "wm maxsize ~a ~a ~a" (path w) x y)))

(defgeneric withdraw (w))
(defmethod withdraw ((tl toplevel))
  (send-w (format nil "wm withdraw ~a" (path tl))))

(defgeneric normalize (w))
(defmethod normalize ((tl toplevel))
  (send-w (format nil "wm state ~a normal" (path tl))))

(defgeneric iconify (w))
(defmethod iconify ((tl toplevel))
  (send-w (format nil "wm iconify ~a" (path tl))))

(defgeneric deiconify (w))
(defmethod deiconify ((tl toplevel))
  (send-w (format nil "wm deiconify ~a" (path tl))))

(defgeneric geometry (w))
(defmethod geometry ((tl toplevel))
  (send-w (format nil "wm geometry ~a" (path tl)))
  (do-read-line))

(defgeneric set-geometry (w width height x y))
(defmethod set-geometry ((tl toplevel) width height x y)
  (send-w (format nil "wm geometry ~a ~ax~a+~a+~a" (path tl) width height x y)))
  ;(do-read-line))

(defgeneric on-close (w fun))
(defmethod on-close ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (send-w (format nil
		    "wm protocol WM_DELETE_WINDOW {puts -nonewline {(\"~A\")};flush stdout}"
		    name))))

(defgeneric on-focus (w fun))
(defmethod on-focus ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (send-w (format nil
		    "wm protocol WM_TAKE_FOCUS {puts -nonewline {(\"~A\")};flush stdout}"
		    name))))

(defun iconwindow (tl wid)
  (send-w (format nil "wm iconwindow ~a ~a" (path tl) (path wid))))
  

;;; winfo functions

(defun screen-width (&optional (w nil))
  "give the width of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (send-w (format nil "puts [winfo screenwidth ~a]" (if w (path w) ".")))
  (read *w* nil nil))

(defun screen-height (&optional (w nil))
  "give the height of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (send-w (format nil "puts [winfo screenheight ~a]" (if w (path w) ".")))
  (read *w* nil nil))

(defun screen-width-mm (&optional (w nil))
  "give the width of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (send-w (format nil "puts [winfo screenmmwidth ~a]" (if w (path w) ".")))
  (read *w* nil nil))

(defun screen-heigth-mm (&optional (w nil))
  "give the height of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (send-w (format nil "puts [winfo screenmmheigth ~a]" (if w (path w) ".")))
  (read *w* nil nil))

(defun screen-mouse-x (&optional (w nil))
  "give x position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (send-w (format nil "puts [winfo pointerx ~a]" (if w (path w) ".")))
  (read *w* nil nil))

(defun screen-mouse-y (&optional (w nil))
  "give y position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (send-w (format nil "puts [winfo pointery ~a]" (if w (path w) ".")))
  (read *w* nil nil))

(defun screen-mouse (&optional (w nil))
  "give the position of the mouse on screen as (x y) (if w is given, of the screen the widget w is displayed on)"
  (send-w (format nil "puts -nonewline {(};puts -nonewline [winfo pointerxy ~a];puts {)}" (if w (path w) ".")))
  (read *w* nil nil))

(defun window-width (tl)
  "give the width of the toplevel in pixels"
  (send-w (format nil "puts [winfo width ~a]" (path tl)))
  (read *w* nil nil))

(defun window-height (tl)
  "give the height of the toplevel in pixels"
  (send-w (format nil "puts [winfo height ~a]" (path tl)))
  (read *w* nil nil))

(defun window-x (tl)
  "give the x position of the toplevel in pixels"
  (send-w (format nil "puts [winfo rootx ~a]" (path tl)))
  (read *w* nil nil))

(defun window-y (tl)
  "give the y position of the toplevel in pixels"
  (send-w (format nil "puts [winfo rooty ~a]" (path tl)))
  (read *w* nil nil))



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

(defvar *mb-icons* (list "error" "info" "question" "warning")
  "icon names valid for message-box function")

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
  (let ((*exit-mainloop* nil)
	(*read-eval* nil))    ;;safety against malicious clients
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


;;;; default ltk test

(defun test()
  (wt)
;  (read-all *w*)
  (mainloop))

;;;; radio button test

(defun rbtest ()
  (with-ltk
   (let* ((buttons nil))
     (dotimes (i 20)
       (push (make-instance 'radio-button
			    :text (format nil "Radio ~a" i)
			    :variable "radios"
			    :value (format nil "R~a" i)) buttons))
     (setf buttons (nreverse buttons))
     (dolist (b buttons)
       (pack b :side "top"))
     (setf (value (first buttons)) "R3")
     )))


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
		      (let* ((pos (screen-mouse))
			     (wx (window-x *tk*))
			     (wy (window-y *tk*))
			     (width (window-width *tk*))
			     (height (window-height *tk*))
			     (mx (first pos))
			     (my (second pos))
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
			)
		      (after 100 #'update)))
     (pack c :expand 1 :fill "both")
     (itemconfigure c e1 "width" 10)
     (itemconfigure c e2 "width" 10)
     (itemconfigure c p1 "fill" "blue")
     (itemconfigure c p2 "fill" "blue")
     (after 100 #'update)
     ))))
	    
