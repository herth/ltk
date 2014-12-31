#|

 This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
 Portions Copyright (c) 2005-2010 Thomas F. Burdick
 Portions Copyright (c) 2006-2010 Cadence Design Systems
 Portions Copyright (c) 2010 Daniel Herring

 The authors grant you the rights to distribute
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
All tk commands as of version 8.4 with support information. "-" means not
supported by purpose (look comment), "x" means supported, though some
options may not be supported. 

command      supported comment
bell                 x
bind                 x 
bindtags               modify the tag list of a widget that describes which events it gets
bitmap               - see image
button               x
canvas               x 
checkbutton          x
clipboard            x (canvas get missing... tricky...)
colors               - constants only
console              - only on some platforms
cursors              x 
destroy              x
entry                x
event                  create and manage virtual events
focus                x focus management functions
font
frame                x
grab                  
grid                 x
image                x 
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
options              - only helpfile
pack                 x
panedwindow          x
photo                x 
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
tk_bisque            - only for tk backwards compatibility
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


support of all config args as keywords to make-instance:

bitmap               
button               x
canvas               x 
checkbutton          x
entry                x
frame                x
image                 
label                x 
labelframe           x 
listbox              x 
menu                  
menubutton            
message                
panedwindow          x
photo                  
radiobutton          x
scale                x
scrollbar            x 
spinbox              x 
text                 x
toplevel             x

|#


(defpackage :ltk
  (:use :common-lisp
        #+(or :cmu :scl) :ext
        #+:sbcl :sb-ext
        )
  (:shadow #+:sbcl #:exit)
  (:export #:ltktest                           
           #:*ltk-version*
           #:*cursors*
           #:*debug-tk*
           #:*debug-buffers*
           #:*break-mainloop*
           #:*exit-mainloop*
           #:*init-wish-hook*
           #:*mb-icons*
           #:*ltk-debug*
           #:*tk*
           #:*wish*
           #:wish-stream
           #:wish-variable
           #:wish-variables
           #:*wish-args*
           #:*wish-pathname*
           #:*default-ltk-debugger*
           #:add-pane
           #:add-separator
           #:after
           #:after-cancel
           #:after-idle
           #:append-text
           #:append-newline
           #:ask-okcancel
           #:ask-yesno
           #:background
           #:bbox
           #:bell
           #:bind
           #:button
           #:calc-scroll-region
           #:canvas
           #:canvas-line
           #:canvas-oval
           #:canvas-polygon
           #:canvas-rectangle
           #:canvas-text
           #:canvas-image
           #:canvas-item
           #:canvas-arc
           #:canvas-bbox
           #:canvas-window
           #:canvasx
           #:canvasy
           #:cget
           #:check-button
           #:choose-color
           #:choose-directory
           #:clear-text
           #:clear
           #:clipboard-append
           #:clipboard-clear
           #:clipboard-get
           #-:tk84
           #:combobox
           #:command
           #:coords
           #:configure
           #:create-arc
           #:create-bitmap
           #:create-image
           #:create-line
           #:create-line*
           #:create-menu2
           #:create-oval
           #:create-polygon
           #:create-rectangle
           #:create-text
           #:create-window
           #:*debug-settings-table*
           #:defargs
           #:deiconify
           #:destroy
           #:do-execute
           #:do-msg
           #:entry
           #:entry-select
           #:exit-wish
           #:event
           #:event-x
           #:event-y
           #:event-keycode
           #:event-char
           #:event-mouse-button
           #:event-root-x
           #:event-root-y
           #:event-width
           #:event-height
           #:focus
           #:font-configure
           #:font-create
           #:font-delete
           #:font-metrics
           #:force-focus
           #:forget-pane
           #:format-wish
           #:frame
           #:geometry
           #:get-open-file
           #:get-save-file
           #:grab
           #:grab-release
           #:grid
           #:grid-columnconfigure
           #:grid-configure
           #:grid-forget
           #:grid-rowconfigure
           #:hscroll
           #:iconify
           #:iconwindow
           #:image-load
           #:image-setpixel
           #:cursor-index
           #:input-box
           #:insert-object
           #:interior
           #:itembind
           #:itemconfigure
           #:itemdelete
           #:itemmove
           #:itemlower
           #:itemraise
           #:label
           #:labelframe
           #:listbox
           #:listbox-append
           #:listbox-clear
           #:listbox-delete
           #:listbox-configure
           #:listbox-get-selection
           #:listbox-nearest
           #:listbox-select
           #:load-text
           #:lower
           #:mainloop
           #:make-items
           #:create-items
           #:make-canvas
           #:make-frame
           #:make-image
           #:make-label
           #:make-menu
           #:make-menubar
           #:make-menubutton
           #:make-scrollbar
           #:make-scrolled-canvas
           #:make-text
           #:make-toplevel
           #:make-line
           #:make-oval
           #:make-polygon
           #:make-rectangle
           #:master
           #:maxsize
           #:menu
           #:menubar
           #:menubutton
           #:menucheckbutton
           #:menu-delete
           #:menuradiobutton
           #:message
           #:message-box
           #:minsize
           #:move
           #:move-all
           #:normalize
           #-:tk84
           #:notebook
           #:on-close
           #:on-focus
           #:pack
           #:pack-forget
           #:pack-forget-all
           #:pack-propagate
           #:paned-window
           #:photo-image
           #:*ping-interval-seconds*
           #:place
           #:place-forget
           #:popup
           #:postscript
           #:process-events
           #:radio-button
           #:raise
           #:read-event
           #:resizable
           #:sash-coord
           #:sash-place
           #:save-text
           #:scale
           #:screen-height
           #:screen-height-mm
           #:screen-mouse
           #:screen-mouse-x
           #:screen-mouse-y
           #:screen-width
           #:screen-width-mm
           #:scrollbar
           #:scrolled-canvas
           #:scrolled-frame
           #:scrolled-listbox
           #:scrolled-text
           #:scrollregion
           #:search-all-text
           #:search-next-text
           #:see
           #:send-lazy
           #:send-wish
           #:set-coords
           #:set-coords*
           #:set-focus-next
           #:set-geometry
           #:set-geometry-wh
           #:set-geometry-xy
           #:set-wm-overrideredirect
           #:spinbox
           #:start-wish
           #:tag-bind
           #:tag-configure
           #:text
           #:textbox
           #:tkobject
           #:title
           #:toplevel
           #:value
           #:options
           #:vscroll
           #:widget
           #:widget-path
           #:window-height
           #:window-id
           #:window-width
           #:window-x
           #:window-y
           #:window-transient
           #:make-ltk-connection
           #:widget-class-name
           #:with-atomic
           #:with-ltk
           #:call-with-ltk
           #:exit-with-remote-ltk
           #:with-modal-toplevel
           #:with-remote-ltk
           #:with-widgets
           #:withdraw
           #-:tk84
           #:wm-forget
           #-:tk84
           #:wm-manage
           #:wm-title
           #:wm-state
           #:with-hourglass
           #:notebook-index
           #:notebook-add
           #:notebook-tab
           #:notebook-forget
           #:notebook-hide
           #:notebook-identify
           #:notebook-select
           #:notebook-events
           #:notebook-enable-traversal
           #:defwidget
           #:progressbar
           #:length
           #:mode
           #:maximum
           #:phase
           #:separator
           #:sizegrip
           #:treeview
           #:treeview-delete
           #:column-configure
           #:children
           #:treeview-focus
           #:treeview-exists
           #:dictionary-plist
           #:treeview-insert
           #:treeview-item
           #:treeview-column
           #:treeview-heading
           #:treeview-move
           #:treeitem
           #:self
           #:reset-scroll
           #:scroll-to-top
           #:tagbind
           #:pane-configure
           #:handle
           #:column-values
           #:listbox-insert
           #:font-families
           #:scrolled-treeview
           #:treeview-get-selection
           #:treeview-identify
           #:treeview-identify-item
           #:treeview-set-selection
           #:items
           #:image))

(defpackage :ltk-user
  (:use :common-lisp :ltk))

(in-package :ltk)
;communication with wish
;;; this ist the only function to adapted to other lisps

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;(pushnew :tk85 *features*)
  ;;(pushnew :tktable *features*)
  )

(defun do-execute (program args &optional (wt nil))
  "execute program with args a list containing the arguments passed to the program
   if wt is non-nil, the function will wait for the execution of the program to return.
   returns a two way stream connected to stdin/stdout of the program"
  #+(or :clisp :lispworks) (declare (ignore wt))
  (let ((fullstring program))
    (dolist (a args)
      (setf fullstring (concatenate 'string fullstring " " a)))
    #+(or :cmu :scl)
    (let ((proc (run-program program args :input :stream :output :stream :wait wt
                             #+scl :external-format #+scl :utf-8)))
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
    #+:sbcl (let ((proc (sb-ext:run-program program args :input :stream :output :stream :wait wt :search t)))
             (unless proc
               (error "Cannot create process."))
             #+:ext-8859-1
             (make-two-way-stream 
              (sb-sys:make-fd-stream 
               (sb-sys:fd-stream-fd (process-output proc))
               :input t :external-format :iso-8859-1)
              (sb-sys:make-fd-stream 
               (sb-sys:fd-stream-fd (process-input proc))
               :output t  :external-format :iso-8859-1))

             (make-two-way-stream 
              (sb-sys:make-fd-stream 
               (sb-sys:fd-stream-fd (process-output proc))
               :input t :external-format :utf-8)
              (sb-sys:make-fd-stream 
               (sb-sys:fd-stream-fd (process-input proc))
               :output t  :external-format :utf-8))
             #+:xxext-8859-1
	     (make-two-way-stream 
	      (process-output proc)              
	      (process-input proc))
             )
    #+:lispworks (system:open-pipe fullstring :direction :io)
    #+:allegro (let ((proc (excl:run-shell-command
			    #+:mswindows fullstring
			    #-:mswindows (apply #'vector program program args)
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

(defvar *ltk-version* "0.98")

;;; global var for holding the communication stream
(defstruct (ltk-connection (:constructor make-ltk-connection (&key remotep))
			   (:conc-name #:wish-))
  (stream nil)
  (callbacks (make-hash-table :test #'equal))
  (after-ids (make-hash-table :test #'equal))
  (counter 1)
  (after-counter 1)
  (event-queue nil)
  ;; This is should be a function that takes a thunk, and calls it in
  ;; an environment with some condition handling in place.  It is what
  ;; allows the user to specify error-handling in START-WISH, and have
  ;; it take place inside of MAINLOOP.
  (call-with-condition-handlers-function (lambda (f) (funcall f)))
  ;; This is only used to support SERVE-EVENT.
  (input-handler nil)
  (remotep nil)
  (output-buffer nil)
  (variables (make-hash-table :test #'equal))
  )

(defmethod wish-variable (name (wish ltk-connection))
  (gethash name (wish-variables wish)))

(defmethod (setf wish-variable) (val name (wish ltk-connection))
  (setf (gethash name (wish-variables wish)) val))


(defmacro with-ltk-handlers (() &body body)
  `(funcall (wish-call-with-condition-handlers-function *wish*)
	    (lambda () ,@body)))

;;; global connection information

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf
   (documentation 'make-ltk-connection 'function)
   "Create a new LTK-CONNECTION object.  This represents a connection to a
    specific wish.  You can maintain connections to several distinct wish
    processes by binding *WISH* to the one you desire to communicate with, and
    using LTK functions within that dynamic scope."))

(define-condition ltk-error (simple-error) ())
(defun ltk-error (format &rest args)
  (error 'ltk-error :format-control format :format-arguments args))

(defvar *wish* (make-ltk-connection)
  "The current connection to an inferior wish.")

(defvar *wish-connections* ()
  "Connections pushed aside by invoking the NEW-WISH restart in START-WISH.")

;;; verbosity of debug messages, if true, then all communication
;;; with tk is echoed to stdout
(defvar *debug-tk* nil)

;; if set to t, ltk will report the buffer size sent to tk
(defvar *debug-buffers* nil)

(defvar *trace-tk* nil)

(defvar *wish-pathname*
  #+freebsd "wish8.5"
  #-freebsd "wish")

(defvar *wish-args* '("-name" "LTK"))

(defvar *init-wish-hook* nil)

(defparameter *buffer-for-atomic-output* nil)

(defun dbg (fmt &rest args)
;  (with-open-file (w "rl.log" :direction :output :if-exists :append :if-does-not-exist :create)
;      (apply #'format w fmt args)
;      (finish-output w))
  (when *debug-tk*
    (apply #'format *trace-output* fmt args)
    (finish-output *trace-output*)
    ))

(defmacro with-atomic (&rest code)
  `(let ((*buffer-for-atomic-output* t))
     ,@code
     (flush-wish)))

(defmacro send-lazy (&rest code)
  `(let ((*buffer-for-atomic-output* t))
     ,@code
     ))
     

;;; setup of wish
;;; put any tcl function definitions needed for running ltk here
(defun init-wish ()
  (send-lazy
  ;; print string readable, escaping all " and \
  ;; proc esc {s} {puts "\"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\""}
  ;(send-wish "proc esc {s} {puts \"\\\"[regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]\\\"\"} ")
  ;(send-wish "proc escape {s} {return [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]} ")
  (send-wish "package require Tk")
  (flush-wish)

  #+:tk84
  (send-wish "catch {package require Ttk}")
  #-:tk84
  (send-wish "if {[catch {package require Ttk} err]} {tk_messageBox -icon error -type ok -message \"$err\"}")


  (send-wish "proc debug { msg } {
       global server
       puts $server \"(:debug \\\"[escape $msg]\\\")\"
       flush $server
    } ")

  (send-wish "proc escape {s} {regsub -all {\\\\} $s {\\\\\\\\} s1;regsub -all {\"} $s1 {\\\"} s2;return $s2}")
  ;;; proc senddata {s} {puts "(data \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}
  (send-wish "proc senddata {s} {global server; puts $server \"(:data [escape $s])\";flush $server}")

  (send-wish "proc senddatastring {s} {
       global server
       
       puts $server \"(:data \\\"[escape $s]\\\")\"
       flush $server
    } ")

  (send-wish "proc senddatastrings {strings} {
                 global server 
                 puts $server \"(:data (\"
 	         foreach s $strings {
                     puts $server \"\\\"[escape $s]\\\"\"
                     }
                 puts $server \"))\";flush $server} ")
  (send-wish "proc to_keyword  {s} {
                if {[string index $s 0] == \"-\"} {
                   return \":[string range $s 1 [string length $s]]\" } {return \":$s\"}}")
  
  (send-wish "proc sendpropertylist {l} {
               global server; 
               set pos 0
               set ll [llength $l]
               puts $server \"(:data (\"
               while {$pos < $ll} {
                 puts $server \" [to_keyword [lindex $l $pos]] \"
                 set pos [expr $pos + 1]
                 puts $server \" [lindex $l $pos] \"
                 set pos [expr $pos + 1]
                }
               puts $server \"))\"
                  
}")

  (send-wish "proc searchall {widget pattern} {
                  set l [string length $pattern]
                 set result [$widget search $pattern 1.0]
                 set previous 0
                 while {$result > $previous} {
                     $widget tag add sel $result $result+${l}chars
                     set previous $result
                     set result [$widget search $pattern $result+${l}chars]
                 }
             }")
  
  (send-wish "proc searchnext {widget pattern} {
                 set l [string length $pattern]
                 set result [$widget search $pattern insert]
                 if {$result > 0} {
                     $widget tag remove sel 1.0 end
                     $widget tag add sel $result $result+${l}chars
                     $widget mark set insert $result+${l}chars
                     $widget see insert
                 }
             }")
 
  (send-wish "proc resetScroll {c} {
      $c configure -scrollregion [$c bbox all]
}

proc moveToStart {sb} {
   set range [$sb get]
   $sb set 0 [expr [lindex $range 1] - [lindex $range 0]]
 }
")

  ;;; proc sendevent {s} {puts "(event \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}
  ;(send-wish "proc sendevent {s x y keycode char width height root_x root_y} {puts \"(:event \\\"$s\\\" $x $y $keycode $char $width $height $root_x $root_y)\"} ")
  (send-wish "proc sendevent {s x y keycode char width height root_x root_y mouse_button} {global server; puts $server \"(:event \\\"$s\\\" $x $y $keycode $char $width $height $root_x $root_y $mouse_button)\"; flush $server} ")
  ;;; proc callback {s} {puts "(callback \"[regsub {"} [regsub {\\} $s {\\\\}] {\"}]\")"}

  ;;; callback structure: (:callback "widgetname")          ;; for non-parameter callbacks
  ;;;                     (:callback "widgetname" val)      ;; wideget returns non-string value
  ;;;                     (:callback "widgetname" "string") ;; widget returns string value

  (send-wish "proc callback {s} {global server; puts $server \"(:callback \\\"$s\\\")\";flush $server} ")
  (send-wish "proc callbackval {s val} {global server; puts $server \"(:callback \\\"$s\\\" $val)\"} ")
  (send-wish "proc callbackstring {s val} {global server; puts $server \"(:callback \\\"$s\\\" \\\"[escape $val]\\\")\"} ")
  (send-wish "proc keepalive {} {global server; puts $server \"(:keepalive  \\\"[clock format [clock seconds] -format \"%d/%m/%y %T\"]\\\")\"; flush $server}")
  ;(send-wish "global serverlist;set serverlist {{foo 10} {bar 20} {baz 40}}")
  ;(send-wish "global host; set host bar")
  ;(send-wish "global hping; set hping 42")
  
  (dolist (fun *init-wish-hook*)	; run init hook funktions 
    (funcall fun))))


(defun init-tcl (&key debug-tcl)
  (let ((translation "lf"))
    #+(and (or windows win32) (not sbcl)) (setf translation "crlf")

    (format (wish-stream *wish*) "
set buffer {}
set server stdout

set tclside_ltkdebug ~:[0~;1~]
package require Tk
wm protocol . WM_DELETE_WINDOW exit

if {$tclside_ltkdebug} {
   toplevel .ltk
   wm title .ltk \"Debug output\"
   text .ltk.debug -height 20
   pack .ltk.debug -side left -expand 1 -fill both
   scrollbar .ltk.vs -orient vertical -command {.ltk.debug yview}
   .ltk.debug configure -yscrollcommand {.ltk.vs set}
   pack .ltk.vs -side right -fill y
}

proc ltkdebug {text} {
  global tclside_ltkdebug
  if {$tclside_ltkdebug} {
    .ltk.debug insert end \"$text\\\n\"
    .ltk.debug see end
  }
}


proc getcount {s} { 
    if {[regexp {^\\s*(\\d+) } $s match num]} {
        return $num
    }
}

proc getstring {s} { 
    if {[regexp {^\\s*(\\d+) } $s match]} {
        return [string range $s [string length $match] end]
    }
}

proc escape_for_lisp {s} {regsub -all {\\\\} $s {\\\\\\\\} s1;regsub -all {\"} $s1 {\\\"} s2;return $s2}

proc process_buffer {} {
    global buffer
    global server
    set cmd $buffer
    set buffer {}
    if {[catch $cmd result]>0} {
        # tk_messageBox -icon error -type ok -title \"Error!\" -message $result
        puts $server \"(:error \\\"[escape_for_lisp $result]\\\")\"
        flush $server
    } 
}

proc bt {txt} {
    global buffer
    append buffer $txt
}

fconfigure stdin -encoding utf-8 -translation ~a
fconfigure stdout -encoding utf-8
#fileevent stdin readable sread
" debug-tcl translation)))

;;; start wish and set (wish-stream *wish*)
(defun start-wish (&rest keys &key debugger-class remotep stream debug-tcl)
  ;; open subprocess
  (if (null (wish-stream *wish*))
      (progn
        (setf (wish-stream *wish*) (or stream (do-execute *wish-pathname* *wish-args*)))
        #+(or mswindows windows win32) (sleep 1)
        (setf (wish-call-with-condition-handlers-function *wish*)
	      (make-call-with-condition-handlers-function debugger-class))
	;; perform tcl initialisations
        (with-ltk-handlers ()
          (unless remotep
            (init-tcl :debug-tcl debug-tcl))
          (when remotep
            (send-wish "fconfigure $server -blocking 0 -translation lf -encoding utf-8")
            (flush-wish))
          (prog1 (init-wish)
            (ensure-timer))))
      ;; By default, we don't automatically create a new connection, because the
      ;; user may have simply been careless and doesn't want to push the old
      ;; connection aside.  The NEW-WISH restart makes it easy to start another.
      (restart-case (ltk-error "There is already an inferior wish.")
        (new-wish ()
          :report "Create an additional inferior wish."
          (push *wish* *wish-connections*)
          (setf *wish* (make-ltk-connection :remotep remotep))
          (apply #'start-wish keys)))))

;;; CMUCL, SCL, and SBCL, use a two-way-stream and the constituent
;;; streams need to be closed.
(defun close-process-stream (stream)
  "Close a 'stream open by 'do-execute."
  (when *debug-tk*
    (format t "Closing wish stream: ~S~%" stream))
  (ignore-errors (close stream))
  #+(or :cmu :scl :sbcl)
  (when (typep stream 'two-way-stream)
    (close (two-way-stream-input-stream stream) :abort t)
    (close (two-way-stream-output-stream stream) :abort t))
  nil)

(defun exit-wish ()
  (with-ltk-handlers ()
    (let ((stream (wish-stream *wish*)))
      (when stream
        (remove-input-handler)
        (when (open-stream-p stream)
          (ignore-errors
            (send-wish "exit")
            (flush-wish)))
        (close-process-stream stream))
      (setf (wish-stream *wish*) nil)
      #+:allegro (system:reap-os-subprocess)
      (setf *wish-connections* (remove *wish* *wish-connections*))))
  #+:lispworks
  (when (wish-remotep *wish*)
    (throw 'exit-with-remote-ltk nil))
  (throw *wish* nil))

(defun send-wish (text)
  (push text (wish-output-buffer *wish*))
  (unless *buffer-for-atomic-output*
    (flush-wish)))

(defun check-for-errors ()
  (let ((wstream (wish-stream *wish*)))
    (when (can-read wstream)
      (let ((event (verify-event (read-preserving-whitespace wstream nil nil))))
        (setf (wish-event-queue *wish*)
              (append (wish-event-queue *wish*) (list event))))))
  nil)

;; maximum line length sent over to non-remote Tk
(defparameter *max-line-length* 1000)

(defun flush-wish ()
  (let ((buffer (nreverse (wish-output-buffer *wish*))))
    (when buffer
      (let ((len (loop for s in buffer summing (length s)))
            (*print-pretty* nil)
            (stream (wish-stream *wish*)))
        (declare (stream stream))
        (incf len (length buffer))
        (setf (wish-output-buffer *wish*) nil)
        (handler-bind ((stream-error (lambda (e) (handle-dead-stream e stream)))
                       #+lispworks
                       (comm:socket-error (lambda (e) (handle-dead-stream e stream)))
                       )
          (cond
            ((wish-remotep *wish*)
             (let ((content (format nil "~{~a~%~}" buffer)))
               (format stream "~d ~a~%"(length content) content)
               (dbg "~d ~a~%" (length content) content)))
            (*max-line-length*
             (when (or *debug-buffers*
                       *debug-tk*)
               (format t "buffer size ~a~%" len) (finish-output))
             
             (dolist (string buffer)
               (loop while (> (length string) *max-line-length*)
                     do
                  (let ((sub (subseq string 0 *max-line-length*)))
                    (setf string (subseq string *max-line-length*))
                    (format stream "bt \"~A\"~%" (tkescape2 sub))
                    (dbg "bt \"~A\"~%" (tkescape2 sub))))
               (format stream "bt \"~A~%\"~%" (tkescape2 string))
               (dbg "bt \"~A\"~%" (tkescape2 string)))
             (format stream "process_buffer~%")
             (dbg "process_buffer~%")
             )
            (t
             (format stream "bt {~D }~%" len)
             (dbg "bt {~D }~%" len)
             (dolist (string buffer)
               (format stream "bt \"~A~%\"~%" (tkescape2 string))
               (dbg "bt \"~A\"~%" (tkescape2 string)))
             (format stream "process_buffer~%")
             (dbg "process_buffer~%")))
          
          (finish-output stream)

          #+nil(loop for string in buffer
                     do (loop with end = (length string)
                              with start = 0
                              for amount = (min 1024 (- end start))
                              while (< start end)
                              do (let ((string (subseq string start (+ start amount))))
                                   (format stream "buffer_text {~A}~%" string)
                                   (dbg "buffer_text {~A}~%" string)
                                   (incf start amount)))
                        (format stream "buffer_text \"\\n\"~%")
                        (dbg "buffer_text \"\\n\"~%")
                     finally (progn (format stream "process_buffer~%")
                                    (dbg "process_buffer~%")
                                    (finish-output stream)))

          (setf (wish-output-buffer *wish*) nil))))))
  
(defun handle-dead-stream (err stream)
  (when *debug-tk*
    (format *trace-output* "Error sending command to wish: ~A" err)
    (finish-output))
  (ignore-errors (close stream))
  (exit-wish))

(defun format-wish (control &rest args)
  "format 'args using 'control as control string to wish"
  (send-wish (apply #'format nil control args)))
  

#+nil
(defmacro format-wish (control &rest args)
  "format 'args using 'control as control string to wish"
  (let ((stream (gensym)))
    `(progn
       (when *debug-tk*
         (format *trace-output* ,control ,@args)
         (format *trace-output* "~%")
         (finish-output))
       (let ((*print-pretty* nil)
             (,stream (wish-stream *wish*)))
         (declare (type stream ,stream)
                  (optimize (speed 3)))
         
         (format ,stream ,control ,@args)
         (format ,stream "~%")
         (finish-output ,stream))
       nil)))



;; differences:
;; cmucl/sbcl READ expressions only if there is one more character in the stream, if
;; it is a whitespace its discarded. Lispworks READs the expression as soon as it can
;; be fully read from the stream - no character is discarded
;; so I am printing an additional space after every READable expression printed from tcl,
;; this has to be eaten for read-line from the stream in lispworks (which returns the line
;; ending character, cmucl/sbcl don't)

(defun read-all(stream)
  (declare (stream stream)
           #-:lispworks (inline read-char-no-hang))
  (let ((c (read-char-no-hang stream nil nil))
        (s (make-array 256 :adjustable t :element-type 'character :fill-pointer 0)))
    (loop
       while c
       do
         (vector-push-extend c s)
         (setf c (read-char-no-hang stream nil nil)))
    (coerce s 'simple-string)))

#+(and lispworks mswindows)
(lw:defadvice (mp:process-wait-with-timeout peek-char-no-hang :around)
    (whostate timeout &optional function &rest args)
  (apply #'lw:call-next-advice
         whostate
         (if (and (eq timeout 1)
                  (stringp whostate)
                  (string= whostate "Waiting for pipe input"))
             0.001
           timeout)
         function args))

;; set it to like 0.1 to simulate bad networks
;;(defparameter *read-delay* nil)

;;; read from wish 
(defun read-wish ()
  "Reads from wish. If the next thing in the stream is looks like a lisp-list
  read it as such, otherwise read one line as a string."
  ;; FIXME: The problem here is that wish sends us error-messages on the same
  ;; stream that we use for our own communication. It would be good if we could
  ;; get the error-messages (that are presumably written to stderr) onto a separate
  ;; stream. The current workaround is based on the observation that wish error
  ;; messages always seem to end on a newline, but this may not always be so.
  ;;
  ;; READ-ALL would be a bad idea anyways, as in that case we could accidentally
  ;; snarf a real message from the stream as well, if it immediately followed
  ;; an error message.
  (flush-wish)
  (let ((*read-eval* nil)
        (*package* (find-package :ltk))
	(stream (wish-stream *wish*)))
    (if (eql #\( (peek-char t stream nil))
	(read stream nil)
	(read-line stream nil))))


(defun can-read (stream)
  "return t, if there is something to READ on the stream"
  (declare (stream stream)
           #-:lispworks (inline read-char-no-hang unread-char))
  (let ((c (read-char-no-hang stream)))
    (loop 
       while (and c
                  (member c '(#\Newline #\Return #\Space)))
       do
         (setf c (read-char-no-hang stream)))
    (when c
      (unread-char c stream)
      t)))

(define-condition tk-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (error stream)
	     (format stream "Tcl/Tk error: ~A" (message error)))))

(defun verify-event (event)
  (cond
    ((not (listp event))
     (error "When reading from tcl, expected a list but instead got ~S" event))
    ((eq (first event) :error)
     (error 'tk-error :message (second event)))
    (t event)))

(defvar *in-read-event* ()
  "A list of ltk-connection objects that are currently waiting to read an event.")

(defun ping-all-wishes ()
  (dolist (*wish* *in-read-event*)
    (format-wish "keepalive")))

(defvar *ltk-ping-timer* nil)
(defvar *ping-interval-seconds* nil)

(defun ensure-timer ()
  (unless *ltk-ping-timer*
    (when *ping-interval-seconds*
      #+sbcl
      (let ((timer (make-timer (lambda () (ping-all-wishes))
                               :name "Ltk ping timer")))
        (schedule-timer timer *ping-interval-seconds*
                        :repeat-interval *ping-interval-seconds*
                        :absolute-p nil)
        (setf *ltk-ping-timer* timer))
      #+(not sbcl)
      nil)))

(defun tcldebug (something)
  (format t "tcl debug: ~a~%" something)
  (finish-output))

(defun read-event (&key (blocking t) (no-event-value nil))
  "read the next event from wish, return the event or nil, if there is no
event to read and blocking is set to nil"
  (or (pop (wish-event-queue *wish*))
      (let ((wstream (wish-stream *wish*)))
	(flush-wish)
        (if (or blocking (can-read wstream))
            (verify-event
             (let ((*in-read-event* (cons *wish* *in-read-event*)))
               (read-preserving-whitespace wstream nil nil)))
            no-event-value))))

(defun read-data ()
  "Read data from wish. Non-data events are postponed, bogus messages (eg.
+error-strings) are ignored."
  (loop
     for data = (read-wish)
     when (listp data) do
       (cond
         ((null data)
          ;; exit wish
          (exit-wish)
          (return nil))
         ((eq (first data) :data)
          (dbg "read-data: ~s~%" data)
          (return (second data)))
         ((eq (first data) :debug)
          (tcldebug (second data)))
         ((find (first data) #(:event :callback :keepalive))
          (dbg "postponing event: ~s~%" data)
          (setf (wish-event-queue *wish*)
                (append (wish-event-queue *wish*) (list data))))
         ((eq (first data) :error)
          (error 'tk-error :message (second data)))
         (t
          
          (format t "read-data problem: ~a~%" data) (finish-output)
          ))
       else do
       (dbg "read-data error: ~a~%" data)))

(defun read-keyword ()
  (let ((string (read-data)))
    (when (> (length string) 0)
      (values (intern #-scl (string-upcase string)
                      #+scl (if (eq ext:*case-mode* :upper)
                                (string-upcase string)
                                (string-downcase string))
                      :keyword)))))


(defun make-adjustable-string (&optional (string ""))
  (make-array (length string) :element-type 'character
              :initial-contents string :adjustable t :fill-pointer t))

;; Much faster version. For one test run it takes 2 seconds, where the
;; other implementation requires 38 minutes.
(defun tkescape (text)
  (unless (stringp text)
    (setf text (format nil "~a" text)))
  (loop with result = (make-adjustable-string)
     for c across text do
       (when (member c '(#\\ #\$ #\[ #\] #\{ #\} #\"))
         (vector-push-extend #\\ result))
       (vector-push-extend c result)
     finally (return result)))

(defun tkescape2 (text)
  (unless (stringp text)
    (setf text (format nil "~a" text)))
  (loop with result = (make-adjustable-string)
     for c across text do
       (when (member c '(#\\ #\$ #\[ #\] #\"))
         (vector-push-extend #\\ result))
       (vector-push-extend c result)
     finally (return result)))

;;; sanitizing strings: lisp -> tcl (format (wish-stream *wish*) "{~a}" string)
;;; in string escaped : {} mit \{ bzw \}  und \ mit \\

(defun brace-tkescape (text)
  text)
#|
  (unless (stringp text)
    (setf text (format nil "~a" text)))
  (loop with result = (make-adjustable-string)
     for c across text do
       (when (member c '(#\\ #\{ #\}))
         (vector-push-extend #\\ result))
       (vector-push-extend c result)
     finally (return result)))
  |#

;; basic tk object
(defclass tkobject ()
  ((name :accessor name :initarg :name :initform nil)
   )
  (:documentation "Base class for every Tk object"))

;; basic class for all widgets 
(defclass widget(tkobject)
  ((master :accessor master :initarg :master :initform nil) ;; parent widget or nil
   (widget-path :initarg :path :initform nil :accessor %widget-path)         ;; pathname to refer to the widget
   (init-command :accessor init-command :initform nil :initarg :init-command)
   )
  (:documentation "Base class for all widget types"))

;; creating of the tk widget after creating the clos object
(defmethod initialize-instance :after ((w widget) &key)
  (unless (name w)			; generate name if not given 
    (setf (name w) (create-name))))

(defvar *tk* (make-instance 'widget :name "." :path ".")
  "dummy widget to access the tk root object")

;;; tcl -> lisp: puts "$x" mit \ und " escaped
;;;  puts [regsub {"} [regsub {\\} $x {\\\\}] {\"}]

;;; call to convert untility
(defun convert(from to)
  (close-process-stream (do-execute "convert" (list from to) t)))

;;; table used for callback every callback consists of a name of a widget and
;;; a function to call

(defun add-callback (sym fun)
  "create a callback sym is the name to use for storage, fun is the function to call"
  (when *debug-tk*
    (format *trace-output* "add-callback (~A ~A)~%" sym fun))
  (setf (gethash sym (wish-callbacks *wish*)) fun))

(defun remove-callback (sym)
  (when *debug-tk*
    (format *trace-output* "remove-callback (~A)~%" sym))
  (setf (gethash sym (wish-callbacks *wish*)) nil))

(defun callback (sym arg)
  "perform the call of the function associated with sym and the args arg"
  (let ((fun (gethash sym (wish-callbacks *wish*))))
    (when fun
      (apply fun arg))))

(defun senddatastring (tclcmd args)
  (let ((fmt (format nil "if {[catch {~a} result]} {
            puts \"(:error \\\"[escape $result]\\\")\"
           } else { 
           senddatastring $result
}" tclcmd)))
    (apply 'format-wish fmt args)))


(defun after (time fun)
 "after <time> msec call function <fun>, returns the after event id,
which can be passed to AFTER-CANCEL"
 (let ((name (format nil "after~a" (incf (wish-after-counter *wish*)))))
   (format-wish "senddatastring [after ~a {callback ~A}]" time name)
   ;(senddatastring "[after ~a {callback ~A}]" time name)
   (let ((id (read-data))
         (blah (wish-after-ids *wish*)))
     (setf (gethash id blah) name)
     (add-callback name
                   (lambda ()
                     (funcall fun)
                     (remhash id blah)
                     (remove-callback name)))
     id)))

(defun after-idle (fun)
 "call fun when tk becomes idle, returns the after event id, which
can be passed to AFTER-CANCEL"
 (let ((name (format nil "afteridle~a" (incf (wish-after-counter *wish*)))))
   (format-wish "senddatastring [after idle {callback ~A}]" name)
   (let ((id (read-data))
         (blah (wish-after-ids *wish*)))         
     (add-callback name
                   (lambda ()
                     (funcall fun)
                     (remhash id blah)
                     (remove-callback name)))
     id)))

(defun after-cancel (id)
 "cancels a call scheduled with AFTER or AFTER-IDLE by its id"
 (format-wish "after cancel ~a" id)
 (let ((blah (wish-after-ids *wish*)))
   (remove-callback (gethash id blah))
   (remhash id blah)))

;; tool functions used by the objects

(defun get-counter()
  "incremental counter to create unique numbers"
  (incf (wish-counter *wish*)))

#+nil(defun create-name ()
  "create unique widget name, append unique number to 'w'"
  (format nil "w~A" (get-counter)))


(defun encode-base-52 (value)
  (let ((numerals "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (result ""))
    (loop
       while (> value 0)
       do
      (multiple-value-bind (mul rest) (truncate value 52)
        (setf result (format nil "~a~a" result (elt numerals rest)))
        (setf value mul)))
    result))
    
  
(defun create-name ()
  "create unique widget name, append unique number to 'w'"
  (format nil "w~A" (encode-base-52 (get-counter))))




(defun create-path (master name)
  "create pathname from master widget <master> and widget name <name>"
  (let ((master-path (if (or (null master) (eql master *tk*))
                         ""
                         (widget-path master))))
    (format nil "~A.~A" master-path name)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
;;; widget class built helper functions

;;(defparameter *generate-accessors* nil)
  
  (defun iarg-name (arg) (nth 0 arg))
  (defun iarg-key (arg) (nth 1 arg))
  (defun iarg-format (arg) (nth 2 arg))
  (defun iarg-code (arg) (nth 3 arg))
  (defun iarg-comment (arg) (nth 4 arg))

  (defparameter *initargs*
    '(
      (button.background Button.background "~@[ -Button.background ~(~a~)~]" button.background "")
      (Button.cursor Button.cursor "~@[ -Button.cursor ~(~a~)~]" Button.cursor "")
      (Button.relief Button.relief "~@[ -Button.relief ~(~a~)~]" Button.relief "")
      
      (activebackground activebackground "~@[ -activebackground ~(~a~)~]" activebackground
       "background of the active area")

      (activeborderwidth activeborderwidth "~@[ -activeborderwidth ~(~a~)~]" activeborderwidth
       "the border width for active widgets (when the mouse cursor is over the widget)")

      (activeforeground activeforeground "~@[ -activeforeground ~(~a~)~]" activeforeground
       "foreground color for active widgets (when the mouse cursor is over the widget)")

      (activerelief activerelief "~@[ -activerelief ~(~a~)~]" activerelief
       "the border relief for active widgets (when the mouse cursor is over the widget)")
      
      (activestyle activestyle "~@[ -activestyle ~(~a~)~]" activestyle
       "the style for drawing the active part (dotbox, none, underline (default))")
      
      (anchor anchor "~@[ -anchor ~(~a~)~]" anchor
       "specify the alignment of text/image drawn on the widget, one of (:n :w :s :e :nw :sw :se :ne) with :nw designating the top left corner")
      
      (aspect aspect "~@[ -aspect ~(~a~)~]" aspect
       "Aspect ratio for the wrapping of the text. 100 means that the text is redered as wide as, tall, 200 twice as wide.")
      (autoseparators autoseparators "~:[~; -autoseparators 1~]" autoseparators
       "when t, separators are added automatically to the undo stack")
      (background background "~@[ -background ~(~a~)~]" background
       "background color of the widget")
      (bigincrement bigincrement "~@[ -bigincrement ~(~a~)~]" bigincrement
       "size of the big step increment")
      (bitmap bitmap "~@[ -bitmap ~(~a~)~]" bitmap
       "the bitmap to display on the widget, the display is affected by the options 'anchor' and 'justify'")

      (borderwidth borderwidth "~@[ -borderwidth ~(~a~)~]" borderwidth
       "width of the border around the widget in pixels")

      (class class "~@[ -class ~(~a~)~]" class
       "the class of the widget, used for lookup in the option database. This option cannot be changed after the widget creation.")

      (closeenough closeenough "~@[ -closeenough ~(~a~)~]" closeenough
       "dermines when the mouse coursor is considered to be inside a shape, the default is 1.0")

      (colormap colormap "~@[ -colormap ~(~a~)~]" colormap
       "The colormap to use for the widget.")

      (command command "~@[ -command {callback ~a}~]" (and command 
                                                       (progn
                                                         (add-callback (name widget) command)
                                                         (name widget)))
       "function to call when the action of the widget is executed")
      
      (cbcommand command "~@[ -command {callbackval ~{~a $~a~}}~]" (and command 
                                                                    (progn
                                                                      (add-callback (name widget) command)
                                                                      (list (name widget) (name widget))))
       "function to call when the action of the widget is executed")
      (scalecommand command "~@[ -command {callbackval ~a }~]" (and command 
                                                                    (progn
                                                                      (add-callback (name widget) command)
                                                                      (name widget)))
       "function to call when the action of the widget is executed")
      (spinbox-command command "~@[ -command {callbackstring ~a %s}~]" (and command 
                                                                           (progn
                                                                             (add-callback (name widget) command)
                                                                             (name widget))))
      (command-radio-button command "~@[ -command {callbackval ~{~a $~a~}}~]" (and command 
                        						       (progn
										 (add-callback (name widget) command)
										 (list (name widget) (radio-button-variable widget))))
       "function to call when the action of the widget is executed")
      
      (command-scrollbar command "~@[ -command {callback ~a}~]" (and command 
								 (progn
								   (add-callback (name widget) command)
								   (name widget)))"")
      (cols cols "~@[ -cols ~d~]" cols "")
      (compound compound "~@[ -compound ~(~a~)~]" compound
       "")
      
      (confine confine "~:[~; -confine 1~]" confine
       "if t (default) allowed values for view are confined to the scrollregion")
      
      (container container "~:[~; -container 1~]" container
       "if t, then the widget will be used as a container for other widgets.")
      
      (cursor cursor "~@[ -cursor ~(~a~)~]" cursor
       "mouse pointer to display on the widget (valid values are listed in *cursors*)")
      
      (default default "~@[ -default ~(~a~)~]" default
       "")

      (digits digits "~@[ -digits ~(~a~)~]" digits
       "number of digits to use when converting the value to a string.")
      
      (direction direction "~@[ -direction ~(~a~)~]" direction "")
      (disabledbackground disabledbackground "~@[ -disabledbackground ~(~a~)~]" disabledbackground "")
      (disabledforeground disabledforeground "~@[ -disabledforeground ~(~a~)~]" disabledforeground "")
      (elementborderwidth elementborderwidth "~@[ -elementborderwidth ~(~a~)~]" elementborderwidth "")
      (exportselection exportselection "~@[ -exportselection ~(~a~)~]" exportselection "")
      (font font "~@[ -font {~a}~]" font "font to use to display text on the widget")
      (foreground foreground "~@[ -foreground ~(~a~)~]" foreground "foreground color of the widget")
      (format format "~@[ -format ~(~a~)~]" format "")
      (from from "~@[ -from ~(~a~)~]" from "")
      (handlepad handlepad "~@[ -handlepad ~(~a~)~]" handlepad "")
      (handlesize handlesize "~@[ -handlesize ~(~a~)~]" handlesize "")
      (height height "~@[ -height ~(~a~)~]" height "height of the widget")
      (highlightbackground highlightbackground "~@[ -highlightbackground ~(~a~)~]" highlightbackground "")
      (highlightcolor highlightcolor "~@[ -highlightcolor ~(~a~)~]" highlightcolor "")
      (highlightthickness highlightthickness "~@[ -highlightthickness ~(~a~)~]" highlightthickness "")
      ;(image image "~@[ -image ~(~a~)~]" (and image (name image))
      (image image "~@[ -image {~/ltk::down/}~]" (and image (name image))
       "the image to display on the widget, the display is affected by the options 'anchor' and 'justify'")
      (increment increment "~@[ -increment ~(~a~)~]" increment "size of the increment of the widget")
      (indicatorOn indicatorOn "~@[ -indicatorOn ~(~a~)~]" indicatorOn "")
      (insertbackground insertbackground "~@[ -insertbackground ~(~a~)~]" insertbackground "")
      (insertborderWidth insertborderWidth "~@[ -insertborderWidth ~(~a~)~]" insertborderWidth "")
      (insertofftime insertofftime "~@[ -insertofftime ~(~a~)~]" insertofftime "")
      (insertontime insertontime "~@[ -insertontime ~(~a~)~]" insertontime "")
      (insertwidth insertwidth "~@[ -insertwidth ~(~a~)~]" insertwidth "")
      (invalidcommand invalidcommand "~@[ -invalidcommand ~(~a~)~]" invalidcommand "")
      (jump jump "~@[ -jump ~(~a~)~]" jump "")
      (justify justify "~@[ -justify ~(~a~)~]" justify "justification of the text on the widget")
      (label label "~@[ -label ~(~a~)~]" label "text to display on the widget")
      (labelanchor labelanchor "~@[ -labelanchor ~(~a~)~]" labelanchor "")
      (labelwidget labelwidget "~@[ -labelwidget ~(~a~)~]" labelwidget "")
      (length length "~@[ -length ~(~a~)~]" length "")
      (listvariable listvariable "~@[ -listvariable ~(~a~)~]" listvariable "")
      (maxundo maxundo "~@[ -maxundo ~(~a~)~]" maxundo "")
      (menu menu "~@[ -menu ~(~a~)~]" menu "")
      (offrelief offrelief "~@[ -offrelief ~(~a~)~]" offrelief "")
      (offvalue offvalue "~@[ -offvalue ~(~a~)~]" offvalue "")
      (offset offset "~@[ -offset ~(~a~)~]" offset "")
      (onvalue onvalue "~@[ -onvalue ~(~a~)~]" onvalue "")
      (opaqueresize opaqueresize "~@[ -opaqueresize ~(~a~)~]" opaqueresize "")
      (orient orientation "~@[ -orient ~(~a~)~]" orientation "orientation of the widget (horizontal, vertical)")
      (overrelief overrelief "~@[ -overrelief ~(~a~)~]" overrelief "relief of the border, when the mouse is over the widget")
      (padx padx "~@[ -padx ~(~a~)~]" padx "padding around text displayed on the widget")
      (pady pady "~@[ -pady ~(~a~)~]" pady "padding around text displayed on the widget")
      (postcommand postcommand "~@[ -postcommand ~(~a~)~]" postcommand "")
      (readonlybackground readonlybackground "~@[ -readonlybackground ~(~a~)~]" readonlybackground "")
      (relief relief "~@[ -relief ~(~a~)~]" relief "relief of the widgets border (raised, sunken, ridge, groove)")
      (repeatdelay repeatdelay "~@[ -repeatdelay ~(~a~)~]" repeatdelay "")
      (repeatinterval repeatinterval "~@[ -repeatinterval ~(~a~)~]" repeatinterval "")
      (resolution resolution "~@[ -resolution ~(~a~)~]" resolution "")
      (rows rows "~@[ -rows ~d~]" rows "")
      (sashcursor sashcursor "~@[ -sashcursor ~(~a~)~]" sashcursor "")
      (sashpad sashpad "~@[ -sashpad ~(~a~)~]" sashpad "")
      (sashrelief sashrelief "~@[ -sashrelief ~(~a~)~]" sashrelief "")
      (sashwidth sashwidth "~@[ -sashwidth ~(~a~)~]" sashwidth "")
      (screen screen "~@[ -screen ~(~a~)~]" screen "screen on which the toplevel is to be shown")
      (scrollregion scrollregion "~@[ -scrollregion ~(~a~)~]" scrollregion "region in which the canvas should be scolled")
      (selectbackground selectbackground "~@[ -selectbackground ~(~a~)~]" selectbackground "")
      (selectborderwidth selectborderwidth "~@[ -selectborderwidth ~(~a~)~]" selectborderwidth "")
      (selectcolor selectcolor "~@[ -selectcolor ~(~a~)~]" selectcolor "")
      (selectforeground selectforeground "~@[ -selectforeground ~(~a~)~]" selectforeground "")
      (selectimage selectimage "~@[ -selectimage ~(~a~)~]" selectimage "")
      (selectmode selectmode "~@[ -selectmode ~(~a~)~]" selectmode "")
      (setgrid setgrid "~@[ -setgrid ~(~a~)~]" setgrid "")
      (show show "~@[ -show ~(~a~)~]" show "")
      (showhandle showhandle "~@[ -showhandle ~(~a~)~]" showhandle "")
      (showvalue showvalue "~@[ -showvalue ~(~a~)~]" showvalue "")
      (sliderlength sliderlength "~@[ -sliderlength ~(~a~)~]" sliderlength "")
      (sliderrelief sliderrelief "~@[ -sliderrelief ~(~a~)~]" sliderrelief "")
      (spacing1 spacing1 "~@[ -spacing1 ~(~a~)~]" spacing1 "")
      (spacing2 spacing2 "~@[ -spacing2 ~(~a~)~]" spacing2 "")
      (spacing3 spacing3 "~@[ -spacing3 ~(~a~)~]" spacing3 "")
      (state state "~@[ -state ~(~a~)~]" state "")
      (style style "~@[ -style ~s~]" style "")
      (tabs tabs "~@[ -tabs ~(~a~)~]" tabs "")
      (takefocus takefocus "~@[ -takefocus ~(~a~)~]" takefocus "if true, the widget can take the focus")
      (tearoff tearoff "~@[ -tearoff ~(~a~)~]" tearoff "if true, the menu can be torn off")
      (tearoffcommand tearoffcommand "~@[ -tearoffcommand ~(~a~)~]" tearoffcommand "")
      (text text "~@[ -text \"~a\"~]" (tkescape text) "")
      ;;(textvariable textvariable "~@[ -textvariable text_~a~]" (and textvariable (name widget)) "")
      (textvariable text "~@[ -textvariable text_~a~]"
       (progn
         (format-wish "global text_~a ; set text_~a \"~a\"" (name widget) (name widget)
                      (if text
                          (tkescape text)
                          "" ))
         (name widget)) "")

      (tickinterval tickinterval "~@[ -tickinterval ~(~a~)~]" tickinterval "")
      (title title "~@[ -title ~(~a~)~]" title "")
      (to to "~@[ -to ~(~a~)~]" to "")
      (troughcolor troughcolor "~@[ -troughcolor ~(~a~)~]" troughcolor "")
      (type type "~@[ -type ~(~a~)~]" type "")
      (underline underline "~@[ -underline ~(~a~)~]" underline "")
      (undo undo "~@[ -undo ~(~a~)~]" undo "")
      (use use "~@[ -use ~(~a~)~]" use "")
      (validate validate "~@[ -validate ~(~a~)~]" validate "")
      ;(validatecommand validatecommand "~@[ -validatecommand ~(~a~)~]" validatecommand "")

      (validatecommand validatecommand "~@[ -validatecommand {callback ~a;1}~]"
       (and validatecommand 
        (progn
          (add-callback (name widget) validatecommand)
          (name widget))))

      (value value "~@[ -value ~(~a~)~]" value "")
      (value-radio-button nil "~@[ -value ~(~a~)~]" (radio-button-value widget)
       "value for the radio button group to take, when the button is selected")
      (values values "~@[ -values {~{{~a}~^ ~}}~]" values "")
      (variable variable "~@[ -variable ~(~a~)~]" variable "name of the variable associated with the widget")
      (variable-radio-button nil "~@[ -variable ~(~a~)~]" (radio-button-variable widget)
       "name of the radio button group the button shall belong to as a string")
      (visual visual "~@[ -visual ~(~a~)~]" visual "")
      (width width "~@[ -width ~(~a~)~]" width "width of the widget")
      (wrap wrap "~@[ -wrap ~(~a~)~]" wrap "")
      (wraplength wraplength "~@[ -wraplength ~(~a~)~]" wraplength "")
      (xscrollcommand xscrollcommand "~@[ -xscrollcommand ~(~a~)~]" xscrollcommand "")
      (xscrollincrement xscrollincrement "~@[ -xscrollincrement ~(~a~)~]" xscrollincrement "")
      (yscrollcommand yscrollcommand "~@[ -yscrollcommand ~(~a~)~]" yscrollcommand "")
      (yscrollincrement yscrollincrement "~@[ -yscrollincrement ~(~a~)~]" yscrollincrement "")
      ))
  

  (defparameter *class-args*
    '())
  
  (defun build-args (class parents defs)
    (declare (ignore class))
    ;;(format t  "class ~s parents ~s defs ~s~%" class parents defs) (finish-output)
    (let ((args nil))
      (dolist (p parents)
	(let ((arglist (rest (assoc p *class-args*))))
	  ;;(format t "parent: ~s arglist: ~s~%" p arglist) (finish-output)
	  (dolist (arg arglist)
	    (unless (member arg args)
	      (setf args (append args (list arg)))))))
      (loop 
         while defs
         do
           (let ((arg (pop defs)))
             (cond
               ((eq arg :inherit)	 
                (let* ((inheritedclass (pop defs))
                       (arglist (rest (assoc inheritedclass *class-args*))))
                  (dolist (arg arglist)
                    (unless (member arg args)
                      (setf args (append args (list arg)))
                      ))))
               ((eq arg :delete)
                (setf args (delete (pop defs) args)))	    
               (t
                (setf args (append args (list arg)))))))
      ;;(format t "class: ~a args: ~a~&" class args) (finish-output)
      args
      ))
  )

#+nil(defmacro defargs (class parents &rest defs)
  (let ((args (build-args class parents defs)))
    (setf *class-args* (append (remove-if (lambda (entry)
					     (equal (car entry) class))
					   *class-args*)))
    `(setf *class-args* (append (remove-if (lambda (entry)
					     (equal (car entry) ',class))
					   *class-args*) (list '(,class ,@args))))))

(defmacro defargs (class parents &rest defs)
  (let ((args (build-args class parents defs)))
    (setf *class-args* (append (remove class *class-args* :key #'car) (list (cons class args))))
   `(setf *class-args* (append (remove (quote ,class) *class-args* :key #'car) (list '(,class ,@args))))))


#+:tk84
(defargs widget () 
  relief cursor borderwidth background)

#-:tk84
(defargs widget () 
  class cursor style takefocus)

#-:tk84
(defargs treeview (widget)
  xscrollcommand yscrollcommand columns displaycolumns height padding selectmode show)

;(defargs button (widget) anchor)
;(defargs text (widget button) :delete anchor color)

#-:tk84
(defargs button (widget) 
 command compound default image state textvariable underline width)

#+:tk84
(defargs button (widget) 
  activebackground activeforeground anchor bitmap command compound default disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image justify overrelief padx pady repeatdelay repeatinterval state takefocus textvariable underline width wraplength)

(defargs canvas ()
  background borderwidth closeenough confine cursor height highlightbackground highlightcolor highlightthickness insertbackground insertborderwidth insertofftime insertontime insertwidth offset relief scrollregion selectbackground selectborderwidth selectforeground state takefocus width xscrollcommand xscrollincrement yscrollcommand yscrollincrement)

#-:tk84
(defargs check-button (widget)
  cbcommand compound image offvalue onvalue state textvariable underline variable width)

#+:tk84
(defargs check-button ()
  activebackground activeforeground anchor background bitmap borderwidth cbcommand compound cursor disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image indicatorOn justify offrelief offvalue onvalue overrelief padx pady relief selectcolor selectimage state takefocus textvariable underline variable width wraplength)

#-:tk84
(defargs combobox ()
  class cursor exportselection justify height postcommand state style takefocus textvariable values width)

#+:tk84
(defargs entry ()
  background borderwidth cursor disabledbackground disabledforeground exportselection font foreground highlightbackground highlightcolor highlightthickness insertbackground insertborderwidth insertofftime insertontime insertwidth invalidcommand justify readonlybackground relief selectbackground selectborderwidth selectforeground show state takefocus textvariable validate validatecommand width xscrollcommand )

#-:tk84
(defargs entry ()
  class cursor exportselection invalidcommand justify show state style takefocus textvariable validate validatecommand width xscrollcommand)

#+:tk84
(defargs frame ()
  borderwidth class relief background colormap container cursor height highlightbackground highlightcolor highlightthickness padx pady takefocus visual width)

(defargs classic-frame ()
  borderwidth class relief background colormap container cursor height highlightbackground highlightcolor highlightthickness padx pady takefocus visual width)

#-:tk84
(defargs frame ()
  borderwidth class relief cursor height padding takefocus width)

#+:tk84
(defargs label ()
  activebackground activeforeground anchor background bitmap borderwidth compound cursor disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image justify padx pady relief state takefocus textvariable underline width wraplength )

#-:tk84
(defargs label ()
  anchor background class compound cursor font foreground image justify padding relief style takefocus textvariable underline width wraplength)

#+:tk84
(defargs labelframe ()
  borderwidth class font foreground labelanchor labelwidget relief text background colormap container cursor height highlightbackground highlightcolor highlightthickness padx pady takefocus visual width)

#-:tk84
(defargs labelframe ()
  class labelanchor labelwidget text cursor height padding style takefocus width)

(defargs listbox ()
  activestyle background borderwidth cursor disabledforeground exportselection font foreground height highlightbackground highlightcolor highlightthickness relief selectbackground selectborderwidth selectforeground selectmode setgrid state takefocus width xscrollcommand yscrollcommand listvariable)

(defargs menu ()
  activebackground activeborderwidth activeforeground background borderwidth cursor disabledforeground font foreground postcommand relief selectcolor takefocus tearoff tearoffcommand title type)

#+:tk84
(defargs menubutton ()
  activebackground activeforeground anchor background bitmap borderwidth cursor direction disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image indicatorOn justify menu padx pady relief compound state takefocus textvariable underline width wraplength)

#-:tk84
(defargs menubutton ()
  class compund cursor direction image menu state style takefocus textvariable underline width)

(defargs message ()
  anchor aspect background borderwidth cursor font foreground highlightbackground highlightcolor highlightthickness justify padx pady relief takefocus textvariable width)

#-:tk84
(defargs notebook ()
  class cursor style takefocus height padding width)

#+:tk84
(defargs paned-window ()
  background borderwidth cursor handlepad handlesize height opaqueresize orient relief sashcursor sashpad sashrelief sashwidth showhandle width)

#-:tk84
(defargs paned-window ()
  class cursor orient style takefocus width height)

#-:tk84
(defargs progressbar ()
  class cursor orient style takefocus length mode maximum phase value)

#+:tk84
(defargs radio-button ()
  activebackground activeforeground anchor background bitmap borderwidth command-radio-button compound cursor disabledforeground font foreground height highlightbackground highlightcolor highlightthickness image indicatorOn justify offrelief overrelief padx pady relief selectcolor selectimage state takefocus textvariable underline value-radio-button variable-radio-button width wraplength)

#-:tk84
(defargs radio-button ()
  command-radio-button class compound cursor image state style takefocus textvariable underline value-radio-button variable-radio-button width)

#+:tk84
(defargs scale ()
  activebackground background bigincrement borderwidth scalecommand cursor digits font foreground from highlightbackground highlightcolor highlightthickness label length orient relief repeatdelay repeatinterval resolution showvalue sliderlength sliderrelief state takefocus tickinterval to troughcolor variable width)

#-:tk84
(defargs scale ()
  class scalecommand cursor from length orient style takefocus to variable)

#+:tk84
(defargs scrollbar ()
  activebackground activerelief background borderwidth command-scrollbar cursor elementborderwidth highlightbackground highlightcolor highlightthickness jump orient relief repeatdelay repeatinterval takefocus troughcolor width)

#-:tk84
(defargs scrollbar ()
  class command-scrollbar cursor orient style takefocus)

#-:tk84
(defargs separator ()
  class cursor orient state style takefocus)

#-:tk84
(defargs sizegrip ()
  class cursor state style takefocus)

(defargs spinbox ()
  activebackground background borderwidth Button.background Button.cursor Button.relief spinbox-command cursor disabledbackground disabledforeground exportselection font foreground format from highlightbackground highlightcolor highlightthickness increment insertbackground insertborderwidth insertofftime insertontime insertwidth invalidcommand justify relief readonlybackground repeatdelay repeatinterval selectbackground selectborderwidth selectforeground state takefocus textvariable to validate validatecommand values width wrap xscrollcommand)

(defargs text ()
  autoseparators  background borderwidth cursor exportselection font foreground height highlightbackground highlightcolor highlightthickness insertbackground insertborderwidth insertofftime insertontime insertwidth maxundo padx  pady relief selectbackground selectborderwidth selectforeground setgrid spacing1 spacing2 spacing3 state tabs takefocus undo width wrap xscrollcommand yscrollcommand)

(defargs toplevel ()
  borderwidth class menu relief screen use background colormap container cursor height highlightbackground highlightcolor highlightthickness padx pady takefocus visual width)

(defmacro defwrapper (class parents slots cmd &rest code)
  (let ((args (sort (copy-list (rest (assoc class *class-args*)))
		    (lambda (x y)
		      (string< (symbol-name x) (symbol-name y))))))
    (let ((cmdstring (format nil "~~a ~~~~A "))
	  (codelist nil)
	  (keylist nil)
	  (accessors nil))
      (dolist (arg args)
	(let ((entry (assoc arg *initargs*)))
	  (cond
            (entry 
             (setf cmdstring (concatenate 'string cmdstring (third entry)))
             (when (iarg-key entry)
               (setf keylist (append keylist (list (iarg-key entry)))))
             (setf codelist (append codelist (list (iarg-code entry))))
             #+:generate-accessors
             (when (and (iarg-key entry)
                        (not (equal (iarg-key entry) 'variable))
                        (not (equal (iarg-key entry) 'class))
                        (not (equal (iarg-key entry) 'length))
                        (not (equal (iarg-key entry) 'values))
                        (not (equal (iarg-key entry) 'format))
                        (not (equal (iarg-key entry) 'scrollregion)))
               (push
                `(defmethod (setf ,(iarg-key entry)) (value (widget ,class))
                   (format-wish ,(format nil "~~a configure ~a"(third entry)) (widget-path widget) value))	   
                accessors)
               (push
                `(defmethod ,(iarg-key entry) ((widget ,class))
                   (format-wish ,(format nil "senddata \"[~~a cget -~(~a~)]\"" (iarg-key entry)) (widget-path widget))
                   (read-data))
                accessors))
             )
            (t 
             (setf cmdstring (concatenate 'string cmdstring (format nil "~~@[ -~(~a~) ~~(~~A~~)~~]" arg)))
             (setf keylist (append keylist (list arg)))
             (setf codelist (append codelist (list arg)))
	  ))))
      (push `(widget-class-name :accessor widget-class-name :initform ,cmd :allocation :class) slots)
      `(progn
	 (defclass ,class (,@parents)
	   ,slots)
	 (defmethod initialize-instance :after ((widget ,class) &key ,@keylist)
           ;;(format-wish ,cmdstring (widget-class-name widget) (widget-path widget) ,@codelist)
	   ;;(format t "setting initarg for ~a~%" (quote ,class)) (finish-output)
	   (setf (init-command widget)
		 (format nil ,cmdstring (widget-class-name widget) ,@codelist))
	   ,@code)
	 ,@accessors
	 ))))

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

(defun destroy (widget)
  (when (slot-boundp widget 'widget-path)
    (send-wish (format nil "destroy ~a" (widget-path widget)))
    (unless (eql widget *tk*)
      (slot-makunbound widget 'widget-path))))

(defun clipboard-clear ()
  (send-wish "clipboard clear"))

(defun clipboard-get ()
  (format-wish "senddatastring [clipboard get]")
  (read-data))

(defun clipboard-append (txt)
  (format-wish "clipboard append {~a}" txt))

;; around - initializer

(defmethod initialize-instance :around ((w widget) &key pack place grid)
  (call-next-method)
  ;; pack widget if parameter has been supplied
  (when pack
    (apply #'pack w pack))
  (when place
    (apply #'place w place))
  (when grid
    (apply #'grid w grid)))

(defgeneric widget-path (widget))
(defmethod widget-path ((w (eql nil))) nil)

(defmethod widget-path ((widget widget))
  "retrieve the slot value widget-path, if not given, create it"
  (or (%widget-path widget)
      (prog1
	  (setf (slot-value widget 'widget-path)
		(create-path (master widget) (name widget)))
	(create widget))))

(defgeneric create (w))

(defmethod create ((widget widget))
  (when (init-command widget)
    ;;(format t "creating: ~a~%" (init-command widget)) (finish-output)
    (format-wish (init-command widget) (widget-path widget))))

(defgeneric (setf command) (value widget))
(defgeneric command (widget))

(defmethod command ((widget widget))
  (gethash (name widget) (wish-callbacks *wish*)))

(defgeneric lower (widget &optional other))
(defmethod lower ((widget widget) &optional other)
  (send-wish (format nil "lower ~a~@[ ~a~]" (widget-path widget) (and other (widget-path other)))))

(defgeneric raise (widget &optional above))
(defmethod raise ((widget widget) &optional above)
  (send-wish (format nil "raise ~a~@[ ~a~]" (widget-path widget) (and above (widget-path above)))))

(defun tk-number (number)
  "convert number to integer/single float"
  (cond
    ((integerp number)
     number)
    ((typep number 'single-float)
     number)
    ((typep number 'double-float)
     (coerce number 'single-float))
    ((typep number 'rational)
     (coerce number 'single-float))
    ((null number)
     nil)
    (t
     (error "~s is not a one of integer, float or rational." number))))

(defstruct event
  x
  y
  keycode
  char
  width
  height
  root-x
  root-y
  mouse-button
  )

(defun construct-tk-event (properties)
  "create an event structure from a list of values as read from tk"
  (make-event
   :x (first properties)
   :y (second properties)
   :keycode (third properties)
   :char (fourth properties)
   :width (fifth properties)
   :height (sixth properties)
   :root-x (seventh properties)
   :root-y (eighth properties)
   :mouse-button (ninth properties)
   ))

(defgeneric bind (w event fun &key append exclusive))
(defmethod bind ((w widget) event fun &key append exclusive)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    ;;(format-wish "bind  ~a ~a {sendevent ~A %x %y %k %K %w %h %X %Y}" (widget-path w) event name)
    (format-wish "bind  ~a ~a {~:[~;+~]sendevent ~A %x %y %k %K %w %h %X %Y %b ~:[~;;break~]}" 
		 (widget-path w) event append name exclusive)
    w))

(defmethod bind (s event fun &key append exclusive)
  "bind fun to event within context indicated by string ie. 'all' or 'Button'"
  (let ((name (create-name)))
    (add-callback name fun)
    ;;(format-wish "bind  ~a ~a {sendevent ~A %x %y %k %K %w %h %X %Y}" s event name)
    (format-wish "bind  ~a ~a {~:[~;+~]sendevent ~A %x %y %k %K %w %h %X %Y %b ~:[~;;break~]}" 
		 s event append name exclusive)))

;;; generic functions

(defgeneric canvas (w))

(defgeneric value (widget)
  (:documentation "reads the value of the variable associated with the widget"))

(defclass tkvariable ()
  ())

(defmethod initialize-instance :around ((v tkvariable) &key)
  (call-next-method)
  (format-wish "~a configure -variable ~a ; global ~a ; set ~a {}" (widget-path v) (name v) (name v) (name v)))

(defmethod value ((v tkvariable))
  (format-wish "global ~a; senddata $~a" (name v) (name v))
  (read-data))

(defgeneric (setf value) (widget val))
(defmethod (setf value) (val (v tkvariable))
  (format-wish "global ~a; set ~a {~a}" (name v) (name v) val)
  val)

(defclass tktextvariable ()
  ())

(defgeneric text (widget)
  (:documentation "reads the value of the textvariable associated with the widget")
  )


(defmethod text ((v tktextvariable))
  (format-wish "global text_~a ; senddatastring $text_~a" (name v) (name v))
  (read-data))

(defgeneric (setf text) (val variable))

(defmethod (setf text) (val (v tktextvariable))
  (format-wish "global text_~a ; set text_~a \"~a\""  (name v)  (name v) (tkescape2 val))
  val)

;;; window menu bar

(defclass menubar(widget)
  ())

(defun make-menubar(&optional (master nil))
 (make-instance 'menubar :master master :name "menubar"))

;(defmethod create ((mb menubar))
(defmethod initialize-instance :after ((mb menubar) &key)
  (format-wish "menu ~a -tearoff 0 -type menubar" (widget-path mb))
  (format-wish "~a configure -menu ~a" (if (master mb)
                                           (widget-path (master mb))
                                           ".")
               (widget-path mb)))

;;; menues

(defclass menu(widget)
  ((text :accessor text :initarg :text)
   (help :accessor menu-help :initarg :help :initform nil)))

;(defmethod create ((m menu))

(defmethod initialize-instance :after ((m menu) &key underline (tearoff 0))
  (when (menu-help m) ;; special treatment for help menu
    (setf (name m) "help")
    (setf (slot-value m 'widget-path) (create-path (master m) (name m))))
  (format-wish "menu ~A -tearoff ~a" (widget-path m) tearoff)
  (when (master m)
    (format-wish "~A add cascade -label {~A} -menu ~a~@[ -underline ~a ~]"
                 (widget-path (master m)) (text m) (widget-path m) underline)))

(defun make-menu(menu text &key underline name (tearoff 0))
  (if name
      (make-instance 'menu :master menu :text text :underline underline :name name :tearoff tearoff)
      (make-instance 'menu :master menu :text text :underline underline :name name :tearoff tearoff)))

(defun add-separator (menu)
   (format-wish "~A add separator" (widget-path menu))
   menu)

(defgeneric state (menu menu-label state))
(defmethod state ((a menu) menu-label state)
  (format-wish "~a entryconfigure {~a} -state {~a}" (widget-path a) menu-label state))

(defgeneric menu-label (menu old new))
(defmethod menu-label ((a menu) old new)
  (format-wish "~a entryconfigure {~a} -label {~a}"  (widget-path a)  old new))

;;; menu button

(defclass menuentry(widget)
  ((text :accessor text :initarg :text :initform ""))
  (:documentation "An abstract base class for menu entries.  These \"widgets\" have to be handled specially by some
methods, e.g. 'configure'."))

(defclass menubutton(menuentry) 
  ())

(defmethod initialize-instance :after ((m menubutton) &key command underline accelerator state)
  (when command
    (add-callback (name m) command))
  (format-wish "~A add command -label {~A}  -command {callback ~A}~@[ -underline ~a ~]~@[ -accelerator {~a} ~]~@[ -state ~(~a~)~]"
               (widget-path (master m)) (text m) (name m) underline accelerator state))

(defun make-menubutton(menu text command &key underline accelerator state)
  (let* ((mb (make-instance 'menubutton :master menu :text text :command command :underline underline
			    :accelerator accelerator :state state)))
    mb))

(defclass menucheckbutton(menuentry)
  ((command :accessor command :initarg :command :initform nil)))

(defmethod initialize-instance :after ((m menucheckbutton) &key)
  (when (command m)
    (add-callback (name m) (command m)))
  (format-wish "~A add checkbutton -label {~A} -variable ~a ~@[ -command {callback ~a}~]"
	       (widget-path (master m)) (text m) (name m) (and (command m) (name m))))

(defmethod value ((cb menucheckbutton))
  (format-wish "global ~a; senddata $~a" (name cb) (name cb))
  (read-data))

(defmethod (setf value) (val (cb menucheckbutton))
  (format-wish "global ~a; set ~a ~a" (name cb) (name cb) val)
  val)

(defclass menuradiobutton(menuentry) 
  ((command :accessor command :initarg :command :initform nil)
   (group :accessor group :initarg :group :initform nil)))

(defmethod initialize-instance :after ((m menuradiobutton) &key)
  (when (command m)
    (add-callback (name m) (command m)))
  (unless (group m)
    (setf (group m)
	  (name m)))
  (format-wish "~A add radiobutton -label {~A} -value ~a -variable ~a ~@[ -command {callback ~a}~]"
               (widget-path (master m)) (text m) (name m) (group m)
               (and (command m) (name m))))

(defmethod value ((cb menuradiobutton))
  (format-wish "global ~a; senddata $~a" (group cb) (group cb))
  (read-data))

(defmethod (setf value) (val (cb menuradiobutton))
  (format-wish "global ~a; set ~a ~a" (group cb) (group cb) val)
  val)


;;; method to pop up a menue at the root window coordinates x and y

(defgeneric popup (menu x y))
(defmethod popup ((menu menu) x y)
  (format-wish "tk_popup ~A ~A ~A" (widget-path menu)
               (tk-number x) (tk-number y))
  menu)

(defgeneric menu-delete (menu index &optional end))
(defmethod menu-delete ((menu menu) index &optional (end :end))
  (format-wish "~A delete ~A ~@[ ~(~A~)~]" (widget-path menu) index end)
  menu)

;;; standard button widget

#+:tk84
(defwrapper button (tktextvariable widget) () "button")

#-:tk84
(defwrapper button (tktextvariable widget) () "ttk::button")

(defmethod (setf command) (val (button button))
  (add-callback (name button) val)
  (format-wish "~a configure -command {callback ~a}" (widget-path button) (name button))
  val)

;;; check button widget

#+:tk84
(defwrapper check-button (tktextvariable widget tkvariable) () "checkbutton")

#-:tk84
(defwrapper check-button (tktextvariable widget tkvariable) () "ttk::checkbutton")

(defmethod (setf command) (val (check-button check-button))
  (add-callback (name check-button) val)
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path check-button)
	       (name check-button) (name check-button))
  val)

(defmethod value ((v check-button))
  (format-wish "global ~a; senddata $~a" (name v) (name v))
  (if (equal 1 (read-data))
      t
      nil))

(defmethod (setf value) (val (v check-button))
  (when (or (equal val 1)
            (equal val 0))
    (warn "Use of 1 and 0 for check-button values is deprecated, use T or NIL. Treating ~A as t"
          val))
  (format-wish "global ~a; set ~a {~a}" (name v) (name v) (if val 1 0))
  val)

;;; radio button widget

#+:tk84
(defwrapper radio-button (tktextvariable widget) 
  ((val :accessor radio-button-value :initarg :value :initform nil)
   (var :accessor radio-button-variable :initarg :variable :initform nil)) 
  "radiobutton")

#-:tk84
(defwrapper radio-button (tktextvariable widget) 
  ((val :accessor radio-button-value :initarg :value :initform nil)
   (var :accessor radio-button-variable :initarg :variable :initform nil)) 
  "ttk::radiobutton")

(defmethod value ((rb radio-button))
  "reads the content of the shared variable of the radio button set"
  (if (radio-button-variable rb)
      (progn
	(format-wish "global ~a; senddata $~a" (radio-button-variable rb) (radio-button-variable rb))
	(read-data))
      nil))

(defmethod (setf value) (val (rb radio-button))
  "sets the content of the shared variable of the radio button set"
  (when (radio-button-variable rb)
    (format-wish "global ~a; set ~a ~a" (radio-button-variable rb) (radio-button-variable rb) val))
  val)

(defmethod (setf command) (val (rb radio-button))
  (add-callback (name rb) val)
  (format-wish "~a configure -command {global ~a;callbackval ~a $~a}" (widget-path rb) (name rb) (name rb) (radio-button-variable rb))
  val)

;; ttk combo box

#-:tk84
(defwrapper combobox (tktextvariable widget) () "ttk::combobox")

#-:tk84
(defgeneric (setf options) (value widget))

#-:tk84
(defmethod (setf options) (values (combobox combobox))
  (format-wish "~a configure -values {~{ \{~a\}~}}" (widget-path combobox) values))


;; text entry widget

#+:tk84
(defwrapper entry (tktextvariable widget) () "entry")

#-:tk84
(defwrapper entry (tktextvariable widget) () "ttk::entry")

(defun entry-select (e from to)
  (format-wish "~a selection range ~a ~a" (widget-path e) from to)
  e)

(defgeneric cursor-index (widget)
  (:documentation "returns the cursor index in the widget"))

(defmethod cursor-index ((e entry))
  (format-wish "senddata [~a index insert]" (widget-path e))
  (read-data))

(defun split (string at)
  (let ((pos (search at string))
        erg)
    (loop
       while pos
       do
         (when (> pos 0)
           (push (subseq string 0 pos) erg))
         (setf string (subseq string (+ pos (length at))))
         (setf pos (search at string)))
    (when (> (length string) 0)
      (push string erg))
    (nreverse erg)))
        


;;; frame widget 

#+:tk84
(defwrapper frame (widget) () "frame")

(defwrapper classic-frame (widget) () "frame")

#-:tk84
(defwrapper frame (widget) () "ttk::frame")

;(defun make-frame (master)
;  (make-instance 'frame :master master))

;;; labelframe widget 

#+:tk84
(defwrapper labelframe (widget) () "labelframe")

#-:tk84
(defwrapper labelframe (widget) () "ttk::labelframe")

(defmethod (setf text) :after (val (l labelframe))
  (format-wish "~a configure -text {~a}" (widget-path l) val)
  val)

;;; panedwindow widget


#+:tk84
(defwrapper paned-window (widget) () "panedwindow")

#-:tk84
(defwrapper paned-window (widget) () "ttk::panedwindow")

#-:tk84
(defwrapper progressbar (widget tkvariable) () "ttk::progressbar")


(defgeneric add-pane (window widget &rest options))
(defmethod add-pane ((pw paned-window) (w widget) &rest options)
  (format-wish "~a add ~a ~{ -~(~a~) {~/ltk::down/}~}" (widget-path pw) (widget-path w) options)
  pw)

(defgeneric forget-pane (window widget))
(defmethod forget-pane ((pw paned-window) (w widget))
  (format-wish "~a forget ~a" (widget-path pw) (widget-path w))
  pw)


(defgeneric pane-configure (l i &rest options))
(defmethod pane-configure ((p paned-window) (w widget)  &rest options)
  (format-wish "~a paneconfigure ~a ~{ -~(~a~) {~/ltk::down/}~}" (widget-path p) (widget-path w)  options)
  p)


(defgeneric sash-coord (window index))

#+:tk84
(defmethod sash-coord ((pw paned-window) index)
  (format-wish "senddata \"([~a sash coord ~a])\"" (widget-path pw) index)
  (read-data))

#-:tk84
(defmethod sash-coord ((pw paned-window) index)
  (format-wish "senddata \"([~a sashpos ~a])\"" (widget-path pw) index)
  (read-data))


#+:tk84
(defgeneric sash-place (window index x y))
#+:tk84
(defmethod sash-place ((pw paned-window) index x y)
  (format-wish "~a sash place ~a ~a ~a" (widget-path pw) index x y))

#-:tk84
(defgeneric sash-place (window index pos))
#-:tk84
(defmethod sash-place ((pw paned-window) index pos)
  (format-wish "~a sashpos ~a ~a" (widget-path pw) index pos))

;;; listbox widget

(defwrapper listbox (widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   ) "listbox")

(defmethod (setf command) (val (listbox listbox))
  (add-callback (name listbox) val)
  (format-wish "bind ~a <<ListboxSelect>> {callbackval ~a ([~a curselection])}" (widget-path listbox) (name listbox)
	       (widget-path listbox))
  val)

(defgeneric listbox-append (l vals))
(defmethod listbox-append ((l listbox) values)
  "append values (which may be a list) to the list box"
  (if (listp values)
      (format-wish "~a insert end ~{ \{~a\}~}" (widget-path l) values)
      (format-wish "~a insert end \{~a\}" (widget-path l) values))
  l)

(defgeneric listbox-get-selection (l))
(defmethod listbox-get-selection ((l listbox))
  (format-wish "senddata \"([~a curselection])\"" (widget-path l))
  (read-data))

(defgeneric listbox-select (l val))
(defmethod listbox-select ((l listbox) val)
  "modify the selection in listbox, if nil is given, the selection is cleared,
if a number is given the corresponding element is selected, alternatively
a list of numbers may be given"
  (if (null val)
      (format-wish "~a selection clear 0 end" (widget-path l))
      (if (listp val)
          (format-wish "~a selection set ~{ ~a~}" (widget-path l) val)
          (format-wish "~a selection set ~a" (widget-path l) val)))
  l)

(defgeneric listbox-clear (l))
(defmethod listbox-clear ((l listbox))
  (format-wish "~a delete 0 end" (widget-path l))
  l)

(defgeneric listbox-delete (l start &optional end))
(defmethod listbox-delete ((l listbox) start &optional end)
  (format-wish "~a delete ~a ~@[~(~a~)~]" (widget-path l) start end)
  l)

(defgeneric listbox-insert (l index values))
(defmethod listbox-insert ((l listbox) index values)
  (if (listp values)
      (format-wish "~a insert ~a ~{ \{~a\}~}" (widget-path l) index values)
      (format-wish "~a insert ~a \{~a\}" (widget-path l) index values))
  l)

(defgeneric listbox-configure (l i &rest options))
(defmethod listbox-configure ((l listbox) index &rest options)
  (format-wish "~a itemconfigure ~a ~{ -~(~a~) {~/ltk::down/}~}" (widget-path l) index options)
  l)

(defgeneric listbox-nearest (listbox y))
(defmethod listbox-nearest ((l listbox) y)
  (format-wish "senddata [~a nearest ~a]" (widget-path l) y)
  (read-data))


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
 
  (configure (hscroll sl) "command" (concatenate 'string (widget-path (listbox sl)) " xview"))
  (configure (vscroll sl) "command" (concatenate 'string (widget-path (listbox sl)) " yview"))
  (configure (listbox sl) "xscrollcommand" (concatenate 'string (widget-path (hscroll sl)) " set"))
  (configure (listbox sl) "yscrollcommand" (concatenate 'string (widget-path (vscroll sl)) " set")))

(defmethod listbox-append ((l scrolled-listbox) values)
  (listbox-append (listbox l) values)
  l)

(defmethod listbox-get-selection ((l scrolled-listbox))
  (listbox-get-selection (listbox l)))

(defmethod listbox-select ((l scrolled-listbox) val)
  (listbox-select (listbox l) val)
  l)

;;; notebook
;#-:tk84
(defwrapper notebook (widget) () "ttk::notebook")

(defgeneric notebook-add (nb widget &rest options))
(defmethod notebook-add ((nb notebook) (w widget) &rest options)
  (format-wish "~a add ~a ~{-~(~a~) {~a}~}" (widget-path nb) (widget-path w) options))

(defgeneric notebook-tab (nb widget option value))
(defmethod notebook-tab ((nb notebook) (w widget) option value)
  (format-wish "~a tab ~a -~(~a~) {~a}" (widget-path nb)
	       (widget-path w) option value))

(defgeneric notebook-forget (nb widget))
(defmethod notebook-forget ((nb notebook) (w widget))
  (format-wish "~a forget ~a" (widget-path nb) (widget-path w)))

(defgeneric notebook-hide (nb tab))
(defmethod notebook-hide ((nb notebook) (tab widget))
  (format-wish "~a hide ~a" (widget-path nb) (widget-path tab)))

(defgeneric notebook-identify (nb x y))
(defmethod notebook-identify ((nb notebook) x y)
  (format-wish "senddatastring [~a identify ~a ~a]" (widget-path nb) x y)
  (read-data))

(defgeneric notebook-index (nb tab))
(defmethod notebook-index ((nb notebook) (tab widget))
  (format-wish "senddata [~a index ~a]" (widget-path nb) (widget-path tab))
  (read-data))

;; todo notebook-insert
;;      notebook-instate

(defgeneric notebook-select (nb tab))
(defmethod notebook-select ((nb notebook) (tab widget))
  (format-wish "~a select ~a" (widget-path nb) (widget-path tab)))

(defun notebook-events ()
  "<<NotebookTabChanged>>")


(defgeneric notebook-enable-traversal (nb))
(defmethod notebook-enable-traversal ((nb notebook))
  (format-wish "ttk::notebook::enableTraversal ~a" nb))

;; notebook-state
;; notebook-tabs

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
 
  (configure (hscroll st) "command" (concatenate 'string (widget-path (textbox st)) " xview"))
  (configure (vscroll st) "command" (concatenate 'string (widget-path (textbox st)) " yview"))
  (configure (textbox st) "xscrollcommand" (concatenate 'string (widget-path (hscroll st)) " set"))
  (configure (textbox st) "yscrollcommand" (concatenate 'string (widget-path (vscroll st)) " set"))
  )

(defgeneric append-text (txt text &rest tags))
(defmethod append-text ((txt scrolled-text) text &rest tags )
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}" (widget-path (textbox txt)) (tkescape text) tags)
  txt)

(defmethod text ((self scrolled-text))
  (text (textbox self)))

(defmethod (setf text) (new-text (self scrolled-text))
  (setf (text (textbox self)) new-text))

(defgeneric insert-object (txt object))
(defmethod insert-object ((txt scrolled-text) obj)
  (format-wish "~a window create end -window ~a" (widget-path (textbox txt)) (widget-path obj))
  txt)

(defgeneric see (txt pos))
(defmethod see ((txt scrolled-text) pos)
  (format-wish "~a see ~(~a~)" (widget-path (textbox txt)) pos)
  txt)

(defmethod see ((lb listbox) pos)
  (format-wish "~a see ~(~a~)" (widget-path lb) pos)
  lb)



;;; scale widget

#+:tk84
(defwrapper scale (tkvariable widget) () "scale")

#-:tk84
(defwrapper scale (tkvariable widget) () "ttk::scale")

(defmethod (setf command) (val (scale scale))
  (add-callback (name scale) val)					
  (format-wish "proc ~a-command {val} {callbackval ~a $val}" (name scale) (name scale))
  (format-wish "~a configure -command ~a-command" (widget-path scale) (name scale))
  val)

;;; spinbox widget

(defwrapper spinbox (tktextvariable widget) () "spinbox")

(defmethod (setf command) (val (sp spinbox))
  (add-callback (name sp) val)					
  (format-wish "~a configure -command {callbackstring ~a %s}" (widget-path sp) (name sp))
  val)

;;; toplevel (window) widget 

(defwrapper toplevel (widget) 
  ((protocol-destroy :accessor protocol-destroy :initarg :on-close :initform nil)
   (title :accessor title :initform nil :initarg :title)
   ) 
  "toplevel"
  (when (title widget)
    (wm-title widget (title widget)))
  (unless (protocol-destroy widget)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {wm withdraw ~a}" (widget-path widget) (widget-path widget))))

(defun make-toplevel (master)
  (make-instance 'toplevel :master master))

;;; label widget

#+:tk84
(defwrapper label (tktextvariable widget) () "label")

#-:tk84
(defwrapper label (tktextvariable widget) () "ttk::label")

;(defun make-label (master text)
;  (make-instance 'label :master master  :text text))

;;; message widget

(defwrapper message (tktextvariable widget) () "message")

;;; scrollbar

#+:tk84
(defwrapper scrollbar (widget) () "scrollbar")

#-:tk84
(defwrapper scrollbar (widget) () "ttk::scrollbar")

(defun make-scrollbar(master &key (orientation "vertical"))
  (make-instance 'scrollbar :master master :orientation orientation))

(defclass scrolled-canvas (frame)
  ((canvas :accessor canvas)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defun make-scrolled-canvas (master)
  (make-instance 'scrolled-canvas :master master ))

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
 
  (configure (hscroll sc) "command" (concatenate 'string (widget-path (canvas sc)) " xview"))
  (configure (vscroll sc) "command" (concatenate 'string (widget-path (canvas sc)) " yview"))
  (configure (canvas sc) "xscrollcommand" (concatenate 'string (widget-path (hscroll sc)) " set"))
  (configure (canvas sc) "yscrollcommand" (concatenate 'string (widget-path (vscroll sc)) " set"))
  )

(defclass scrolled-frame (frame)
  ((frame-class :accessor frame-class :initform 'frame :initarg :frame-class)
   (canvas      :accessor canvas      :initform nil    :initarg :canvas)
   (inner :accessor interior)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

(defmethod reset-scroll ((sf scrolled-frame))
  (format-wish "after idle {resetScroll ~a}" (widget-path (canvas sf))))

(defmethod scroll-to-top ((sf scrolled-frame))
  (format-wish "~a yview moveto 0" (widget-path (canvas sf)))
  )

;  (flush-wish))


(defmethod initialize-instance :after ((sf scrolled-frame) &key background)
  (let* ((canvas (make-instance 'canvas :master sf :background background))
         (f (if background
                (make-instance (frame-class sf) :master canvas :background background)
                (make-instance (frame-class sf) :master canvas))))
    (setf (canvas sf) canvas)
    (setf (interior sf) f) ;; (make-instance 'frame :master f :background background))
    (setf (hscroll sf) (make-instance 'scrollbar :master sf :orientation "horizontal"))
    (setf (vscroll sf) (make-instance 'scrollbar :master sf :orientation "vertical"))
    (grid canvas 0 0 :sticky "news")
    (grid (hscroll sf) 1 0 :sticky "we")
    (grid (vscroll sf) 0 1 :sticky "ns")
    (grid-columnconfigure sf 0 "weight" 1)
    (grid-columnconfigure sf 1 "weight" 0)
    (grid-rowconfigure sf 0 "weight" 1)
    (grid-rowconfigure sf 1 "weight" 0)
    (format-wish 
     "
~a configure -xscrollcommand [list ~a set] -yscrollcommand [list ~a set]
~a configure -command [list ~a xview]
~a configure -command [list ~a yview]
~a create window 10 10 -window ~a -anchor nw -tags f

after idle [list resetScroll ~a]

bind ~a <Configure> [list resetScroll ~a] 

"
     (widget-path canvas) (widget-path (hscroll sf)) (widget-path (vscroll sf))
     (widget-path (hscroll sf)) (widget-path canvas)
     (widget-path (vscroll sf)) (widget-path canvas)
     (widget-path canvas) (widget-path f)
     (widget-path canvas) 
     (widget-path f) (widget-path canvas)
     )
    ))


#+nil
(defclass scrolled-frame (frame)
  ((inner :accessor interior)
   (displayframe :accessor scrolled-frame-display)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   ))

#+nil
(defmethod initialize-instance :after ((sf scrolled-frame) &key background)
  (let ((f (make-instance 'frame :master sf :background background)))
    (setf (scrolled-frame-display sf) f)
    (setf (interior sf) (make-instance 'frame :master f :background background))
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
    (send-wish (format nil "~a set  0.1 0.5" (widget-path (hscroll sf))))
    (send-wish (format nil "~a set  0.1 0.5" (widget-path (vscroll sf))))
    (send-wish (format nil "~a configure -command ~axv" (widget-path (hscroll sf)) (name sf)))
    (send-wish (format nil "~a configure -command ~ayv" (widget-path (vscroll sf)) (name sf)))
    (send-wish (format nil "
proc ~axv {{com moveto} {val 0} {unit 0}} {

set x [winfo x ~a]
set y [winfo y ~a]
set wx [winfo width ~a]
set w [winfo width ~a]

if {$val < 0} {set val 0}
if {$val > [expr 1.0*($wx-$w)/$wx]} {set val  [expr 1.0*($wx-$w)/$wx]}
if {$wx<$w} { set val 0 }
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
if {$wy<$h} { set val 0 }
place ~a -x $x -y [expr -($val * $wy)]
set y [winfo y ~a]
~a set [expr -1.0*$y/$wy] [expr 1.0*($h-$y)/$wy]
}

" (name sf)
  (widget-path (interior sf)) (widget-path (interior sf)) (widget-path (interior sf))
  (widget-path f)  (widget-path (interior sf))  (widget-path (interior sf))		   
  (widget-path (hscroll sf))
  (name sf)   (widget-path (interior sf))  (widget-path (interior sf))
  (widget-path (interior sf))  (widget-path f)  (widget-path (interior sf))
  (widget-path (interior sf))    (widget-path (vscroll sf))
  ))
    (format-wish "bind ~a <Configure> {ltkdebug \"~a configure\";~axv configure;~ayv configure}" (widget-path sf) (name sf)(name sf)(name sf))
    (format-wish "bind ~a <Configure> {ltkdebug \"~a iconfigure\";~axv configure;~ayv configure}" (widget-path (interior sf)) (name sf)(name sf)(name sf))
    ))

;;; separator widget

#-:tk84
(defwrapper separator (widget) () "ttk::separator")

;;; sizegrip widget

#-:tk84
(defwrapper sizegrip (widget) () "ttk::sizegrip")

;;; treeview widget

#-:tk84
(defwrapper treeview (tktextvariable widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   (items   :accessor items   :initform nil :initarg :items))
  "ttk::treeview")

(defclass treeitem (tkobject)
  ((tree :accessor tree :initform nil :initarg :tree)
   (text :accessor text :initform nil :initarg :text)
   (image :accessor image :initform nil :initarg :image)
   (master :accessor master :initarg :master :initform nil)
   (tag    :accessor tag    :initform nil :initarg :tag)
   (column-values :accessor column-values :initform nil :initarg :column-values)
   ))

(defmethod initialize-instance :after ((item treeitem) &key)
  (setf (name item) (create-name))
  (format-wish "~a insert ~a end -id ~a -text \"~a\" ~@[-tag ~a~] ~@[-image ~a~]" (widget-path (tree item)) (if (master item)
                                                                                    (name (master item))
                                                                                    "{}")
               (name item) (tkescape (text item)) (tag item) (and (image item) (if (stringp (image item))
                                                                        (image item)
                                                                        (name (image item)))))
  (push item (items (tree item)))
  item)

(defmethod (setf text) (val (item treeitem))
  (format-wish "~a item ~a -text {~A}" (widget-path (tree item)) (name item) val)
  val)

(defmethod (setf image) (val (item treeitem))
  (format-wish "~a item ~a -image {~A}" (widget-path (tree item)) (name item) val)
  val)

(defmethod see ((tv treeview) (item treeitem))
  (format-wish "~a see ~a" (widget-path tv) (name item))
  tv)

(defgeneric children (tree item))
(defmethod children ((tree treeview) item)
  (format-wish "~a children ~a" (widget-path tree) item))

(defmethod children ((tree treeview) (item treeitem))
  (format-wish "~a children ~a" (widget-path tree) (name item)))

(defgeneric (setf children) (val tree item))
(defmethod (setf children) (val (tree treeview) item)
  (format-wish "~a children ~a {~{~a~^ ~}}" (widget-path tree) item val))

(defmethod (setf children) (val (tree treeview) (item treeitem))
  (format-wish "~a children ~a {~{~a~^ ~}}" (widget-path tree) (name item) val))

(defgeneric column-configure (tree column option value &rest rest))
(defmethod column-configure ((tree treeview) column option value &rest rest)
  (format-wish "~a column ~a -~(~a~) {~a}~{ -~(~a~) {~(~a~)}~}" (widget-path tree) column
	       option value rest))

(defgeneric treeview-delete (tree items))
(defmethod treeview-delete ((tree treeview) item)
  (format-wish "~a delete {~a}" (widget-path tree) item))

(defmethod treeview-delete ((tree treeview) (item treeitem))
  (setf (items tree) (remove item (items tree)))
  (format-wish "~a delete {~a}" (widget-path tree) (name item)))

(defmethod treeview-delete ((tree treeview) (items cons))
   (format-wish "~a delete {~{~a~^ ~}}" (widget-path tree) items))

(defgeneric treeview-exists (tree item))
(defmethod treeview-exists ((tree treeview) item)
  (format-wish "senddata [~a exists ~a]" (widget-path tree) item)
  (= (read-data) 1))

(defgeneric treeview-focus (tree))
(defmethod treeview-focus ((tree treeview))
  (format-wish "senddatastring [~a focus]" (widget-path tree))
  (let ((name (read-data)))
    (find name (items tree) :key #'name :test #'equal)))

(defgeneric treeview-identify-item (tree x y))
(defmethod treeview-identify-item ((tree treeview) x y)
  (format-wish "senddatastring [~a identify row ~a ~a ]" (widget-path tree) x y)
  (let ((name (read-data)))
    (find name (items tree) :key #'name :test #'equal)))


(defgeneric (setf treeview-focus) (item tree))
(defmethod (setf treeview-focus) (item tree)
  (format-wish "~a focus ~a" (widget-path tree) item))

(defmethod (setf treeview-focus) ((item treeitem) tree)
  (format-wish "~a focus ~a" (widget-path tree) (name item)))

(defun dictionary-plist (string)
  "return a plist representing the TCL dictionary"
  ;; crude but rather effective
  (do* ((*package* (find-package :keyword))
        (length (length string))
        (plist nil)
        (key (position #\- string)
             (position #\- string :start (1+ val)))
        (val (position #\Space string :start (if key (1+ key) length))
             (position #\Space string :start (if key (1+ key) length))))
       ((null val)
        (reverse plist))
    (push (read-from-string string t t :start (1+ key)) plist)
    (push (read-from-string string t t :start (1+ val)) plist)))

(defun tk-princ (stream arg colon at)
  "Like princ (format ~a), but convert a lisp list to a Tk list."
  (declare (ignore colon at))
  (cond ((or (null arg)
             (and (stringp arg)
                  (string= arg "")))
         (format stream "{}"))
        ((listp arg)
         (format stream "{~{~/ltk::tk-princ/~^ ~}}" (mapcar #'tkescape arg)))
        (t
         (format stream "~a" (tkescape arg)))))

(defun treeview-insert (tree &rest options
                        &key (parent "{}") (index "end") (id (create-name)) &allow-other-keys)
  "Creates a new item.  Returns its id.  See also the treeitem class."
  ;; Remove the keys that aren't optional in Tcl.
  (remf options :parent)
  (remf options :index)
  (format-wish "~a insert ~a ~a~{ -~(~a~) ~/ltk::tk-princ/~}"
               (widget-path tree)
               parent
               index
               options)
  #| Note:
  It is tempting to use senddata/read-data and let Tk allocate an id.
  BAD IDEA!  Process swapping causes a massive slowdown (observed 100x longer).
  |#
  id)

(defun treeview-item (tree item &rest options)
  "Query or modify the options for the specified item."
  (cond
    ((second options) ;; modify
     (format-wish "~a item ~a~{ -~(~a~) ~/ltk::tk-princ/~}"
                  (widget-path tree) item options))
    (t ;; query
     (format-wish "senddatastring [~a item ~a ~@[ -~(~a~)~]]"
                  (widget-path tree) item (car options))
     (read-data))))

(defun treeview-column (tree column &rest options)
  "Query or modify the options for the specified column."
  (cond
    ((second options) ;; modify
     (format-wish "~a column ~a~{ -~(~a~) ~/ltk::tk-princ/~}"
                  (widget-path tree) column options))
    (t ;; query
     (format-wish "senddatastring [~a column ~a ~@[ -~(~a~)~]]"
                  (widget-path tree) column (car options))
     (read-data))))

(defun treeview-heading (tree column &rest options
                         &key command &allow-other-keys
                         &aux (path (widget-path tree)))
  "Query or modify the heading options for the specified column."
  (cond
    ((second options) ;; modify
     (when command
       ;; register the callback
       (let ((cbname (format nil "~a:~a" path column)))
         (add-callback cbname command)
         (setf (getf options :command)
               (concatenate 'string "{callback " cbname "}"))))
     (format-wish "~a heading ~a~{ -~(~a~) ~/ltk::tk-princ/~}"
                  path column options))
    (t ;; query
     (format-wish "senddatastring [~a heading ~a ~@[ -~(~a~)~]]"
                  path column (car options))
     (read-data))))

(defun treeview-move (tree item &optional parent index)
  "Moves item to position index in parent's list of children."
  (format-wish "~a move ~a ~a ~a"
               (widget-path tree)
               item
               (or parent "{}")
               (or index "end")))

(defgeneric treeview-get-selection (w))
(defmethod treeview-get-selection ((tv treeview))
  (format-wish "senddatastrings [~a selection]" (widget-path tv))
  (let ((names (read-data))
        (items (items tv)))
    (mapcar (lambda (name)
              (find name items :key #'name :test #'equal))
            names)))

(defgeneric treeview-set-selection (w items))
(defmethod treeview-set-selection ((tv treeview) items)
  (format-wish "~a selection set {~{~a ~}}" (widget-path tv) (mapcar #'name items)))




(defclass scrolled-treeview (frame)
  ((treeview :accessor treeview)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)))

(defmethod initialize-instance :after ((st scrolled-treeview) &key)
  (setf (hscroll st) (make-scrollbar st :orientation "horizontal"))
  (setf (vscroll st) (make-scrollbar st))
  (setf (treeview st) (make-instance 'treeview :master st :xscroll (hscroll st) :yscroll (vscroll st)))
  (grid (treeview st) 0 0 :sticky "news")
  (grid (hscroll st) 1 0 :sticky "we")
  (grid (vscroll st) 0 1 :sticky "ns")
  (grid-columnconfigure st 0 :weight 1)
  (grid-columnconfigure st 1 :weight 0)
  (grid-rowconfigure st 0 :weight 1)
  (grid-rowconfigure st 1 :weight 0)
 
  (configure (hscroll st) "command" (concatenate 'string (widget-path (treeview st)) " xview"))
  (configure (vscroll st) "command" (concatenate 'string (widget-path (treeview st)) " yview"))
  (configure (treeview st) "xscrollcommand" (concatenate 'string (widget-path (hscroll st)) " set"))
  (configure (treeview st) "yscrollcommand" (concatenate 'string (widget-path (vscroll st)) " set")))
   

;;; canvas widget

(defwrapper canvas (widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   (scrollregion-x0 :accessor scrollregion-x0 :initform nil)
   (scrollregion-y0 :accessor scrollregion-y0 :initform nil)
   (scrollregion-x1 :accessor scrollregion-x1 :initform nil)
   (scrollregion-y1 :accessor scrollregion-y1 :initform nil)
   ) 
  "canvas"
  )

;; wrapper class for canvas items
(defclass canvas-item ()
  ((canvas :accessor canvas :initarg :canvas)
   (handle :accessor handle :initarg :handle))
  )

(defmethod print-object ((self canvas-item) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (when (slot-boundp self 'handle)
      (format stream "~a" (handle self)))))


(defmethod canvas ((canvas canvas)) canvas)

(defun make-canvas (master &key (width nil) (height nil) (xscroll nil) (yscroll nil))
  (make-instance 'canvas :master master :width width :height height :xscroll xscroll :yscroll yscroll))

(defgeneric scale (canvas factor &optional factory))
(defmethod scale ((canvas canvas) factor &optional factory)
  (format-wish "~a scale all 0 0 ~a ~a" (widget-path canvas) factor (or factory factor))
  canvas)

(defun move-all (canvas dx dy)
  (format-wish "~a move all ~a ~a" (widget-path canvas) dx dy)
  canvas)


(defgeneric bbox (item))
(defmethod bbox ((item canvas-item))
  (canvas-bbox (canvas item) (handle item)))

(defmethod bbox ((canvas canvas))
  (format-wish "senddata \"([~a bbox all])\"" (widget-path canvas))
  (read-data))


(defun canvas-bbox (canvas handle)
  (format-wish "senddata \"([~a bbox ~a])\"" (widget-path canvas) handle)
  (read-data))

(defgeneric calc-scroll-region (canvas))
(defmethod calc-scroll-region ((canvas canvas))
  (format-wish "~a configure -scrollregion [~a bbox all]" (widget-path canvas) (widget-path canvas))
  canvas)

(defgeneric set-coords (canvas item coords))

(defmethod set-coords (canvas item coords)
  (format-wish "~a coords ~a~{ ~a~}" (widget-path canvas) item coords)
  canvas)

(defmethod set-coords ((canvas canvas) (item canvas-item) (coords list))
  (set-coords canvas (handle item) coords))

(defgeneric set-coords* (canvas item &rest coords))

(defmethod set-coords* (canvas item &rest coords)
  (funcall #'set-coords canvas item coords))

(defmethod set-coords* ((canvas canvas) (item canvas-item) &rest coords)
  (funcall #'set-coords canvas (handle item) coords))

(defgeneric coords (item))
(defmethod coords ((item canvas-item))
     (list 0 0)				; not implemented yet
     )

(defun format-number (stream number)
  (cond
   ((complexp number)
    (format-number stream (realpart number))
    (format-number stream (imagpart number)))
   ((integerp number)
    (format stream " ~d" number))	    
   ((typep number 'single-float)
    (format stream " ~a" number))
   ((numberp number)
    (format-number stream (coerce number 'single-float)))
   ((null number)
    )
   ((listp number)
    (format-number stream (car number))
    (format-number stream (cdr number)))
   ((arrayp number)
    (dotimes (i (length number))
      (format-number stream (aref number i))))
   ))        

(defun process-coords (input)
  (with-output-to-string (s)
			 (format-number s input)))

(defgeneric (setf coords) (val item))

(defmethod (setf coords) (val (item canvas-item))
  (let ((coord-list (process-coords val)))
    (format-wish "~a coords ~a ~a" (widget-path (canvas item)) (handle item) coord-list)
    coord-list))

(defgeneric itembind (canvas w event fun))
(defmethod itembind ((canvas canvas) (item canvas-item) event fun)
  (itembind canvas (handle item) event fun))

(defmethod itembind ((canvas canvas) (item integer) event fun)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a bind ~a ~a {sendevent ~A %x %y %k %K %w %h %X %Y %b}" (widget-path canvas) item event name))
  canvas)

(defmethod tagbind ((canvas canvas) tag event fun &key exclusive)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a bind ~(~a~) ~a {sendevent ~A %x %y %k %K %w %h %X %Y %b ~:[~;;break~]}" (widget-path canvas) tag event name exclusive))
  canvas)


(defmethod bind ((w canvas-item) event fun &key append exclusive)
  (declare (ignore append exclusive))
  (itembind (canvas w) (handle w) event fun))

(defmethod tcl-bind ((w canvas-item) event code &key append exclusive)
  (declare (ignore append exclusive))
  (format-wish "~a bind ~a ~a {~a}"
               (widget-path (canvas w)) (handle w) event code))
  

(defgeneric scrollregion (canvas x0 y0 x1 y1))
(defmethod scrollregion ((c canvas) x0 y0 x1 y1)
  (setf (scrollregion-x0 c) (tk-number x0))
  (setf (scrollregion-y0 c) (tk-number y0))
  (setf (scrollregion-x1 c) (tk-number x1))
  (setf (scrollregion-y1 c) (tk-number y1))
  (configure c :scrollregion (format nil "~a ~a ~a ~a" (tk-number x0) (tk-number y0) (tk-number x1) (tk-number y1)))
  c)

(defgeneric canvasx (canvas screenx))
(defmethod canvasx ((canvas canvas) screenx)
  (format-wish "senddata [~a canvasx ~a]" (widget-path canvas) (tk-number screenx))
  (read-data))

(defgeneric canvasy (canvas screeny))
(defmethod canvasy ((canvas canvas) screeny)
  (format-wish "senddata [~a canvasy ~a]" (widget-path canvas) (tk-number screeny))
  (read-data))

(defgeneric itemmove (canvas item dx dy))
(defmethod itemmove ((canvas canvas) (item integer) dx dy)
  (format-wish "~a move ~a ~a ~a" (widget-path canvas) item (tk-number dx) (tk-number dy))
  canvas)

(defmethod itemmove ((canvas canvas) (item canvas-item) dx dy)
  (itemmove (canvas item) (handle item) (tk-number dx) (tk-number dy)))

(defmethod itemmove ((canvas canvas) item dx dy)
  (format-wish "~a move {~/ltk::down/} ~a ~a" (widget-path canvas) item (tk-number dx) (tk-number dy)))

(defgeneric itemdelete (canvas item))
(defmethod itemdelete ((canvas canvas) (item integer))
  (format-wish "~a delete ~a" (widget-path canvas) item)
  canvas)

(defmethod itemdelete ((canvas canvas) (item canvas-item))
  (format-wish "~a delete ~a" (widget-path canvas) (handle item))
  canvas)

(defgeneric move (item dx dy))
(defmethod move ((item canvas-item) dx dy)
  (itemmove (canvas item) (handle item) (tk-number dx) (tk-number dy)))

(defgeneric clear (widget))
(defmethod clear ((canvas canvas))
  "delete all items within a canvas"
  (format-wish "~a delete all" (widget-path canvas))
  canvas)

;; canvas item functions

(defun create-line (canvas coords)
  (format-wish "senddata [~a create line ~a]" (widget-path canvas) (process-coords coords))
  (read-data))

(defun create-line* (canvas &rest coords)
  (funcall #'create-line canvas coords))

(defclass canvas-line (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-line) &key canvas coords)
  (setf (handle c) (create-line canvas coords)))

(defun make-line (canvas coords)
  (make-instance 'canvas-line :canvas canvas :coords coords))

(defun create-polygon (canvas coords)
  (format-wish "senddata [~a create polygon ~a]" (widget-path canvas) (process-coords coords))
  (read-data))

(defclass canvas-polygon (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-polygon) &key canvas coords)
  (setf (handle c) (create-polygon canvas coords)))

(defun make-polygon (canvas coords)
  (make-instance 'canvas-polygon :canvas canvas :coords coords))

(defun create-oval (canvas x0 y0 x1 y1)
  (format-wish "senddata [~a create oval ~a ~a ~a ~a]" (widget-path canvas)
               (tk-number x0) (tk-number y0)
               (tk-number x1) (tk-number y1))
  (read-data))

(defclass canvas-oval (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-oval) &key canvas x0 y0 x1 y1)
  (setf (handle c) (create-oval canvas x0 y0 x1 y1)))

(defun make-oval (canvas x0 y0 x1 y1)
  (make-instance 'canvas-oval :canvas canvas :x0 x0 :y0 y0 :x1 x1 :y1 y1))


(defun create-rectangle (canvas x0 y0 x1 y1)
  (format-wish "senddata [~a create rectangle ~a ~a ~a ~a]" (widget-path canvas)
               (tk-number x0) (tk-number y0) (tk-number x1) (tk-number y1))
  (read-data))

(defclass canvas-rectangle (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-rectangle) &key canvas x0 y0 x1 y1)
  (setf (handle c) (create-rectangle canvas x0 y0 x1 y1)))

(defun make-rectangle (canvas x0 y0 x1 y1)
  (make-instance 'canvas-rectangle :canvas canvas :x0 x0 :y0 y0 :x1 x1 :y1 y1))


(defun create-item-command (canvas item stream)
  "Create the tk command string for creating a canvas item according to the item spec.
   The item spec has the format '(itemtype args @rest)
   Where itemtype is the type of item to create, args its mandatory arguments
   and rest any number of option value pairs.
   Understood item types are:
     :rectangle x0 y0 y1 y2
     :arc x0 y0 y1 y2
     :line x0 y0 x1 y1
     :text x y text
     :ctext x y text
"
  (labels ((arg ()
             (pop item))
           (number ()
             (tk-number (pop item)))
           (args ()
             (loop
                while item
                do
                (format stream " -~(~a~) {~/ltk::down/}" (pop item) (pop item)))))
    (let ((itemtype (pop item))
          (cpath (widget-path canvas)))
      (when (consp itemtype)
        (setf itemtype (car itemtype)))
      (cond
        ((eq itemtype :rectangle)
         (format stream "~a create rectangle ~a ~a ~a ~a " cpath (number) (number) (number) (number))
         (args))

        ((eq itemtype :polygon)
         (format stream "~a create polygon ~a" cpath (process-coords (pop item)))
         (args))

        ((eq itemtype :arc)
         (format stream "~a create arc ~a ~a ~a ~a " cpath (number) (number) (number) (number))
         (args))

        ((eq itemtype :oval)
         (format stream "~a create oval ~a ~a ~a ~a " cpath (number) (number) (number) (number))
         (args))
      
        ((eq itemtype :line)
         (format stream "~a create line ~a ~a ~a ~a " cpath (number) (number) (number) (number))
         (args))

	((eq itemtype :image)
         (format stream "~a create image ~a ~a " cpath (number) (number))
         (args))

        ((eq itemtype :text)
         (format stream "~a create text ~a ~a -anchor nw -text {~a} "
                 cpath (number) (number) (tkescape (arg)))
         (args))
      
        ((eq itemtype :ctext)
         (format stream "~a create text ~a ~a -anchor n -text {~a} "
                 cpath (number) (number) (tkescape (arg)))
         (args))
        ))))

(defun create-items (canvas items)
  "Create canvas items according to the item specs without returning ltk objects for them.
   This means, they cannot be accessed in any way, but also the creation does not flush
   the ltk output buffer."
  (let ((code (with-output-to-string (s)
                (dolist (item items)
                  (create-item-command canvas item s)
                  (format s "~%")))))
    (send-wish code)))

(defun make-items (canvas items)
  "Create canvas items according to the item specs and return a list of canvas-items."
  (let ((code (with-output-to-string (s)
                (format s "senddata \"( ~%")
                (dolist (item items)
                  (format s " [")
                  (create-item-command canvas item s)
                  (format s " ]~%"))
                (format s ")\"~%"))))
    (send-wish code)
    (let ((handles (read-data)))
      ;;(format t "data: ~s~%" erg) (finish-output)
      (loop for handle in handles as (itemtype) in items collect
           (let ((class (if (consp itemtype)
                            (cdr itemtype)
                            'canvas-item)))
             (make-instance class :canvas canvas :handle handle))))))

(defun create-text (canvas x y text)
  (format-wish "senddata [~a create text ~a ~a -anchor nw -text {~a}]" (widget-path canvas)
               (tk-number x) (tk-number y)
               text)
  (read-data))

(defclass canvas-text (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-text) &key canvas x y text)
  (setf (handle c) (create-text canvas x y text)))

(defun create-image (canvas x y &key image)
  (format-wish "senddata [~a create image ~a ~a -anchor nw~@[ -image ~a~]]" (widget-path canvas)
               (tk-number x) (tk-number y)
	       (and image (name image)))
  (read-data))

(defclass canvas-image (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-image) &key canvas x y image)
  (setf (handle c) (create-image canvas x y :image image)))

(defun image-setpixel (image data x y &optional x2 y2 )
  (format-wish "~A put {~{{~:{#~2,'0X~2,'0X~2,'0X ~} } ~} } -to ~a ~a~@[ ~a~]~@[ ~a~]" (name image) data x y x2 y2)
  image)

(defun create-bitmap (canvas x y &key (bitmap nil))
  (format-wish "senddata [~a create image ~a ~a -anchor nw~@[ -bitmap ~a~]]" (widget-path canvas)
               (tk-number x) (tk-number y)
	       (and bitmap (name bitmap)))
  (read-data))


(defun create-arc (canvas x0 y0 x1 y1 &key (start 0) (extent 180) (style "pieslice"))
  (format-wish "senddata [~a create arc ~a ~a ~a ~a -start ~a -extent ~a -style ~a]"
	       (widget-path canvas)
               (tk-number x0) (tk-number y0) (tk-number x1) (tk-number y1) start extent style)
  (read-data))

(defclass canvas-arc (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-arc) &key canvas x0 y0 x1 y1 (start 0) (extent 180) (style "pieslice"))
  (setf (handle c) (create-arc canvas x0 y0 x1 y1 :start start :extent extent :style style)))


(defclass canvas-window (canvas-item)
  ())

(defmethod initialize-instance :after ((c canvas-window) &key canvas (x 0) (y 0) widget (anchor :nw))
  (setf (handle c) (create-window canvas x y widget :anchor anchor)))

(defun create-window (canvas x y widget &key (anchor :nw))
  (format-wish "senddata [~a create window ~a ~a -anchor ~(~a~) -window ~a]"
 	       (widget-path canvas) (tk-number x) (tk-number y) anchor (widget-path widget))
  (read-data))

(defun postscript (canvas filename &key rotate pagewidth pageheight)
  (if (and (scrollregion-x0 canvas)
	   (scrollregion-x1 canvas)
	   (scrollregion-y0 canvas)
	   (scrollregion-y1 canvas))
      (format-wish "~a postscript -file ~a -x ~a -y ~a -width ~a -height ~a~@[ -rotate ~a~]~@[ -pagewidth ~a~]~@[ -pageheight ~a~]"
		(widget-path canvas) filename
		(scrollregion-x0 canvas) (scrollregion-y0 canvas)
		(- (scrollregion-x1 canvas) (scrollregion-x0 canvas))
		(- (scrollregion-y1 canvas) (scrollregion-y0 canvas))
		rotate pageheight pagewidth
		)
    (format-wish "~a postscript -file ~a" (widget-path canvas) filename))
  canvas)

;;; text widget

(defwrapper text (widget)
  ((xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
  )  "text")

(defmethod cursor-index ((text text))
  (format-wish "senddatastring [~a index insert]" (widget-path text))
  (let* ((index (split (read-data) ".")))
        (values (parse-integer (first index))
                (parse-integer (second index)))))

(defun make-text (master &key (width nil) (height nil))
  (make-instance 'text :master master :width width :height height))

(defmethod append-text ((txt text) text &rest tags)
  (format-wish "~a insert end \"~a\" {~{ ~(~a~)~}}" (widget-path txt) (tkescape text) tags)
  txt)

(defmethod insert-object ((txt text) obj)
  (format-wish "~a window create end -window ~a" (widget-path txt) (widget-path obj))
  txt)

(defun append-newline (text)
  (append-text text (coerce '(#\Linefeed) 'string)))

(defgeneric clear-text (txt))
(defmethod clear-text ((txt text))
  (format-wish "~A delete 0.0 end" (widget-path txt))
  txt)

(defmethod see((txt text) pos)
  (format-wish "~a see ~(~a~)" (widget-path txt) pos)
  txt)

(defgeneric search-all-text (text pattern))
(defmethod search-all-text ((txt text) pattern)
  (format-wish "searchall ~a ~a" (widget-path txt) pattern)
  txt)

(defgeneric search-next-text (text pattern))
(defmethod search-next-text ((txt text) pattern)
  (format-wish "searchnext ~a ~a" (widget-path txt) pattern)
  txt)

(defgeneric tag-configure (txt tag option value &rest others))
(defmethod tag-configure ((txt text) tag option value &rest others)
  (format-wish "~a tag configure ~a~{ -~(~a~) {~/ltk::down/}~}" (widget-path txt)
	       (if (stringp tag)
		   tag
		 (format nil "~(~a~)" tag))
	       (list* option value others))
  txt)

(defgeneric tag-bind (txt tag event fun))
(defmethod tag-bind ((txt text) tag event fun)
  "bind fun to event of the tag of the text widget txt"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "~a tag bind ~a ~a {callback ~A}" (widget-path txt) tag event name)
    )
  txt)

(defmethod text ((text text))
  (format-wish "senddatastring [~a get 1.0 end]" (widget-path text))
  (read-data))

(defmethod (setf text) (val (text text))
  (format-wish "~A delete 0.0 end;~A insert end {~A}" (widget-path text) (widget-path text) val)
  val)

(defgeneric save-text (txt filename))
(defmethod save-text ((txt text) filename)
  "save the content of the text widget into the file <filename>"
  (format-wish "set file [open {~a} \"w\"];puts $file [~a get 1.0 end];close $file;puts \"asdf\"" filename (widget-path txt))
  (read-line (wish-stream *wish*))
  txt)

(defgeneric load-text (txt filename))
(defmethod load-text((txt text) filename)
  "load the content of the file <filename>"
;  (format-wish "set file [open {~a} \"r\"];~a delete 1.0 end;~a insert end [read $file];close $file;puts \"asdf\"" filename (widget-path txt) (widget-path txt))
  (format-wish "set file [open {~a} \"r\"];~a delete 1.0 end;~a insert end [read $file];close $file;puts \"(:DATA asdf)\"" filename (widget-path txt) (widget-path txt))
  (read-data))

;;; photo image object

(defclass photo-image(tkobject)
  ((data :accessor data :initform nil :initarg :data)
   (file :accessor file :initform nil :initarg :file)))

(defmethod widget-path ((photo photo-image))
  (name photo))

(defmethod initialize-instance :after ((p photo-image)
                                       &key width height format grayscale data file)
  (check-type data (or null string))
  (setf (name p) (create-name))
  (format-wish "image create photo ~A~@[ -width ~a~]~@[ -height ~a~]~@[ -format \"~a\"~]~@[ -grayscale~*~]~@[ -data ~s~]~@[ -file ~s~]"
               (name p) width height format grayscale data (and file (tkescape2 file))))

(defun make-image ()
  (let* ((name (create-name))
	 (i (make-instance 'photo-image :name name)))
    ;(create i)
    i))

(defgeneric image-load (p filename))
(defmethod image-load((p photo-image) filename)
  ;(format t "loading file ~a~&" filename)
  (send-wish (format nil "~A read {~A} -shrink" (name p) filename))
  p)

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
  (format-wish "pack ~A -side ~(~A~) -fill ~(~A~)~@[~* -expand 1~]~@[ -after ~A~]~@[ -before ~A~]~@[ -padx ~A~]~@[ -pady ~A~]~@[ -ipadx ~A~]~@[ -ipady ~A~]~@[ -anchor ~(~A~)~]"
          (widget-path w) side fill expand (and after (widget-path after)) (and before (widget-path before)) padx pady ipadx ipady anchor)
  w)

(defmethod pack ((list list) &rest rest)
  (mapcar #'(lambda (w)
              (apply #'pack w rest))
	  list))

(defgeneric pack-propagate (widget flag))
(defmethod pack-propagate ((w widget) flag)
  (format-wish "pack propagate ~A ~A"
	       (widget-path w)
	       (if flag "true" "false"))
  w)

(defgeneric pack-forget (widget))
(defmethod pack-forget ((w widget))
  (format-wish "pack forget ~A" (widget-path w))
  w)

(defgeneric pack-forget-all (widget))
(defmethod pack-forget-all ((w widget))
  "removes all widgets packed into w"
  (format-wish "foreach slave [pack slaves ~A] { pack forget $slave}" (widget-path w))
  w)




;;; place manager

(defgeneric place (widget x y &key anchor bordermode width height in relheight relwidth relx rely))
(defmethod place (widget x y &key anchor width bordermode height in relheight relwidth relx rely)
  (format-wish "place ~A -x ~A -y ~A~@[ -anchor ~a~]~@[ -width ~a~]~@[ -height ~a~]~@[ -bordermode ~a~]~@[ -in ~a~]~@[ -relheight ~a~]~@[ -relwidth ~a~]~@[ -relx ~a~]~@[ -rely ~a~]" (widget-path widget)
               (tk-number x) (tk-number y)
               anchor
               (tk-number width)
               (tk-number height)
               bordermode in (tk-number relheight) (tk-number relwidth) (tk-number relx) (tk-number rely)
               )
  widget)

(defgeneric place-forget (widget))
(defmethod place-forget ((w widget))
  (format-wish "place forget ~A" (widget-path w))
  w)

;;; grid manager

(defgeneric grid (widget r c &key columnspan ipadx ipady padx pady rowspan sticky))
(defmethod grid ((w widget) row column &key columnspan ipadx ipady padx pady rowspan sticky)
  (format-wish "grid ~a -row ~a -column ~a~@[ -columnspan ~a~]~@[ -ipadx ~a~]~
             ~@[ -ipady ~a~]~@[ -padx ~a~]~@[ -pady ~a~]~@[ -rowspan ~a~]~
             ~@[ -sticky ~(~a~)~]" (widget-path w) row column columnspan ipadx ipady padx pady rowspan sticky)
  w)

(defgeneric grid-columnconfigure (widget c o v))
(defmethod grid-columnconfigure (widget column option value)
  (format-wish "grid columnconfigure ~a ~a -~(~a~) {~a}" (widget-path widget) column option value)
  widget)

(defgeneric grid-rowconfigure (widget r o v))
(defmethod grid-rowconfigure (widget row option value)
  (format-wish "grid rowconfigure ~a ~a -~(~a~) {~a}" (widget-path widget) row option value)
  widget)

(defgeneric grid-configure (widget o v))
(defmethod grid-configure (widget option value)
  (format-wish "grid configure ~a -~(~a~) {~a}" (widget-path widget) option value)
  widget)

(defgeneric grid-forget (widget))
(defmethod grid-forget ((w widget))
  (format-wish "grid forget ~A" (widget-path w))
  w)

;;; configure a widget parameter

(defun down (stream object colon at)
  "Print OBJECT to STREAM, downcasing unless OBJECT is a string, and giving the path of tkobjects."
  (declare (ignore colon at))
  (typecase object
    (string (write-string object stream))
    (tkobject (write-string (widget-path object) stream))
    (t (format stream "~(~a~)" object))))

(defgeneric configure (widget option value &rest others))
(defmethod configure (widget option value &rest others)
  (format-wish "~A configure~{ -~(~a~) {~/ltk::down/}~}"
               (widget-path widget)
               (list* option value others))
  widget)

(defmethod configure ((item menuentry) option value &rest others)
  (let ((path (widget-path (master item))))
    (format-wish "~A entryconfigure [~A index {~A}]~{ -~(~a~) {~/ltk::down/}~}"
                 path
                 path
                 (text item)
                 (list* option value others)))
  item)

(defmethod configure ((item canvas-item) option value &rest others)
  (format-wish "~A itemconfigure ~A~{ -~(~a~) {~/ltk::down/}~}"
               (widget-path (canvas item)) (handle item)
               (list* option value others))
  item)

(defmethod tag-configure ((c canvas) tag option value &rest others)
  (format-wish "~a itemconfigure ~a~{ -~(~a~) {~/ltk::down/}~}" (widget-path c)
	       (if (stringp tag)
		   tag
		 (format nil "~(~a~)" tag))
	       (list* option value others))
  c)

;;; for tkobjects, the name of the widget is taken
(defmethod configure (widget option (value tkobject) &rest others)
  (format-wish "~A configure -~(~A~) {~A} ~{ -~(~a~) {~(~a~)}~}" (widget-path widget) option (widget-path value) others)
  widget)

(defgeneric cget (widget option))
(defmethod cget ((widget widget) option)
  (format-wish "senddatastring [~a cget -~(~a~)]" (widget-path widget) option)
  (read-data))

;(defun background (widget)
;  (cget widget :background))

#-:gcl
;(defun (setf background) (val widget)
;  (configure widget :background val))

#|
(defmacro defoption (option)
  `(progn
     (defun ,option (widget)
       (cget widget "asdf"))
     (export ,option)))

(defoption fill)
|#

(defgeneric itemconfigure (widget item option value))

(defmethod itemconfigure ((widget canvas) item option value)
  (format-wish "~A itemconfigure ~A -~(~A~) {~A}" (widget-path widget) item option
	    (if (stringp value) ;; There may be values that need to be passed as
		value           ;; unmodified strings, so do not downcase strings
	      (format nil "~(~a~)" value))) ;; if its not a string, print it downcased
  widget)


;;; for tkobjects, the name of the widget is taken
(defmethod itemconfigure ((widget canvas) item option (value tkobject))
  (format-wish "~A itemconfigure ~A -~(~A~) {~A}" (widget-path widget) item option (widget-path value))
  widget)

(defgeneric itemlower (w i &optional below))
(defmethod itemlower ((widget canvas) item &optional below)
  (format-wish "~A lower ~A ~@[~A~]" (widget-path widget)
	       item below)
  widget)

(defmethod lower ((item canvas-item) &optional below)
  (itemlower (canvas item) (handle item) (and below (handle below))))

(defgeneric itemraise (w i &optional above))
(defmethod itemraise ((widget canvas) item &optional above)
  (format-wish "~A raise ~A ~@[~A~]" (widget-path widget)
	       item above)
  widget)

(defmethod raise ((item canvas-item) &optional above)
  (itemraise (canvas item) (handle item) (and above (handle above))))

;;; grab functions

(defgeneric grab (toplevel &key global))
(defmethod grab ((toplevel widget) &key global)
  (format-wish "grab set ~:[~;-global~] ~a" global (widget-path toplevel))
  toplevel)

(defgeneric grab-release (toplevel))
(defmethod grab-release ((toplevel widget))
  (format-wish "grab release ~a" (widget-path toplevel))
  toplevel)


;;; font functions
;; use {~/ltk::down/} on the font name to match itemconfigure
 
;;(defun font-actual ...)

(defun font-configure (name &key family size weight slant underline overstrike)
  (format-wish "font configure {~/ltk::down/}~@[ -family ~a~]~@[ -size ~a~]~@[ -weight ~(~a~)~]~@[ -slant ~(~a~)~]~@[ -underline ~a~]~@[ -overstrike ~a~]"
               name family size weight slant underline overstrike))

(defun font-create (name &key family size weight slant underline overstrike)
  (format-wish "senddatastring [font create {~/ltk::down/}~@[ -family ~a~]~@[ -size ~a~]~@[ -weight ~(~a~)~]~@[ -slant ~(~a~)~]~@[ -underline ~a~]~@[ -overstrike ~a~]]"
               name family size weight slant underline overstrike)
   (read-data))
 
(defun font-delete (&rest names)
  (format-wish "font delete~{ {~/ltk::down/}~}" names))

;;(defun font-families ...)
;;(defun font-measure ...)

 (defun font-metrics (font)
   (format-wish "sendpropertylist [font metrics {~/ltk::down/}]" font)
   (read-data))
 
;;(defun font-names ...)

(defun font-families ()
  (send-wish "senddatastrings [font families]")
  (read-data))


;;; wm functions

(defgeneric resizable (widget x y))
(defmethod resizable ((tl widget) x y)
  (format-wish "wm resizable ~a ~a ~a" (widget-path tl) x y)
  tl)

(defgeneric set-wm-overrideredirect (widget value))
(defmethod set-wm-overrideredirect ((w widget) val)
  (format-wish "wm overrideredirect ~a ~a" (widget-path w) val)
  w)

(defgeneric wm-title (widget title))
(defmethod wm-title ((w widget) title)
  (format-wish "wm title ~a {~a}" (widget-path w) title)
  w)

#-:tk84
(defgeneric wm-manage (widget))
#-:tk84
(defmethod wm-manage ((w widget))
  (format-wish "wm manage ~a" (widget-path w))
  w)

#-:tk84
(defgeneric wm-forget (widget))
#-:tk84
(defmethod wm-forget ((w widget))
  (format-wish "wm forget ~a" (widget-path w))
  w)
  

(defgeneric wm-state (widget))
(defmethod wm-state ((w widget))
  (format-wish "senddatastring [wm state ~a]" (widget-path w))
  (read-wish))

(defgeneric (setf wm-state) (new-state widget))
(defmethod (setf wm-state) (new-state (w widget))
  (format-wish "wm state ~a ~a" (widget-path w) new-state)
  new-state)

(defgeneric minsize (widget x y))
(defmethod minsize ((w widget) x y)
  (format-wish "wm minsize ~a ~a ~a" (widget-path w)
               (tk-number x) (tk-number y))
  w)

(defgeneric maxsize (widget x y))
(defmethod maxsize ((w widget) x y)
  (format-wish "wm maxsize ~a ~a ~a" (widget-path w) (tk-number x) (tk-number y))
  w)

(defgeneric withdraw (toplevel))
(defmethod withdraw ((tl widget))
  (format-wish "wm withdraw ~a" (widget-path tl))
  tl)

(defgeneric normalize (toplevel))
(defmethod normalize ((tl widget))
  (format-wish "wm state ~a normal" (widget-path tl))
  tl)

(defgeneric iconify (toplevel))
(defmethod iconify ((tl toplevel))
  (format-wish "wm iconify ~a" (widget-path tl))
  tl)

(defgeneric deiconify (toplevel))
(defmethod deiconify ((tl toplevel))
  (format-wish "wm deiconify ~a" (widget-path tl))
  tl)

(defgeneric geometry (toplevel))
(defmethod geometry ((tl widget))
  (format-wish "senddatastring [wm geometry ~a]" (widget-path tl))
  (let ((str (read-data)))
    (multiple-value-bind (width letterx) (parse-integer str :junk-allowed t)
      (assert (char= (char str letterx) #\x))
      (multiple-value-bind (height letter+)
          (parse-integer str :start (1+ letterx) :junk-allowed t)
        (assert (or (char= (char str letter+) #\+)
                    (char= (char str letter+) #\-)))
        (multiple-value-bind (x letter+)
            (parse-integer str :start (1+ letter+) :junk-allowed t)
          (assert (or (char= (char str letter+) #\+)
                      (char= (char str letter+) #\-)))
          (let ((y (parse-integer str :start (1+ letter+))))
            (when (char= (char str letter+) #\-)
              (let ((sh (screen-height tl)))
                (setf y (- sh y))))
            (list width height x y)))))))

(defgeneric (setf geometry) (geometry widget))
(defmethod (setf geometry) (geometry (tl widget))
  (format-wish "wm geometry ~a ~a" (widget-path tl) geometry)
  geometry)

(defgeneric set-geometry (toplevel width height x y))
(defmethod set-geometry ((tl widget) width height x y)
  ;;(format-wish "wm geometry ~a ~ax~a+~a+~a" (widget-path tl) width height x y)
  (format-wish "wm geometry ~a ~ax~a+~D+~D" (widget-path tl)
               (tk-number width) (tk-number height) (tk-number x) (tk-number y))
  tl)

(defgeneric set-geometry-wh (toplevel width height))
(defmethod set-geometry-wh ((tl widget) width height)
  (format-wish "wm geometry ~a ~ax~a" (widget-path tl)
               (tk-number width) (tk-number height))
  tl)

(defgeneric set-geometry-xy (toplevel x y))
(defmethod set-geometry-xy ((tl widget) x y)
  (format-wish "wm geometry ~a +~D+~D" (widget-path tl) (tk-number x) (tk-number y))
  tl)
 
(defgeneric on-close (toplevel fun))
(defmethod on-close ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {callback ~A}" (widget-path tl) name))
  tl)

(defmethod on-close ((tl widget) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {callback ~A}" (widget-path tl) name))
  tl)

(defmethod on-close ((tl (eql *tk*)) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol . WM_DELETE_WINDOW {callback ~A}" name)
    tl))

(defgeneric on-focus (toplevel fun))
(defmethod on-focus ((tl toplevel) fun)
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "wm protocol WM_TAKE_FOCUS {callback ~A}"
	      name))
  tl)

(defun iconwindow (tl wid)
  (format-wish "wm iconwindow ~a ~a" (widget-path tl) (widget-path wid))
  tl)  

;;; winfo functions

(defun screen-width (&optional (w nil))
  "give the width of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenwidth ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-height (&optional (w nil))
  "give the height of the screen in pixels (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenheight ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-width-mm (&optional (w nil))
  "give the width of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenmmwidth ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-height-mm (&optional (w nil))
  "give the height of the screen in mm (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo screenmmheight ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-mouse-x (&optional (w nil))
  "give x position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo pointerx ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-mouse-y (&optional (w nil))
  "give y position of the mouse on screen (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata [winfo pointery ~a]" (if w (widget-path w) "."))
  (read-data))

(defun screen-mouse (&optional (w nil))
  "give the position of the mouse on screen as (x y) (if w is given, of the screen the widget w is displayed on)"
  (format-wish "senddata \"([winfo pointerxy ~a])\"" (if w (widget-path w) "."))
  (let ((vals (read-data)))
    (values (first vals) (second vals))))

(defun window-id (tl)
  "get the window id of the toplevel"
  (format-wish "senddatastring [winfo id ~a]" (widget-path tl))
  (read-data))

(defun window-width (tl)
  "give the width of the toplevel in pixels"
  (format-wish "senddata [winfo width ~a]" (widget-path tl))
  (read-data))

(defun window-height (tl)
  "give the height of the toplevel in pixels"
  (format-wish "senddata [winfo height ~a]" (widget-path tl))
  (read-data))

(defun window-x (tl)
  "give the x position of the toplevel in pixels"
  (format-wish "senddata [winfo rootx ~a];flush $server" (widget-path tl))
  (read-data))

(defun window-y (tl)
  "give the y position of the toplevel in pixels"
  (format-wish "senddata [winfo rooty ~a];flush $server" (widget-path tl))
  (read-data))

(defun window-transient (tl win)
  "set the transient property of tl to be transient to win or nil"
  (format-wish "wm transient ~a ~a" (widget-path tl) (if win
                                                         (widget-path win)
                                                         "{}")))

;;; misc functions

#-:tk84
(defun use-theme(name)
  (format-wish "ttk::style theme use ~a" name))

#-:tk84
(defun theme-names ()
  (send-wish "senddatastrings [ttk::style theme names]")
  (ltk::read-data))



(defun focus (widget)
  (format-wish "focus ~a" (widget-path widget))
  widget)

(defun force-focus (widget)
  (format-wish "focus -force ~a" (widget-path widget))
  widget)

(defun set-focus-next (widget next)
  (format-wish "bind ~a <Tab> { focus ~a; break }" (widget-path widget) (widget-path next)))


;;; Dialog functions

(defun choose-color (&key parent title initialcolor )
  (format-wish "senddatastring [tk_chooseColor ~@[ -parent ~A~]~@[ -title {~A}~]~@[ -initialcolor {~A}~]]" (when parent (widget-path parent)) title initialcolor)
  (read-data))

(defun get-open-file (&key (filetypes '(("All Files" "*")))
			   (initialdir (namestring *default-pathname-defaults*))
			   multiple parent title)
  (let ((files
        (with-output-to-string (s)
          (format s "{")
          (dolist (type filetypes)
            (let ((name (first type))
                  (wildcard (second type)))
              (format s "{{~a} {~a}} " name wildcard)))
          (format s "}"))))
    (if multiple
	(format-wish "senddatastrings [tk_getOpenFile ~
                      -filetypes ~a ~@[ -initialdir {~a}~] -multiple 1 ~
                      ~@[ -parent ~a~] ~@[ -title {~a}~]]"
		      files initialdir 
		      (and parent (widget-path parent)) title)
	(format-wish "senddatastring [tk_getOpenFile ~
                      -filetypes ~a ~@[ -initialdir {~a}~]  ~
                      ~@[ -parent ~a~] ~@[ -title {~a}~]]"
		      files initialdir 
		      (and parent (widget-path parent)) title))
    (read-data)))

(defun get-save-file (&key (filetypes '(("All Files" "*"))))
  (let ((files
        (with-output-to-string (s)
          (format s "{")
          (dolist (type filetypes)
            (let ((name (first type))
                  (wildcard (second type)))
              (format s "{{~a} {~a}} " name wildcard)))
          (format s "}"))))
    (format-wish "senddatastring [tk_getSaveFile -filetypes ~a]" files)
    (read-data)))

(defun choose-directory (&key (initialdir (namestring *default-pathname-defaults*))
			      parent title mustexist)
  (format-wish "senddatastring [tk_chooseDirectory ~@[ -initialdir \"~a\"~]~@[ -parent ~a ~]~@[ -title {~a}~]~@[ -mustexist ~a~]]" (tkescape2 initialdir) (and parent (widget-path parent)) title (and mustexist 1))
  (read-data))

(defvar *mb-icons* (list "error" "info" "question" "warning")
  "icon names valid for message-box function")

;;; see make-string-output-string/get-output-stream-string
(defun message-box (message title type icon &key parent)
  ;;; tk_messageBox function
  (format-wish "senddatastring [tk_messageBox -message \"~a\" -title {~a} -type ~(~a~) -icon ~(~a~)~@[ -parent ~a~]]" (tkescape2 message) title type icon (and parent (widget-path parent)))
  (read-keyword))


(defun ask-yesno(message &key (title "") parent)
  (equal (message-box message title "yesno" "question" :parent parent) :yes))

(defun ask-okcancel(message &key (title "") parent)
  (equal (message-box message title "okcancel" "question" :parent parent) :ok))

(defun do-msg(message &key (title "") parent)
  (message-box message title "ok" "info" :parent parent))

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

(defun cm (tree widget-path)
  (cond
   ((eq tree :separator)
    (send-wish (format nil "~A add separator" widget-path)))
   ((listp (second tree))
    (let ((newpath (format nil "~A.~A" widget-path (create-name))))
      (when (and (equal widget-path ".menubar")
		 (or (equal (first tree) "Help")
		     (equal (first tree) "help")
		     (equal (first tree) "Hilfe")))
	(setf newpath ".menubar.help"))
      (send-wish (format nil "menu ~A -tearoff 0" newpath))
      (send-wish (format nil "~a add cascade -label \"~a\" -menu ~a" widget-path (first tree) newpath))
      (dolist (entry (second tree))
	(cm entry newpath))))
   (t
    (let* ((name (create-name)))
      (add-callback name (second tree))		     
      (send-wish (format nil "~A add command -label {~A} -command {puts -nonewline  {(\"~A\")};flush $server}" widget-path (first tree) name))
      ))))

(defun create-menu2 (menutree)
  (send-wish (format nil "menu .menubar -tearoff 0 -type menubar"))
  (dolist (e menutree)
    (cm e ".menubar"))
  (send-wish (format nil ". configure -menu .menubar"))
  )  


;;;; Error handling

(defparameter *debug-settings-table*
  (copy-tree
   '(((0 :minimum) . nil)
     ((1 :deploy)  . production-condition-handler)
     ((2 :develop) . graphical-condition-handler)
     ((3 :maximum) . paranoid-condition-handler))))

(defun debug-setting-condition-handler (debug-setting)
  "Given a debug setting (see WITH-LTK for details), return the debugger class to use."
  (let* ((debug (if (numberp debug-setting)
		    (min 3 (max 0 (ceiling debug-setting)))
		    debug-setting))
	 (cons (assoc (list debug) *debug-settings-table* :test #'intersection)))
    (cond
      ((or (typep debug 'class)
           (and (symbolp debug-setting)
                (ignore-errors (find-class debug))))
       debug)
      (cons (cdr cons))
      (t (error "Unknown debug setting ~S" debug)))))

(defun make-call-with-condition-handlers-function (handler-class)
  "Return a function that will call a thunk with the appropriate condition handlers in place."
  (if handler-class
      (let ((warning-handler (make-condition-handler-function
                              :class handler-class :title "Warning"))
            (error-handler (make-condition-handler-function
                            :class handler-class :title "Error"))
            (generic-handler (make-condition-handler-function
                              :class handler-class :title "Attention")))
        (lambda (thunk)
          (handler-bind ((condition generic-handler)
                         (warning warning-handler)
                         (error error-handler))
            (funcall thunk))))
      #'funcall))

;;;; main event loop, runs until stream is closed by wish (wish exited) or
;;;; the variable *exit-mainloop* is set

(defvar *exit-mainloop* nil)
(defvar *break-mainloop* nil)

(defun break-mainloop ()
  (setf *break-mainloop* t))

(defgeneric handle-output (key params))

(defmethod handle-output (key params)
  (declare (ignore key params)))

(defun process-one-event (event)
  (when event
    (when *debug-tk*
      (format *trace-output* "l:~s<=~%" event)
      (finish-output *trace-output*))
    (cond
     ((and (not (listp event))
           *trace-tk*)
      (princ event *trace-output*)
      (finish-output *trace-output*))
     ((not (listp event)) nil)
     ((eq (first event) :callback)
      (let ((params (rest event)))
        (callback (first params) (rest params))))
     ((eq (first event) :event)
      (let* ((params (rest event))
             (callback (first params))
             (evp (rest params))
             (event (construct-tk-event evp)))
        (callback callback (list event))))
      ((eq (first event) :keepalive)
       (format *trace-output* "Ping from wish: ~{~A~^~}~%" (rest event))
       (finish-output *trace-output*))
      ((eq (first event) :debug)
       (tcldebug (second event)))
     (t
      (handle-output
       (first event) (rest event))))))

(defun process-events ()
  "A function to temporarliy yield control to wish so that pending
events can be processed, useful in long loops or loops that depend on
tk input to terminate"
  (let (event)
    (loop 
     while (setf event (read-event :blocking nil))
     do (with-atomic (process-one-event event)))))

(defparameter *inside-mainloop* ())

(defun main-iteration (&key (blocking t) (reentrant? nil))
  "The heart of the main loop.  Returns true as long as the main loop should continue."
  (let ((no-event (cons nil nil)))
    (labels ((proc-event ()
	       (let ((event (read-event :blocking blocking
					:no-event-value no-event)))
		 (cond
		   ((null event)
                    (ignore-errors (close (wish-stream *wish*)))
                    (exit-wish)
		    nil)
		   ((eql event no-event)
		    t)
		   (t (with-atomic (process-one-event event))
		      (cond
			(*break-mainloop* nil)
			(*exit-mainloop*
			 (exit-wish)
			 nil)
			(t t)))))))
      ;; For recursive calls to mainloop, we don't want to setup our ABORT and EXIT restarts.
      ;; They make things too complex.
      (if reentrant?
	  (proc-event)
	  (restart-case (proc-event)
	    (abort ()
	      :report "Abort handling Tk event"
	      t)
	    (exit ()
	      :report "Exit Ltk main loop"
	      nil))))))

(defun mainloop (&key serve-event)
  (let ((reentrant? (member *wish* *inside-mainloop*))
	(*inside-mainloop* (adjoin *wish* *inside-mainloop*)))
    (cond
      (serve-event (install-input-handler))
      ((wish-input-handler *wish*)
       (let ((*exit-mainloop* nil)
             (*break-mainloop* nil))
         (loop until (or *break-mainloop* *exit-mainloop*)
               do (serve-event))))
      (t (let ((*exit-mainloop* nil)
               (*break-mainloop* nil))
	   (if reentrant?
	       (loop while (main-iteration :reentrant? t))
	       (loop while (with-ltk-handlers ()
			     (main-iteration)))))))))

;;; Event server

#-(or sbcl cmu)
(progn
  (defun install-input-handler ()
    (error "SERVE-EVENT is not implemented on this system"))
  (defun remove-input-handler ()
    nil)
  (defun serve-event ()
    (error "SERVE-EVENT is not implemented on this system")))

#+(or sbcl cmu)
(progn
  (defun add-fd-handler (fd direction function)
    #+sbcl (sb-sys:add-fd-handler fd direction function)
    #+cmu (system:add-fd-handler fd direction function))

  (defun remove-fd-handler (handler)
    #+sbcl (sb-sys:remove-fd-handler handler)
    #+cmu (system:remove-fd-handler handler))

  (defun serve-event ()
    #+sbcl (sb-sys:serve-event)
    #+cmu (system:serve-event))
  
  (defun fd-stream-fd (stream)
    #+sbcl (sb-sys:fd-stream-fd stream)
    #+cmu (system:fd-stream-fd stream))

  (defun make-input-handler (wish)
    "Return a SERVE-EVENT input handler."
    (let ((fd-stream (two-way-stream-input-stream (wish-stream wish))))
      (labels ((call-main ()
		 (with-ltk-handlers () 
		   (handler-bind ((stream-error
				   ;; If there was a stream error on the fd that
				   ;; we're listening to, we need to remove the
				   ;; input handler to avoid getting stuck in an
				   ;; infinite loop of stream errors making
				   ;; noise on the fd, causing us to try to read
				   ;; from it, causing an error, which makes
				   ;; noise on the fd...
				   (lambda (e)
				     (when (eql (stream-error-stream e) fd-stream)
				       (return-from call-main nil)))))
		     (catch wish (main-iteration :blocking nil)))))
	       (ltk-input-handler (fd)
		 (declare (ignore fd))
		 (let ((*wish* wish)) ; use the wish we were given as an argument
                   (if (find wish *inside-mainloop*)
                       (call-main)
                       (let ((*exit-mainloop* nil)
                             (*break-mainloop* nil))
                         (unless (call-main)
                           (remove-input-handler)))))))
	#'ltk-input-handler)))

  (defun install-input-handler ()
    (unless (wish-input-handler *wish*)
      (let ((fd (fd-stream-fd (two-way-stream-input-stream (wish-stream *wish*)))))
	(setf (wish-input-handler *wish*)
	      (add-fd-handler fd :input (make-input-handler *wish*))
              *exit-mainloop* nil
              *break-mainloop* nil))))

  (defun remove-input-handler ()
    (remove-fd-handler (wish-input-handler *wish*))
    (setf (wish-input-handler *wish*) nil)))

;;

(defun filter-keys (desired-keys keyword-arguments)
  (loop for (key val) on keyword-arguments by #'cddr
	when (find key desired-keys) nconc (list key val)))

;;; wrapper macro - initializes everything, calls body and then mainloop

(defmacro with-ltk ((&rest keys &key (debug 2) stream serve-event &allow-other-keys)
		    &body body)
  "Create a new Ltk connection, evaluate BODY, and enter the main loop.

  :DEBUG indicates the level of debugging support to provide.  It can be a
  number from 0 to 3, or one of the corresponding keywords:
  :minimum, :deploy, :develop, or :maximum.

  If :SERVE-EVENT is non-NIL, Ltk will use SERVE-EVENT handlers instead of a
  blocking main loop.  This is only supported on SBCL and CMUCL.  Note that
  using SERVE-EVENT means that WITH-LTK will return immediately after evaluating
  BODY.

  If :STREAM is non-NIL, it should be a two-way stream connected to a running
  wish.  This will be used instead of running a new wish."
  (declare (ignore debug serve-event stream))
  `(call-with-ltk (lambda () ,@body) ,@keys))

(defun call-with-ltk (thunk &rest keys &key (debug 2) stream serve-event remotep
                      &allow-other-keys)
  "Functional interface to with-ltk, provided to allow the user the build similar macros."
  (declare (ignore stream))
  (flet ((start-wish ()
           (apply #'start-wish
                  :remotep remotep
                  (append (filter-keys '(:stream :debugger-class :debug-tcl)
                                       keys)
                          (list :debugger-class (debug-setting-condition-handler debug)))))
         (mainloop () (apply #'mainloop (filter-keys '(:serve-event) keys))))
    (let ((*wish* (make-ltk-connection :remotep remotep)))
      (catch *wish*
        (unwind-protect
             (progn
               (start-wish)
               (multiple-value-prog1
                   (with-ltk-handlers ()
                     (with-atomic (funcall thunk)))
                 (mainloop)))
          (unless serve-event
            (exit-wish)))))))
       
;;; with-widget stuff

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun process-layout (line parent)
    (let ((class-name (first line))
	  (instance-name (second line)))
      (multiple-value-bind (keyargs subwidgets)
	  (do ((params (cddr line))	; all other parameters to the widget/subwidget defs
	       (keywords+values nil)    ; keyword args for the widget
	       (sublists nil))		; list of the subwidgets	      
 	      ((null params) (values (reverse keywords+values) (reverse sublists)))
 	    (cond ((listp (car params))
 		   (dolist (subwidget (process-layout (pop params) instance-name))
 		     (push subwidget sublists)))
 		  (t (push (pop params) keywords+values)
 		     (push (pop params) keywords+values))))
	(cons
	 (list instance-name
	       (append
		(list 'make-instance (list 'quote class-name))
		(if parent (list :master parent) nil)
		keyargs))
	 subwidgets))))

  (defmacro with-widgets (layout &rest body)
    (let* ((defs (process-layout layout nil))
	   (widgets (mapcar #'car defs)))
      `(let* ,defs
	 (declare (ignorable ,@widgets))
	 ,@body)))  
  )


;; defwidget the better version :)
(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defmacro defwidget (namespec parent slots widgetspecs &rest body)
    (let* ((name (if (listp namespec)
                     (second namespec)
                     namespec))
           (selfname (if (listp namespec)
                         (first namespec)
                         'self)))
      (unless name
        (error "defwidget: no name given"))
      
      (unless (listp parent)
        (error "defwidget: parent class(es) specifier \"~a\" needs to be a list of symbols" parent))
      
      (unless (listp slots)
        (error "defwidget: slots \"~a\" needs to be a list of slot specifiers" slots))
      
      (unless (listp widgetspecs)
        (error "defwidget: widgets \"~a\" need to be a list of widget specifiers" widgetspecs))
      
      (when (null widgetspecs)
        ;; (warn "defwidget: widget list is empty.")
        )
      
      (let (defs wnames events accessors methods)
        (labels ((on-type (subwidget methodname)
                   "Handle an :on-type form"
                   (push
                    `(bind ,subwidget "<KeyPress>"
                           (lambda (event)
                             (,methodname ,selfname (event-char event))))
                    events)
                   (push `(declaim (ftype function ,methodname))
                         methods)
                   ;; 		 (push
                   ;; 		  `(defgeneric ,methodname (self key))
                   ;; 		  methods)
                   ;; 		 (push
                   ;; 		  `(defmethod ,methodname ((,selfname ,name) keycode))
                   ;; 		  methods)
                   )

                 (process-layout (line parent)
                   (let ((instance-name (first line))
                         (class-name (second line)))
                     (multiple-value-bind (keyargs subwidgets)
                         (do ((params (cddr line))       ; all other parameters to the
                                        ; widget/subwidget defs
                              (keywords+values nil)      ; keyword args for the widget
                              (sublists nil))	       ; list of the subwidgets	      
                             ((null params) (values (reverse keywords+values) (reverse sublists)))
                           (cond ((listp (car params))
                                  (dolist (subwidget (process-layout (pop params) instance-name))
                                    (push subwidget sublists)))
                                 ((equal (car params) :on-type)
                                  (pop params)
                                  (on-type instance-name (pop params)))
                                 (t (let* ((param (pop params))
                                           (val (pop params)))
                                      (push param keywords+values)
                                      (push (if (equal param :pack)
                                                (list 'quote val)
                                                val)
                                            keywords+values)))))
                       (cons
                        (list instance-name
                              (append
                               (list 'make-instance (list 'quote class-name))
                               (if parent (unless (member :master keyargs)
                                            (list :master parent)) nil)
                               keyargs))
                        subwidgets))))
                 
                 (compute-slots (names)
                   (mapcar (lambda (name)
                             (if (listp name)
                                 name
                                 `(,name :accessor ,name :initform nil
                                         :initarg ,(intern (symbol-name name) :keyword))))
                           names))
                 
                 (make-accessor (acname spec)
                   (push `(declaim (ftype function ,acname (setf ,acname)))
                         accessors)
                   (push
                    `(defmethod ,acname ((,selfname ,name))
                       ,spec)
                    accessors)
                   (push
                    `(defmethod (setf ,acname) (val (,selfname ,name))
                       (setf ,spec val))
                    accessors))
                 
                 (grab-accessors ()
                   (loop while (equal (caar body) :accessor)
                         do (destructuring-bind (unused aname aspec)
                                (pop body)
                              (declare (ignore unused))
                              (make-accessor aname aspec)))))
          
          (setf defs (mapcan (lambda (w)
                               (process-layout w selfname)) widgetspecs))
          (setf wnames (mapcar #'car defs))
          (grab-accessors)
          (let* ((all-slots (compute-slots (append slots wnames))))
            `(progn
               (defclass ,name ,parent
                 ,all-slots)
               ,@(reverse accessors)
               ,@(reverse methods)

               (defmethod initialize-instance :after ((,selfname ,name) &key)
                 (let ,wnames
                   ,@(mapcar (lambda (def)
                               (append (list 'setf) def)) defs)
                   ,@(mapcar (lambda (wname)
                               `(setf (,wname ,selfname) ,wname)) wnames)

                   ,@events
                   ,@body))

               )))))))

;;:on-type test-widget-type


;; example-usage

(defwidget test-widget (frame) 
  (a b c)
  ((bu button :text "A button" 
       :pack (:side :top :anchor :w)
       :command (lambda ()
		  (format t "the content of enry is:~a~%" (text entry)) (finish-output)
		  (setf (text entry) "")))
   (f1 frame :pack (:side :top :fill :both :expand t) 
       (lb label :text "A label" :pack (:side :left))
       (entry entry :pack (:side :left :fill :x :expand t) :text "")))

  ;; other code here
  )

(defgeneric firstline (widget))
(defgeneric (setf firstline) (val widget))
(defgeneric secondline (widget))
(defgeneric (setf secondline) (val widget))
(defgeneric entry-typed (widget keycode))

(defwidget (this test-widget2) (frame)
  ()
  ((mw test-widget :pack (:side :top :fill :x))
   (mw2 test-widget :pack (:side :top :fill :x))
   (b button :text "set" :command (lambda ()
				    (setf (text (entry mw)) "foo")
				    (setf (text (entry mw2)) "bar"))
      :pack (:side :top))
   (e entry :pack (:side :top) :text ""
      :on-type entry-typed))
   
   (:accessor firstline (text (entry (mw this))))
   (:accessor secondline (text (entry (mw2 this))))


   (bind this "<Enter>" (lambda (event)
			  (declare (ignore event))
			  (format t "Entered!~%") (finish-output)))
   )


(defmethod entry-typed ((self test-widget2) keycode)
  (format t "typed:~a~%text:~a~%" keycode (text (e self))) (finish-output))

(defun defwidget-test ()
  (declare (optimize (debug 3)))
  (with-ltk ()
    (let ((mw (make-instance 'tw)))
      (format t "mw is: ~a~%" mw) (finish-output)
      ;(error "test: ~a~%" mw)
      (pack mw :side :top :fill :both :expand t)
      (pack (make-instance 'button :text "get first"
			   :command (lambda ()
				      (format t "the first is: ~a~%" (firstline mw))
				      (finish-output))) :side :top :fill :both :expand t))))

;;;


(defun with-widgets-test ()
  (with-ltk ()
    (with-widgets
	(toplevel top-frame :title "with-widgets-test"
		  (label lb1 :text "Test, Test!" :pack '(:side :top))
		  (entry en1 :pack '(:side :top) :text "")
		  (frame fr1 :pack '(:side :bottom)
			 (button bt1 :text "OK" :pack '(:side :right)
				 :command (lambda () (format t "Pressed OK~%")))
			 (button bt2 :text "CANCEL" :pack '(:side :left)
				 :command (lambda () (withdraw top-frame)))))
      (setf (text lb1) "Test, Test, Test!")
      )))

;;;; testing functions

;; notebook
(defun nbtest ()
  (with-ltk ()
    (let* ((nb (make-instance 'notebook))
	   (f1 (make-instance 'frame :master nb))
	   (f2 (make-instance 'frame :master nb))
	   (t1 (make-instance 'text :master f1 :width 40 :height 10))
	   (b1 (make-instance 'button :master f1 :text "Press me"
			      :command (lambda ()
					 (format t "the index is:~a~%" (notebook-index nb f1))
					 (finish-output))))
	   (b2 (make-instance 'button :master f2 :text "A button"
			      :command (lambda ()
					 (format t "the index is:~a~%" (notebook-index nb f2))
					 (finish-output)))))
      (pack nb :fill :both :expand t)  
      (pack t1 :fill :both :expand t)
      (pack b1 :side :top)
      (pack b2 :side :top)
      (notebook-add nb f1 :text "Frame 1")
      (notebook-add nb f2 :text "Frame 2")
      (append-text t1 "Foo Bar Baz")
      )))

(defwidget nbw (frame)
  ()
  ((nb notebook :pack (:fill :both :expand t)
       (f1 frame
           (t1 text :width 60 :height 20 :pack (:side :top))
           (b1 button :text "Press Me" :pack (:side :top)
               :command (lambda ()
                          (format t "the index is:~a~%" (notebook-index nb f1))
                          (finish-output))))
       (f2 frame
           (b2 button :text "A button" :pack (:side :top)
               :command (lambda ()
                          (format t "the index is:~a~%" (notebook-index nb f2))
                          (finish-output))))))
  (notebook-add nb f1 :text "Frame 1")
  (notebook-add nb f2 :text "Frame 2")
  (append-text t1 "Foo Bar Baz"))

(defun nbt2 ()
  (with-ltk ()
    (let ((w (make-instance 'nbw)))
      (pack w :side :top :fill :both :expand t))))

;;; ltktest

(defvar *do-rotate* nil)
(defvar *demo-line* nil)
(defvar *demo-canvas* nil)

(defun eggs (radio)
  (format t "Prepare ~a eggs.~%"
          (case (value radio)
            (1 "fried")
            (2 "stirred")
            (3 "cooked")))
  (finish-output))

;;;; default ltk test
(defun ltktest()
  (with-ltk (:debug-tcl nil)
      (let* ((bar (make-instance 'frame))
             (fradio (make-instance 'frame :master bar))
             (leggs (make-instance 'label :master fradio :text "Eggs:"))
             (r1 (make-instance 'radio-button :master fradio :text "fried" :value 1 :variable "eggs"))
             (r2 (make-instance 'radio-button :master fradio :text "stirred" :value 2 :variable "eggs"))
             (r3 (make-instance 'radio-button :master fradio :text "cooked" :value 3 :variable "eggs"))
             #-:tk84
	     (combo (make-instance 'combobox :master fradio :text "foo" :values '(foo bar baz)))

	     #-:tk84
	     (fprogress (make-instance 'frame :master bar))
	     #-:tk84
	     (lprogress (make-instance 'label :master fprogress :text "Progress:"))
	     #-:tk84
	     (progress (make-instance 'progressbar :master fprogress :value 0 :length 150))
	     #-:tk84
	     (bprogress (make-instance 'button :text "Step" :command 
				       (lambda ()
					 (incf (value progress) 10)
					 (when (> (value progress) 100)
					     (setf (value progress) 0)))
				       :master fprogress))


	     #-:tk84
	     (fscale (make-instance 'frame :master bar))
	     #-:tk84
	     (scale (make-instance 'scale :master fscale :from 0 :to 100  :length 150 ))
	     #-:tk84
	     (separator (make-instance 'separator :master fscale))
	     
	     (fcheck (make-instance 'frame :master bar))
	     (lcheck (make-instance 'label :master fcheck :text "Add:"))
	     (ch1 (make-instance 'check-button :master fcheck :text "Salt"))
	     (ch2 (make-instance 'check-button :master fcheck :text "Pepper"))

	     (fstyles (make-instance 'frame :master bar))
	     (lstyles (make-instance 'label :master fstyles :text "Tk styles:"))
	     (fr (make-instance 'frame :master bar))
	     (lr (make-instance 'label :master fr :text "Rotation:"))
	     (bstart (make-instance 'button :master fr :text "Start" :command 'start-rotation))
	     (bstop  (make-instance 'button :master fr :text "Stop"  :command 'stop-rotation))
	     (b1 (make-instance 'button :master bar :text "Hallo"
				:command (lambda ()
					   (format t "Hallo~%")
					   (finish-output))))
	     (b2 (make-instance 'button :master bar :text  "Welt!"
				:command (lambda ()
					   (format t "Welt~%")
					   (finish-output))))
	     (f (make-instance 'frame :master bar))
	     (l (make-instance 'label :master f :text "Test:"))
	     (b3 (make-instance 'button :master f :text  "Ok." :command 'test-rotation))
	     (e (make-instance 'entry :master bar))
	     (b4 (make-instance 'button :master bar :text "get!"
				:command (lambda ()
					   (format t "content of entry:~A~%" (text e))
					   (finish-output))))
	     (b5 (make-instance 'button :master bar :text "set!"
				:command (lambda ()
                                           (setf (text e) "test of set"))))
	     (sc (make-instance 'scrolled-canvas :borderwidth 2 :relief :raised))
	     (c (canvas sc))
	     (lines nil)
	     (mb (make-menubar))
	     (mfile (make-menu mb "File" ))
	     (mf-load (make-menubutton mfile "Load" (lambda () ;(error "asdf")
						      (format t "Load pressed~&")
						      (finish-output))
				       :underline 1))
	     (mf-save (make-menubutton mfile "Save" (lambda ()
						      (format t "Save pressed~&")
						      (finish-output))
				       :underline 1))
	     (sep1 (add-separator mfile))
	     (mf-export (make-menu mfile "Export..."))
	     (sep2 (add-separator mfile))
	     (mf-print (make-menubutton mfile "Print" (lambda () (postscript c "wt.ps"))))
	     (sep3 (add-separator mfile))
	     (mfe-jpg (make-menubutton mf-export "jpeg" (lambda ()
							  (format t "Jpeg pressed~&")
							  (finish-output))))
	     (mfe-gif (make-menubutton mf-export "png" (lambda ()
							 (format t "Png pressed~&")
							 (finish-output))))

             (mf-scale (make-menu mfile "Scale..."))
             (mfs-1 (make-menubutton mf-scale "0.5" (lambda ()
                                                      (scale c 0.5))))
             (mfs-2 (make-menubutton mf-scale "2" (lambda ()
                                                    (scale c 2))))
             (mfs-3 (make-menubutton mf-scale "2/0.5" (lambda ()
                                                        (scale c 2 0.5))))
             (mfs-4 (make-menubutton mf-scale "0.5/2" (lambda ()
                                                        (scale c 0.5 2))))
             (sep4 (add-separator mfile))
	     (mf-exit (make-menubutton mfile "Exit" (lambda () (setf *exit-mainloop* t))
				       :underline 1
				       :accelerator "Alt Q"))
	     (mp (make-menu nil "Popup"))
	     (mp-1 (make-menubutton mp "Option 1" (lambda () (format t "Popup 1~&") (finish-output))))
	     (mp-2 (make-menubutton mp "Option 2" (lambda () (format t "Popup 2~&") (finish-output))))
	     (mp-3 (make-menubutton mp "Option 3" (lambda () (format t "Popup 3~&") (finish-output))))
	     )
	(declare (ignore mf-print mf-exit mfe-gif mfe-jpg mf-save mf-load sep1 sep2 sep3 sep4
			 mp-1 mp-2 mp-3 mfs-1 mfs-2 mfs-3 mfs-4)) 


	#-:tk84
	(setf (value progress) 10)

	(configure scale :orient :horizontal)

	(bind *tk* "<Alt-q>" (lambda (event) (declare (ignore event)) (setf *exit-mainloop* t)))

	(bind c "<1>" (lambda (event) (popup mp (event-root-x event) (event-root-y event))))
	(configure c :borderwidth 2 :relief :sunken)
	(pack sc :side :top :fill :both :expand t)
	(pack bar :side :bottom)
	(pack fradio :side :top :fill :x)
	(pack (list leggs r1 r2 r3) :side :left)

        (pack fprogress :side :top :fill :x)
	(pack lprogress :side :left)
	(pack progress :side :left :fill :x :padx 10)
	(pack bprogress :side :left)
	

	(pack fscale :side :top :fill :x)
	(pack scale :side :left :fill :x :padx 20)
	(pack separator :side :left)
	(configure separator :orient :vertical)

	(pack fcheck :side :top :fill :x)
	(pack (list lcheck ch1 ch2) :side :left)
	(setf (value r1) 1)
        #-:tk84
	(pack combo :side :left)
        (dolist (r (list r1 r2 r3))
          (let ((button r))
            (setf (command r) (lambda (val)
                                (declare (ignore val))
                                (eggs button)))))
	(pack fstyles :side :top :fill :x)
	(pack lstyles :side :left)
	(let ((radio nil))
	  (dolist (theme (theme-names))
	    (let* ((theme theme)
		   (r (make-instance 'radio-button :master fstyles
				    :text theme
				    :value theme
				    :variable "radiostyles"
				    :command (lambda (val)
					       (declare (ignore val))
					       (use-theme theme)))))
	      (pack r :side :left)
	      (setf radio r)))
	  (setf (value radio) "default"))
	(scrollregion c 0 0 500 400)
	(pack fr :side :left)
	(pack lr :side :left)
	(configure fr :borderwidth 2 :relief :sunken)
	(pack bstart :side :left)
	(pack bstop :side :left)
	(pack b1 :side :left)
	(pack b2 :side :left)
	(configure f :borderwidth 2 :relief :sunken)
	(pack f :fill :x :side :left)
	(pack l :side :left)
	(pack b3 :side :left)
	(pack e :side :left)
	(pack b4 :side :left)
	(pack b5 :side :left)
	(dotimes (i 100)
	  (let ((w (* i 2.8001f0)))
	    (let ((x (+ 250 (* 150.0f0 (sin w))))
		  (y (+ 200 (* 150.0f0 (cos w)))))
	      (push y lines)
	      (push x lines)
	      )))
	(setf *demo-line* (create-line c lines))
	(setf *demo-canvas* c)
	(create-text c 10 10 "Ltk Demonstration")
	))
  0)

(defvar *angle* 0.0f0)
(defvar *angle2* 0.0f0)
(defvar *angle3* 0.0f0)
(declaim (single-float *angle* *angle2* *angle3*))

(defun rotate()
;  (declare (optimize speed)    (single-float *angle* *angle2* *angle3*))
  (let ((*debug-tk* nil))
    (let ((lines nil)
	  (dx (* 50 (sin *angle2*)))
	  (dy (* 50 (cos *angle2*)))
	  (wx (sin *angle3*))
;	  (wy (cos *angle3*))
	  )
      (incf *angle* 0.1f0)
      (incf *angle2* 0.03f0)
      (incf *angle3* 0.01f0)
      
      (dotimes (i 100)
        (declare (fixnum i))
	(let ((w (+ *angle* (* i 2.8001f0))))
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
  (time (dotimes (i 1000)
	  (rotate)
          (flush-wish)))
  (finish-output))

(defun start-rotation()
  (setf *do-rotate* t)
  (rotate))

(defun stop-rotation()
  (setf *do-rotate* nil))


;;;; the eyes :)

(defun ltk-eyes ()
  (with-ltk ()
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

(defmacro with-modal-toplevel ((var &rest toplevel-initargs) &body body)
  `(let* ((,var (make-instance 'toplevel ,@toplevel-initargs))
          (*exit-mainloop* nil)
          ;(*buffer-for-atomic-output* nil)
          )
     (send-wish "update idletasks")
    (unwind-protect
         (catch 'modal-toplevel
	   (block nil
             (on-close ,var (lambda () (return)))
	     (grab ,var)
	     (multiple-value-prog1
		 (progn ,@body)
               (flush-wish)
	       (mainloop))))
      (grab-release ,var)
      (withdraw ,var)
      (flush-wish))))

;;; Basic graphical error/warning/etc handling

(defun make-condition-handler-function (&key (class 'graphical-condition-handler)
                                        (title "Error"))
  "Create a function appropriate for use in a handler-bind."
  (declare (optimize (debug 3)))
  (if class
      (let ((prototype (make-instance class :prototype t)))
        (lambda (condition)
          (when (handle-condition-p prototype condition)
            (restart-case
                (with-modal-toplevel (tl :title title :height 40 :width 300)
                  (let ((debugger (make-instance class :master tl :condition condition)))
                    (on-close tl (lambda ()
                                   (abort-condition-handler debugger)
                                   (return)))
                    (pack debugger)))
              (send-to-debugger (condition)
                :report "Send condition to the debugger"
                (invoke-debugger condition))))))
      (constantly nil)))

;;; The protocol for graphical condition handlers. This, and they must
;;; take :master, :condition and :prototype initargs.

(defgeneric handle-condition-p (handler condition)
  (:documentation
   "Handlers will be asked about all signalled conditions, errors, warnings or otherwise.
    They may choose to handle or decline them via this method."))

(defgeneric handler-condition (handler)
  (:documentation "An accessor for the condition that this handler is handling. May be nil."))

(defgeneric debugp (handler)
  (:documentation "Should this handler offer to send the user into the debugger?"))

(defgeneric describe-condition (handler)
  (:documentation "A text description of the condition, to be presented to the user."))

(defgeneric compute-buttons (handler master)
  (:documentation
   "This method should return a list of buttons (generally for invoking restarts) to be packed."))

(defgeneric report-bug (handler)
  (:documentation "Report this condition as a bug to the developer."))

(defgeneric abort-condition-handler (handler)
  (:documentation
   "Get rid of this handler; called, for example, when the user closes the handler's window.
    A handler basically has two choices here:

    - Call cl:abort, which will abort the Ltk event that raised this condition.

    - Throw to ltk:modal-toplevel, which will leave the condition unhandled."))

(defclass ltk-condition-handler (frame)
  ((prototypep :initform nil :initarg :prototype
               :documentation
               "When set, do not actually create a Tk widget, this instance exists for purposes of generic-function dispatch only.")
   (condition :initform nil :initarg :condition :accessor handler-condition)))

(defmethod initialize-instance :around ((handler ltk-condition-handler) &key prototype
                                        &allow-other-keys)
  (if prototype
      handler
      (call-next-method)))

;;; Our default condition handler, meant for use by the developer
;;; writing an Ltk application. For other uses, this can be usefully
;;; subclassed and adapted.

(defclass graphical-condition-handler (ltk-condition-handler)
  ((debugp :initform t :initarg :debugp :accessor debugp)
   (details-pane :accessor details-pane
		 :documentation "The scrolled-text where we display the stacktrace.")))

(defmethod handle-condition-p ((handler graphical-condition-handler) condition)
  (typep condition 'serious-condition))

(defmethod initialize-instance :after ((handler graphical-condition-handler) &rest ignore)
  (declare (ignore ignore))
  (let* ((message (make-instance 'label
		    :master handler
		    :text (describe-condition handler)
		    :justify :left))
	 (buttons-frame (make-instance 'frame :master handler)))
    (setf (details-pane handler) (make-instance 'scrolled-text :master handler))
    (pack (mapc (lambda (button) (setf (master button) handler))
		(compute-buttons handler buttons-frame))
	  :side :left)
    (pack (list message buttons-frame))))

(defmethod describe-condition ((handler graphical-condition-handler))
  (let ((cont (find-restart 'continue))
	(cond (handler-condition handler)))
    (cond
      ((and cont cond) (format nil "~A~%~A?" cond cont))
      (cond (princ-to-string cond))
      (cont (format nil "An internal error has occured.~%~A?" cont))
      (t "An internal error has occured."))))

(defmethod compute-buttons ((handler graphical-condition-handler) master)
  (let ((exit (make-instance 'button :master master :text "Exit"))
        (ok (make-instance 'button :master master :text "Dismiss"))
	(yes (make-instance 'button :master master :text "Yes"))
	(no (make-instance 'button :master master :text "No"))
	(show (make-instance 'button :master master :text "Show Details"))
	(debugger (make-instance 'button :master master :text "Debugger"))
	(report (make-instance 'button :master master :text "Report Bug"))
	(details (details-pane handler))
	(backtrace-done? nil))
    (labels ((ensure-backtrace ()
	       (unless backtrace-done?
		 (append-text details
			      (with-output-to-string (out)
				(print-backtrace (handler-condition handler) out)))
		 (setf backtrace-done? t)))
	     (show ()
	       (ensure-backtrace)
	       (pack details)
	       (setf (text show) "Hide details"
		     (command show) #'hide))
	     (hide ()
	       (pack-forget details)
	       (setf (text show) "Show details"
		     (command show) #'show))
	     (give-up () (abort-condition-handler handler))
	     (debugger () (invoke-restart 'send-to-debugger (handler-condition handler)))
         (exit () (invoke-restart 'exit)))
      (setf (command ok) #'give-up
            (command exit) #'exit
	    (command yes) #'continue
	    (command no) #'abort
	    (command show) #'show
	    (command debugger) #'debugger
	    (command report) (lambda () (report-bug handler)))
      (append (when (find-restart 'exit)
                (list exit))
              (if (find-restart 'continue)
		  (list yes no)
		  (list ok))
	      (when (debugp handler) (list show debugger))
	      (list report)))))

(defun print-backtrace (error stream)
  (format stream "~A~%  [Condition of type ~A]"
	  error (class-name (class-of error)))
  #+allegro
  (loop for f = (excl::int-newest-frame)
	then (excl::int-next-older-frame f)
	while f
	do (terpri stream)
	  (debugger:output-frame stream f))
  #+sbcl (sb-debug:backtrace most-positive-fixnum stream)
  #+(or cmu scl) (debug:backtrace most-positive-fixnum stream)
  )

(defmethod report-bug ((handler graphical-condition-handler))
  ;; This is modal only because the earlier non-modal one sometimes
  ;; died before the error report could be sent. I'd rather not be
  ;; modal here, and Ltk users are encouraged to write their own bug
  ;; reporters that start a new Tcl/Tk process and send reports over
  ;; the net.
  (with-modal-toplevel (tl :title "Save a bug report")
    (let ((summary (make-instance 'label :master tl :text "Summary:"))
          (esummary (make-instance 'entry :master tl :text ""))
          (desc (make-instance 'message :master tl :width 400 :text "Please describe what you were doing, and where so the developers can try to reproduce this situation"))
          (edesc (make-instance 'scrolled-text :master tl))
          (bsave (make-instance 'button :master tl :text "Save")))
      (labels ((save ()
		 (let ((summary (text esummary))
		       (desc (text edesc))
		       (file (get-save-file)))
		   (with-open-file (out file :direction :output :if-exists :supersede)
		     (multiple-value-bind (sec min hr day mo yr)
			 (decode-universal-time (get-universal-time) 0)
		       (let ((timestamp (format nil "~D/~D/~D ~D:~D:~D GMT"
						yr mo day hr min sec)))
			 (format out "This bug report was generated by the Ltk debugger on ~A.~%"
				 timestamp)
			 (format out "Ltk version: ~A~%" *ltk-version*)
			 (format out "Lisp: ~A ~A~%"
				 (lisp-implementation-type) (lisp-implementation-version))
			 (format out "Summary: ~A~%Description:~%~A~%~%" summary desc)
			 (print-backtrace (handler-condition handler) out))))
		   (return))))
	(setf (command bsave) #'save)
        (grid-columnconfigure tl 0 :weight 0)
        (grid-columnconfigure tl 1 :weight 1)
        (grid summary 2 0 :sticky :w)
        (grid esummary 2 1 :sticky :ew)
        (grid desc 3 0 :columnspan 2 :sticky :w)
        (grid edesc 4 0 :columnspan 2 :sticky :ew)
        (grid bsave 5 0)))))

(defmethod abort-condition-handler ((handler graphical-condition-handler))
  (abort))

;;; The production error handler

(defclass production-condition-handler (graphical-condition-handler)
  ((debugp :initform nil)))

(defmethod describe-condition ((handler production-condition-handler))
  (let ((cont (find-restart 'continue)))
    (if cont
	(format nil "An internal error has occured.~%~A?" cont)
	"An internal error has occured")))

;;; Paranoid condition handler

(defclass paranoid-condition-handler (graphical-condition-handler) ())

(defmethod handle-condition-p ((debugger paranoid-condition-handler) condition)
  (declare (ignore condition))
  t)

(defmethod abort-condition-handler ((handler paranoid-condition-handler))
  (if (typep (handler-condition handler) 'serious-condition)
      (abort)
      (throw 'modal-toplevel nil)))

;;; Trivial debugger

(defun trivial-debugger (condition hook)
  "A function appropriate for *debugger-hook*. It prints a stack trace and exits."
  (declare (ignore hook))
  (ignore-errors
    (format *error-output* "An error of has occured: ~%")
    (print-backtrace condition *error-output*)
    #+sbcl (quit)
    #+(or cmu scl) (ext:quit)))

(defun debugger-test (debugger-class)
  (with-ltk (:debugger-class debugger-class :debug-tcl t)
    (pack (list (make-instance 'label :text (format nil "Debugger class ~S" debugger-class))
		(make-instance 'button
		  :text "Error"
		  :command (lambda () (error "This is an error.")))
		(make-instance 'button
		  :text "Continuable Error"
		  :command (lambda ()
                             (cerror "Keep going with this computation"
                                     "This is an error.")
                             (do-msg "You chose to continue!" :title "Congratulations")))
		(make-instance 'button
		  :text "Warning"
		  :command (lambda ()
                             (warn "This is a warning.")
                             (do-msg "After a warning was raised, computation continued."
                               :title "And then...")))
		(make-instance 'button
		  :text "Signal"
		  :command (lambda ()
                             (signal 'condition)
                             (do-msg "After a condition was signalled, computation continued."
                               :title "And then...")))))))

(defun input-box (prompt &key (title "Input") default)
  (let* ((*exit-mainloop* nil)
	 (ok t)
	 (w (make-instance 'toplevel :title title))
	 (l (make-instance 'label :master w :text prompt))
	 (e (make-instance 'entry :master w :width 40))
	 (f (make-instance 'frame :master w))
	 (b_ok (make-instance 'button :master f :text "Ok" 
			      :command (lambda ()
					 (break-mainloop)
					 )))
	 (b_cancel (make-instance 'button :master f :text "Cancel" 
				  :command (lambda ()
					     (setf ok nil)
					     (break-mainloop)
					     )))
	 )
    (pack l :side :top :anchor :w)
    (pack e :side :top)
    (pack f :side :top :anchor :e)
    (pack b_cancel :side :right)
    (pack b_ok :side :right)
    (bind w "<Return>" (lambda (event)
			 (declare (ignore event))
			 (break-mainloop)))
    (when (and default (> (length default) 0))
      (setf (text e) default))
    (focus e)
    (grab w)
    (mainloop)
    (grab-release w)
    (withdraw w)
    (and ok
	 (text e))
    ))
(defun modal-test ()
  (with-ltk ()
   (let* ((b (make-instance 'button :text "Input" 
			    :command (lambda ()
				       (let ((erg (input-box "Enter a string:" :title "String input")))
					 (if erg 
					     (format t "input was: ~a~%" erg)
					   (format t "input was cancelled~%"))
				       (finish-output))))))
     (pack b))))


#-:tk84
 (defun combotest ()
  (with-ltk ()
    (let* ((c (make-instance 'combobox :text "foo" :values '("bar" "baz" "foo bar")))
           (add (make-instance 'button :text "Add values"
                               :command (lambda ()
                                          (setf (options c) (list 1 2 "asdf xx" "bb" "cc")))))
           (ok (make-instance 'button :text "Ok" :command
                             (lambda ()
                               (format t "text: ~a~%" (text c))
                               (exit-wish)))))
      (bind c "<KeyRelease>" (lambda (event)
                               (declare (ignore event))
                               (format t "newsel:~a~%" (text c))
                               (finish-output)))
      (bind c "<<ComboboxSelected>>" (lambda (event)
                                       (declare (ignore event))
                                       (format t "newsel:~a~%" (text c))
                                       (finish-output)))
      (pack add :side :right)
      (pack ok :side :right)
      (pack c :side :left))))

(defun packtest1 ()
  (with-ltk ()
    (dotimes (i 10)
      (let ((s ""))
        (dotimes (j i)
          (setf s (format nil "~a " s))) 
      (pack (make-instance 'button :text (format nil "Button~a Nr. ~a" s i)))
      (sleep 0.1))
    )))

(defun packtest2 ()
  (with-ltk (:debug-tcl t)
    (with-atomic
        (dotimes (i 10)
          (pack (make-instance 'button :text (format nil "Button Nr. ~a" i)))
          ;(sleep 0.1)
          )
      )))


(defun sctest()
  (with-ltk (:debug-tcl t)
    (let* ((sf (make-instance 'scrolled-frame))
           (f (interior sf))
           (n 1)
           (b1 (make-instance 'button :master f :text "Button 1"
                              :command (lambda ()
                                         (incf n)
                                         (pack (make-instance 'button :master f
                                                              :text (format nil "Button ~a" n))
                                               :side :left))))
           )
      (pack sf :side :top :fill :both :expand t)
      (pack b1 :side :left)

    )))


(defun etest ()
  (with-ltk ()
    (let ((b (make-instance 'button :text " a button :)")))
      (pack b)
      ;;(send-wish (format nil "~a configure -text \" a } button\"" (widget-path b)))
      (setf (text b) " )} xasdf ")
      ;(send-wish "button }\"")
      (flush-wish))))


;;; treview tests

(defwidget treeviewtest (frame)
  ()
  ((tree treeview :pack (:side :top :expand t :fill :both)
	 )
   )
  (let* ((first (make-instance 'treeitem :tree tree :text "Hallo"))
	 (second (make-instance 'treeitem :tree tree :master first :text "Welt")))
    (declare (ignorable second))
  ))

(defun treeview-test ()
  (with-ltk ()
    (pack (make-instance 'treeviewtest) :fill :both :expand t)))

(defmacro with-hourglass (widgets &rest body)
  `(unwind-protect
    (progn
      ,@(mapcar (lambda (w)
		  `(configure ,w :cursor :watch))
		widgets)
      (flush-wish)
      ,@body)
    ,@(mapcar (lambda (w)
		  `(configure ,w :cursor ""))
		widgets)))

(pushnew :ltk *features*)


