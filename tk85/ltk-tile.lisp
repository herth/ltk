#|

 This software is Copyright (c) 2005  Peter Herth <herth@peter-herth.de>

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

(defpackage :ltk-tile
  (:use :common-lisp
	:ltk
	)
  (:export
   #:activate-tile
   #:style-default
   #:style-element-names
   #:theme-names
   #:use-theme
   #:tile-test
   ))

(in-package :ltk-tile)
(defparameter *tile-widgets* '(button check-button entry label radio-button scrollbar  ))
;;; checkbutton combobox dialog notebook paned progressbar treeview menubutton separator

(defun require-tile ()
  (send-wish "package require tile"))

(defun activate-tile ()
  (pushnew #'require-tile *init-wish-hook*)
  (dolist (widget *tile-widgets*)
    (let ((w (make-instance widget)))
      (unless (search "ttk::" (widget-class-name w))
	(setf (widget-class-name w) (concatenate 'string "ttk::" (widget-class-name w)))))))

(defun theme-names ()
  (send-wish "senddatastrings [style theme names]")
  (ltk::read-data))

(defun use-theme(name)
  (format-wish "style theme use ~a" name))

(defun style-element-names ()
  (send-wish "senddatastrings [style element names]")
  (ltk::read-data))

(defun style-default (style &rest params)
  (format-wish "style default ~A ~{ -~(~a~) {~a}~}" style params))

(defun tile-test ()
  (activate-tile)
  (with-ltk ()
     (let* ((mb (make-menubar))
	    (mtheme (make-menu mb "Theme" ))
	    (b (make-instance 'button :text "a button"))
	    (l (make-instance 'label :text "a label"))
	    (e (make-instance 'entry :text "an entry"))
	    )
       (pack (list l e b) :side :left)
       (dolist (theme (theme-names))
	 (let ((theme theme))
	   (make-menubutton mtheme theme (lambda ()
					   (use-theme theme)))))
       )))

	     
       