
(defpackage :tktable
    (:use :common-lisp :ltk
          #+:sbcl :sb-ext
          )
  (:export
   #:table

   #:scrolled-table
   #:subvals
   #:vals
   #:set-row
   #:defmethod))

(in-package :tktable)


(eval-when (:load-toplevel)
  (setf *init-wish-hook* (append *init-wish-hook*
				 (list (lambda ()
					 (send-wish "if {[catch {package require Tktable} err]} {tk_messageBox -icon error -type ok -message \"$err\"}")
				       )))))

;;; tktable widget

(ltk::defargs table ()
  anchor relief rows cols borderwidth titlecols titlerows)

(ltk::defwrapper table (widget)
  ((rows :accessor rows :initarg :rows :initform nil)
   (cols :accessor cols :initarg :cols :initform nil)
   (data :accessor data :initarg :data :initform nil)
   (cache :accessor cache :initarg :cache :initform 1)
   (xscroll :accessor xscroll :initarg :xscroll :initform nil)
   (yscroll :accessor yscroll :initarg :yscroll :initform nil)
   )
  "table"
  (configure widget :cache (cache widget))
  (format-wish "~a configure -variable ~a" (widget-path widget) (ltk::name widget))
  (when (and (data widget)
	       (not (rows widget))
	       (not (cols widget)))
    (setf (rows widget) (length (data widget)))
    (setf (cols widget) (length (car (data widget)))))
  (let ((r 0))
    (dolist (row (data widget))
      (set-row widget r row)
      (incf r))))

#+nil(defmethod value ((v table))
  (format-wish "global ~a; senddata $~a" (name v) (name v))
  (read-data))

#+nil(defgeneric (setf value) (widget val))
#+nil(defmethod (setf value) (val (v table))
  (format-wish "global ~a; set ~a {~a}" (name v) (name v) val)
  val)


(defmethod (setf rows) :after (val (table table))
  (format-wish "~a configure -rows ~d" (widget-path table) val))

(defmethod (setf cols) :after (val (table table))
  (format-wish "~a configure -cols ~d" (widget-path table) val))

(defmethod vals ((table table))
  (format-wish "senddatastrings [~a get 0,0 end]" (widget-path table))
  (ltk::read-data))

(defgeneric subvals (table row col &optional to-row to-col))
(defmethod subvals ((table table) row col &optional to-row to-col)
  (format-wish "senddatastrings [~a get ~a,~a ~a,~a]" (widget-path table)
	       row col (or to-row row) (or to-col col))
  (ltk::read-data))

(defgeneric set-row (table row value-list))
(defmethod set-row ((table table) row value-list)
  (format-wish "~a set row ~d,0 {~{{~a}~^ ~}}" (widget-path table) row value-list))  

(defclass scrolled-table (frame)
  ((table :accessor table)
   (hscroll :accessor hscroll)
   (vscroll :accessor vscroll)
   (rows :accessor rows :initarg :rows :initform nil)
   (cols :accessor cols :initarg :rows :initform nil)
   (titlerows :accessor titlerows :initarg :titlerows :initform nil)
   (titlecols :accessor titlecols :initarg :titlecols :initform nil)
   (data :accessor data :initarg :data :initform nil)
   ))


(defmethod initialize-instance :after ((st scrolled-table) &key)
  (setf (hscroll st) (make-scrollbar st :orientation "horizontal"))
  (setf (vscroll st) (make-scrollbar st))

  (setf (table st) (make-instance 'table :master st :xscroll (hscroll st) :yscroll (vscroll st) :rows (rows st) :cols (cols st)
				  :data (data st) :titlerows (titlerows st)
				  :titlecols (titlecols st)))
  (grid (table st) 0 0 :sticky :news)
  (grid (hscroll st) 1 0 :sticky :we)
  (grid (vscroll st) 0 1 :sticky :ns)
  (grid-columnconfigure st 0 :weight 1)
  (grid-columnconfigure st 1 :weight 0)
  (grid-rowconfigure st 0 :weight 1)
  (grid-rowconfigure st 1 :weight 0)
 
  (configure (hscroll st) "command" (concatenate 'string (widget-path (table st)) " xview"))
  (configure (vscroll st) "command" (concatenate 'string (widget-path (table st)) " yview"))
  (configure (table st) "xscrollcommand" (concatenate 'string (widget-path (hscroll st)) " set"))
  (configure (table st) "yscrollcommand" (concatenate 'string (widget-path (vscroll st)) " set"))
  )


(defun tabletest ()
  (with-ltk ()
    (let ((sctable (make-instance 'scrolled-table
				:titlerows 1
				:titlecols 1
				:data
				(cons (cons "*" (loop for c from 1 to 40 collect
						   c))
				      (loop for r from 1 to 200
					 collect
					   (cons r
						 (loop for c from 1 to 40 collect
						      (* r c))))))))
      (pack sctable :side :top :fill :both :expand t)
      (format t "7 * 8 is ~a~%" (car (subvals (table sctable) 7 8)))
      (finish-output))))
