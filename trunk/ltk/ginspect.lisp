;;; graphical inspector


(defpackage :gins
  (:use :common-lisp
	:ltk
	;"SB-IMPL"
	)
  (:export #:ginspect))

(in-package :gins)


(defvar *inspect-length* 100)
(defvar *inspect-unbound-object-marker* "<UNBOUND>")

(defclass ginspector ()
  ((fields :accessor fields :initform nil)
   (desc :accessor desc)
   (named-p :accessor named-p :initform nil)
   (description :accessor description :initform nil)
   (elements :accessor elements :initform nil)
   (stack :accessor stack :initform nil)
   (value :accessor value :initform nil)
   (title :accessor title :initform nil)
   ))


(defun do-inspect (inspector value)
   (multiple-value-bind (description named-p elements)
       (inspected-parts value)
     (setf (named-p inspector) named-p
	   (description inspector) description
	   (elements inspector) elements)
     ;;(configure (title inspector) "text" (format nil "inspecting: ~a ~s" (type-of value) value))
     (setf (text (title inspector)) (format nil "inspecting: ~a ~s" (type-of value) value))
     (setf (text (desc inspector)) description)
     (listbox-clear (fields inspector))
     (if named-p
	 (listbox-append (fields inspector)
			 (mapcar (lambda (element)
				   (format nil "~a:~20T~s" (car element) (cdr element)))
				 elements))
       (listbox-append (fields inspector)
		       (mapcar (lambda (element)
				 (format nil "~s" element))
			       elements)))))


(defun ginspect (inspected-value)
  "inspect the value with a graphical user interface"
  (let ((*debug-tk* nil))
  (with-ltk ()
   (setf *debug-tk* nil)
   (let* ((inspector (make-instance 'ginspector))	   
	  (f (make-instance 'frame))
	  (lbl (make-instance 'label :text "  "))
	  (pane (make-instance 'paned-window :orientation "vertical"))
	  (f2 (make-instance 'frame :master pane))
	  (desc (make-instance 'text :master f2 :width 60 :height 4))
	  (dscroll (make-instance 'scrollbar :master f2 :orientation "vertical"))
	  (lb-fields (make-instance 'scrolled-listbox :master pane))
	  (fields (listbox lb-fields))
	  (entry (make-instance 'entry))
	  (history nil)
	  (history-pos 0)
	  (quit (make-instance 'button :master f :text "quit"
			       :command (lambda () (setf ltk::*exit-mainloop* t))))
	  (back (make-instance 'button :master f :text "back"
			       :command 
			       (lambda ()
				 (let ((val (pop (stack inspector))))
				   (when val
				     (setf (value inspector) val)
				     (do-inspect inspector val)))))))    
     (pack quit :side :left)
     (pack back :side :right)
     (pack f :side :top :fill :x)
     (pack lbl :side :top  :fill :x)
     (pack pane :side :top :expand 1 :fill :both)
     (grid desc 0 0 :sticky :news)
     (grid dscroll 0 1 :sticky :ns)
     (grid-columnconfigure f2 0 "weight" 1)
     (grid-columnconfigure f2 1 "weight" 0)
     (grid-rowconfigure f2 0 "weight" 1)
     (configure dscroll "command" (concatenate 'string (widget-path desc) " yview"))
     (configure desc "yscrollcommand" (concatenate 'string (widget-path dscroll) " set"))
     
     (add-pane pane f2)
     (add-pane pane lb-fields)
     (pack entry :side :top :fill :x)
     (configure lbl :font "courier 10 bold")
     (configure desc :font "courier 10 bold")
     (configure fields :font "courier 10 bold")
     (configure entry :font "courier 10 bold")
     (setf (desc inspector) desc)
     (setf (title inspector) lbl)
     (setf (fields inspector) fields)
     (bind fields "<Double-Button-1>"
	   (lambda (event)
	     (declare (ignore event))
	     (let* ((nr (first (listbox-get-selection fields))))
	       (when nr
		 (let ((entry (nth nr (elements inspector))))
		   (push (value inspector) (stack inspector))
		   (if (named-p inspector)
			 (setf (value inspector) (cdr entry))
		     (setf (value inspector) entry))
		   (do-inspect inspector (value inspector))
		   )))))

     (bind entry "<KeyPress-Return>"
	   (lambda (event)
	     (declare (ignore event))
	     (let ((txt (text entry)))
	       (multiple-value-bind (result condition)		   
		   (ignore-errors 
		     (with-input-from-string
		      (s txt)
		      (setf (value inspector) (eval (read s)))
		      (push txt history)
		      (setf history-pos 0)
		      (do-inspect inspector (value inspector))
		      t
		      ))
		 (declare (ignore condition))
		 (unless result
		   (setf (text entry) (format nil "Failed to evaluate: ~a" txt))
		   ))
	       (entry-select entry 0 "end")
	       )))
     (bind entry "<KeyPress-Up>"
	   (lambda (event)
	     (declare (ignore event))
	     (let ((val (nth (+ 1 history-pos) history)))
	       (when val
		 (setf (text entry) val)
		 (incf history-pos)))))
     (bind entry "<KeyPress-Down>"
	   (lambda (event)
	     (declare (ignore event))
	     (if (> history-pos 0)
		 (progn
		   (decf history-pos)
		   (setf (text entry) (nth history-pos history)))
	       (setf (text entry) ""))))

	     

     (setf (value inspector) inspected-value)
     (do-inspect inspector inspected-value)
     ))))

;;;; INSPECTED-PARTS

;;; Destructure an object for inspection, returning
;;;   (VALUES DESCRIPTION NAMED-P ELEMENTS),
;;; where..
;;;
;;;   DESCRIPTION is a summary description of the destructured object,
;;;   e.g. "The object is a CONS.~%".
;;;
;;;   NAMED-P determines what representation is used for elements
;;;   of ELEMENTS. If NAMED-P is true, then each element is
;;;   (CONS NAME VALUE); otherwise each element is just VALUE.
;;;
;;;   ELEMENTS is a list of the component parts of OBJECT (whose
;;;   representation is determined by NAMED-P).
;;;
;;; (The NAMED-P dichotomy is useful because symbols and instances
;;; need to display both a slot name and a value, while lists and
;;; vectors need only display a value.)
(defgeneric inspected-parts (object))

(defmethod inspected-parts ((object symbol))
  (values (format nil "The object is a SYMBOL.~%")
	  t
	  (list (cons "Name" (symbol-name object))
		(cons "Package" (symbol-package object))
	        (cons "Value" (if (boundp object)
				  (symbol-value object)
				  *inspect-unbound-object-marker*))
		(cons "Function" (if (fboundp object)
				     (symbol-function object)
				     *inspect-unbound-object-marker*))
		(cons "Plist" (symbol-plist object)))))

#+:sbcl
(defun inspected-structure-elements (object)
  (let ((parts-list '())
        (info (sb-impl::layout-info (sb-kernel:layout-of object))))
    (when (sb-kernel::defstruct-description-p info)
      (dolist (dd-slot (sb-impl::dd-slots info) (nreverse parts-list))
        (push (cons (sb-impl::dsd-name dd-slot)
                    (funcall (sb-impl::dsd-accessor-name dd-slot) object))
              parts-list)))))

#+(or :scl :cmu)
(defun inspected-structure-elements (object)
  (let ((parts-list '())
        (info (kernel:layout-info (kernel:layout-of object))))
    (when (kernel::defstruct-description-p info)
      (dolist (dd-slot (kernel:dd-slots info) (nreverse parts-list))
        (push (cons (kernel:dsd-name dd-slot)
                    (funcall (kernel:dsd-accessor dd-slot) object))
              parts-list)))))

#-(or :sbcl :scl :cmu)
(defun inspected-structure-elements (object)
  (list))

(defmethod inspected-parts ((object structure-object))
  (values (format nil "The object is a STRUCTURE-OBJECT of type ~S.~%"
		  (type-of object))
	  t
	  (inspected-structure-elements object)))

#+:sbcl
(defun inspected-standard-object-elements (object)
  (let ((reversed-elements nil)
        (class-slots (#+:sbcl sb-pcl::class-slots
                              #+:scl clos:class-slots
                              (class-of object))))
    (dolist (class-slot class-slots (nreverse reversed-elements))
      (let* ((slot-name (slot-value class-slot
                                    #+:sbcl 'sb-pcl::name
                                    #+:scl 'clos::name))
	     (slot-value (if (slot-boundp object slot-name)
			     (slot-value object slot-name)
			     *inspect-unbound-object-marker*)))
	(push (cons slot-name slot-value) reversed-elements)))))
#-:sbcl
(defun inspected-standard-object-elements (object)
  (list))

(defmethod inspected-parts ((object standard-object))
  (values (format nil "The object is a STANDARD-OBJECT of type ~S.~%"
		  (type-of object))
	  t
	  (inspected-standard-object-elements object)))

;(defmethod inspected-parts ((object funcallable-instance))
;  (values (format nil "The object is a FUNCALLABLE-INSTANCE of type ~S.~%"
;		  (type-of object))
;	  t
;	  (inspected-standard-object-elements object)))

(defmethod inspected-parts ((object condition))
  (values (format nil "The object is a CONDITION of type ~S.~%"
		  (type-of object))
	  t
	  (inspected-standard-object-elements object)))
#+:sbcl
(defmethod inspected-parts ((object function))
  (let* ((type (sb-kernel:widetag-of object))
	 (object (if (= type sb-vm:closure-header-widetag)
		     (sb-kernel:%closure-fun object)
		     object)))
    (values (format nil "FUNCTION ~S.~@[~%Argument List: ~A~]." object
		    (sb-kernel:%simple-fun-arglist object)
		    ;; Defined-from stuff used to be here. Someone took
		    ;; it out. FIXME: We should make it easy to get
		    ;; to DESCRIBE from the inspector.
		    )
	    t
	    nil)))
#-:sbcl
(defmethod inspected-parts ((object function))
  (values (format nil "FUNCTION ~S.~@[~%Argument List: ~A~]." object (list)
		  ;(sb-kernel:%simple-fun-arglist object)
		  ;; Defined-from stuff used to be here. Someone took
		  ;; it out. FIXME: We should make it easy to get
		  ;; to DESCRIBE from the inspector.
		  )
	    t
	    nil))

(defmethod inspected-parts ((object vector))
  (values (format nil
		  "The object is a ~:[~;displaced ~]VECTOR of length ~W.~%"
		  #+:sbcl (and (sb-impl::array-header-p object)
			       (sb-impl::%array-displaced-p object))
                  #+(or :scl :cmu) (and (kernel:array-header-p object)
                                        (kernel:%array-displaced-p object))
                  #-(or :sbcl :scl :cmu) nil
                 (length object))
	  nil
	  ;; FIXME: Should we respect *INSPECT-LENGTH* here? If not, what
	  ;; does *INSPECT-LENGTH* mean?
	  (coerce object 'list)))

(defun inspected-index-string (index rev-dimensions)
  (if (null rev-dimensions)
      "[]"
      (let ((list nil))
	(dolist (dim rev-dimensions)
	  (multiple-value-bind (q r) (floor index dim)
	    (setq index q)
	    (push r list)))
	(format nil "[~W~{,~W~}]" (car list) (cdr list)))))

(defmethod inspected-parts ((object array))
  (let* ((length (min (array-total-size object) *inspect-length*))
	 (reference-array (make-array length :displaced-to object))
	 (dimensions (array-dimensions object))
	 (reversed-elements nil))
    ;; FIXME: Should we respect *INSPECT-LENGTH* here? If not, what does
    ;; *INSPECT-LENGTH* mean?
    (dotimes (i length)
      (push (cons (format nil
			  "~A "
			  (inspected-index-string i (reverse dimensions)))
		  (aref reference-array i))
	    reversed-elements))
    (values (format nil "The object is ~:[a displaced~;an~] ARRAY of ~A.~%~
                         Its dimensions are ~S.~%"
		    (array-element-type object)
		    #+:sbcl (and (sb-impl::array-header-p object)
				 (sb-impl::%array-displaced-p object))
                    #+(or :scl :cmu) (and (kernel:array-header-p object)
                                          (kernel:%array-displaced-p object))
                    #-(or :sbcl :scl :cmu) nil
		    dimensions)
	    t
	    (nreverse reversed-elements))))

(defmethod inspected-parts ((object cons))
  (if (consp (cdr object))
      (inspected-parts-of-nontrivial-list object)
      (inspected-parts-of-simple-cons object)))

(defun inspected-parts-of-simple-cons (object)
  (values "The object is a CONS.
"
	  t
	  (list (cons 'car (car object))
		(cons 'cdr (cdr object)))))

(defun inspected-parts-of-nontrivial-list (object)
  (let ((length 0)
	(in-list object)
	(reversed-elements nil))
    (flet ((done (description-format)
	     (return-from inspected-parts-of-nontrivial-list
	       (values (format nil description-format length)
		       t
		       (nreverse reversed-elements)))))
      (loop
       (cond ((null in-list)
	      (done "The object is a proper list of length ~S.~%"))
	     ((>= length *inspect-length*)
	      (push (cons 'rest in-list) reversed-elements)
	      (done "The object is a long list (more than ~S elements).~%"))
	     ((consp in-list)
	      (push (cons length (pop in-list)) reversed-elements)
	      (incf length))
	     (t
	      (push (cons 'rest in-list) reversed-elements)
	      (done "The object is an improper list of length ~S.~%")))))))

(defmethod inspected-parts ((object number))
  (values (format nil "The object is a NUMBER: ~S~%" object) nil nil))

(defmethod inspected-parts ((object integer))
  (values (format nil "The object is an INTEGER: ~S~%" object) nil nil))

(defmethod inspected-parts ((object rational))
  (values (format nil "The object is a rational number: ~S~%" object) t
	  (list (cons "Numerator" (numerator object))
		(cons "Denominator" (denominator object)))
	  ))

(defmethod inspected-parts ((object character))
  (values (format nil "The object is an CHARACTER: ~S~%" object) t
	  (list (cons "Character" object)
		(cons "Name" (if (char-name object)
				 (char-name object)
			       object))
		(cons "Ascii" (char-code object)))))

(defmethod inspected-parts ((object t))
  (values (format nil "The object is an ATOM:~%  ~W~%" object) nil nil))


	  