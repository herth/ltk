;;; Copyright (c) 2005 Thomas F. Burdick.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :fly)

(defclass system-browser (treelist)
  ()
  (:default-initargs :depth 5 :data (all-packages)))

(defclass node ()
  ((name :initarg name :accessor name)
   (children :initarg children :accessor children)
   (select :initform nil :initarg select :accessor select)))

(defmethod name ((string string)) string)

(defmethod treelist-name ((node node))
  (let ((name (name node)))
    (etypecase name
      (string name)
      ((or function symbol) (funcall name)))))

(defmethod treelist-children ((tree system-browser) (node node))
  (declare (ignore tree))
  (let ((children (children node)))
    (etypecase children
      (list children)
      ((or function symbol) (funcall children)))))

(defmethod treelist-select ((tree system-browser) (node node))
  (declare (ignore tree))
  (let ((s (select node)))
    (when s (funcall s))))

(defun node (&key name children select)
  (make-instance 'node 'name name 'children children 'select select))

(defun all-packages ()
  (labels ((all-node ()
	     (node :name "-- all --"
		   :children
		   (lambda ()
		     (sort (loop for p in (list-all-packages)
				 nconc (loop for s being the symbols of p
					     for class = (find-class s nil)
					     when class
					     collect (class-node class t)))
			   #'string< :key #'name))))
	   (pnode (package)
	     (node :name (princ-to-string (intern (package-name package) :keyword))
		   :children
		   (lambda ()
		     (sort (loop for s being the present-symbols of package
				 for class = (find-class s nil)
				 when class collect (class-node class))
			   #'string< :key #'name)))))
  (list* (all-node)
	 (sort (mapcar #'pnode (list-all-packages))
	       #'string< :key #'name))))

(defun class-node (class &optional full-name-p)
  (labels ((all-gfs ()
	     (mapcar #'sb-mop:method-generic-function
		     (sb-mop:specializer-direct-methods class)))
	   (gfs (package)
	     (remove-duplicates (loop for gf in (all-gfs)
				      when (eql (gf-package gf) package)
				      collect gf)))
	   (packages ()
	     (remove-duplicates
	      (loop for gf in (all-gfs)
		    for name = (sb-mop:generic-function-name gf)
		    collect (gf-package gf))))
	   (gf-package (gf)
	     (let ((name (sb-mop:generic-function-name gf)))
	       (etypecase name
		 (symbol (symbol-package name))
		 ((cons (eql setf) cons) (symbol-package (second name)))
		 (list (symbol-package (first name))))))
	   (all-node ()
	     (node :name "-- all --"
		   :children (lambda ()
			       (sort (loop for gf in (all-gfs)
					   collect (gf-node gf class t))
				     #'string< :key #'name))))
	   (package-node (package)
	     (node :name (princ-to-string (intern (package-name package)
						  :keyword))
		   :children (lambda ()
			       (sort (loop for gf in (gfs package)
					   collect (gf-node gf class))
				     #'string< :key #'name)))))
    (node :name (if full-name-p
		    (prin1-to-string (class-name class))
		    (princ-to-string (class-name class)))
	  :children (lambda ()
		      (list* (all-node)
			     (sort (mapcar #'package-node (packages))
				   #'string< :key #'name))))))

(defun gf-node (gf class &optional full-name-p)
  (node :name (if full-name-p
		  (prin1-to-string (sb-mop:generic-function-name gf))
		  (princ-to-string (sb-mop:generic-function-name gf)))
	:children
	(lambda ()
	  (sort (loop for m in (sb-mop:generic-function-methods gf)
		      when (find class (sb-mop:method-specializers m))
		      collect (method-node m))
		#'string< :key #'name))))

(defun petty-method-name (method)
  (format nil "~A ~{~S~^ ~} ~A"
	  (sb-mop:generic-function-name (sb-mop:method-generic-function method))
	  (sb-mop:method-qualifiers method)
	  (mapcar (lambda (x)
		    (typecase x
		      (sb-mop:eql-specializer
		       (list 'eql (sb-mop:eql-specializer-object x)))
		      (class (class-name x))
		      (t x)))
		  (sb-mop:method-specializers method))))

(defun method-node (method)
  (node :name (petty-method-name method)
	:children ()))


