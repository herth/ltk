(in-package :cl-user)
(defpackage :bounce
  (:use :common-lisp
        :ltk)
  (:export
   :bounce
   ))

(in-package :bounce)

(defparameter *update-interval* 10)

(defwidget bounce (canvas)
  (ball
   paddle max-x max-y
   (px :accessor px :initform 25)
   (bx :accessor bx :initform 15)
   (by :accessor by :initform 15)
   (vx :accessor vx :initform 4)
   (vy :accessor vy :initform 4))
  ()
  (setf (max-x self) 790)
  (setf (max-y self) 590)
  (configure self :background :black :width 800 :height 600)
  (destructuring-bind (b p)
      (make-items self
                  `((:oval 0 0 30 30 :fill :yellow)
                    (:rectangle 0 500 50 510 :fill :red)))
    (setf (ball self) b
          (paddle self) p)
    (configure self :cursor :none)
    (focus self)
    (bind self "<Motion>" (lambda (event)
                            (motion self (event-x event))))
    (bind self "<KeyPress-Escape>" (lambda (event)
                            (declare (ignore event))
                            (exit-wish)))
    (bind self "<KeyPress-q>" (lambda (event)
                            (declare (ignore event))
                            (exit-wish))))
  (after 1000 (lambda () (animate self)))
  )

(defgeneric motion (self x))
(defmethod motion ((self bounce) x)
  (move (paddle self) (- x (px self)) 0)
  (setf (px self) x))

(defgeneric animate (self))
(defmethod animate ((self bounce))
  (incf (bx self) (vx self))
  (incf (by self) (vy self))
  (move (ball self) (vx self) (vy self))
  (when (<= (bx self) 15)
    (setf (vx self) (abs (vx self))))
  (when (<= (by self) 15)
    (setf (vy self) (abs (vy self))))
  (when (<= (max-x self) (bx self))
    (setf (vx self) (- (abs (vx self)))))
  (when (<= (max-y self) (by self))
    (setf (vy self) (- (abs (vy self)))))
  (when (and
         (<= (- (px self) 25) (bx self) (+ (px self) 25))
         (<= 490 (by self) 500))
    (setf (vy self) (- (abs (vy self)))))

  (after *update-interval* (lambda ()
                             (animate self))))

(defun bounce ()
  (with-ltk (:debug 0)
    (pack (make-instance 'bounce) :side :top :expand t :fill :both)))


