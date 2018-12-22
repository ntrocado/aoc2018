(ql:quickload '(qtools qtcore qtgui))

(in-package :cl+qt)
(in-readtable :qtools)

(defparameter *file* "day10-input.txt")
(defparameter *window-x* 61)
(defparameter *window-y* 9)

(defun get-numbers-in-string (str &optional numbers)
  (let ((pos (position-if (lambda (x)
			    (or (digit-char-p x)
				(char= #\- x)))
			  str)))
    (if (null pos)
	(reverse numbers)
	(multiple-value-bind (num end) (parse-integer str :junk-allowed t)
	  (if num
	      (get-numbers-in-string (subseq str end)
				     (cons num numbers))
	      (get-numbers-in-string (subseq str pos) numbers))))))

(defun input ()
  (with-open-file (data *file*)
    (loop :for l := (read-line data nil)
	  :while l
	  :collect (get-numbers-in-string l))))

(defstruct point pos-x pos-y vel-x vel-y)

(defun initial-points (data)
  (make-array
   (length data)
   :element-type 'point
   :initial-contents (mapcar
		      (lambda (x)
			(destructuring-bind (pos-x pos-y vel-x vel-y) x
			  (make-point :pos-x pos-x
				      :pos-y pos-y
				      :vel-x vel-x
				      :vel-y vel-y)))
		      data)))

(defun update-positions (points &optional (mult 1))
  (loop :for pt :across points
	:do (incf (point-pos-x pt) (* mult (point-vel-x pt)))
	:do (incf (point-pos-y pt) (* mult (point-vel-y pt)))
	:maximize (point-pos-x pt) :into max-x
	:maximize (point-pos-y pt) :into max-y
	:minimize (point-pos-x pt) :into min-x
	:minimize (point-pos-y pt) :into min-y
	:finally (return (values points max-x max-y min-x min-y))))

(defun normalize-points (points min-x min-y)
  (loop :for pt :across points
	:do (decf (point-pos-x pt) min-x)
	:do (decf (point-pos-y pt) min-y))
  points)

(defun final-points ()
  (loop :with pt := (initial-points (input))
	:for (points max-x max-y min-x min-y)
	  := (multiple-value-list (update-positions pt))
	:count points :into r
	:until (= (- max-y min-y) 9)
	:finally (return (values (normalize-points points min-x min-y)
				 (- max-x min-x)))))

(defun print-points (points x)
  (let ((lines (loop :repeat 10
		     :collect (make-string (1+ x)
					   :initial-element #\.))))
    (loop :for pt :across points
	  :do (setf (char (elt lines (point-pos-y pt)) (point-pos-x pt)) #\#))
    lines))

(defun answer-1 ()
  (multiple-value-bind (points x) (final-points)
    (print-points points x)))

(defun answer-2 ()
  (loop :with pt := (initial-points (input))
	:for y := (multiple-value-bind (points max-x max-y min-x min-y)
		      (update-positions pt)
		    (declare (ignore points max-x min-x))
		    (- max-y min-y))
	:count y
	:until (= y 9)))

;;; Graphic visualization

(defun range (value orig-min orig-max dest-min dest-max )
  (round (+ (/ (* (- value orig-min)
		  (- dest-max dest-min))
	       (- orig-max orig-min))
	    dest-min)))

(define-widget canvas (QWidget)
  ((data :initarg :data :accessor data)
   (timer-interval :initarg :timer-interval
		   :accessor timer-interval)
   (update-mult :initarg :update-mult
		:accessor update-mult)
   (counter :initarg :counter :accessor counter))
  (:default-initargs
    :data (initial-points (input))
    :timer-interval 5000
    :update-mult 1
    :counter 0))

(define-subwidget (canvas timer) (q+:make-qtimer canvas)
  (setf (q+:single-shot timer) nil)
  (q+:start timer timer-interval))

(define-slot (canvas update) ()
  (declare (connected timer (timeout)))
  (incf counter update-mult)
  (q+:repaint canvas))

(define-override (canvas paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter canvas))
		    (point (q+:make-qpoint)))
    (multiple-value-bind (new-data max-x max-y min-x min-y)
	(update-positions data update-mult)
      (loop :for pt :across new-data
	    :for x := (point-pos-x pt)
	    :for y := (point-pos-y pt)
	    :do (setf (q+:x point)
		      (range (point-pos-x pt) min-x max-x 0 *window-x*))
	    :do (setf (q+:y point)
		      (range (point-pos-y pt) min-y max-y 0 *window-y*))
	    :do (q+:draw-point painter point))
      (with-finalizing ((font (q+:make-qfont "Monospace" 10)))
	(setf (q+:style-hint font) (q+:qfont.type-writer))
	(setf (q+:font painter) font)
	(q+:draw-text painter 5 200
		      (format nil "counter: ~a; range: ~a"
			      counter (- max-y min-y)))))))

(define-override (canvas key-release-event) (ev)
  (cond ((= (q+:key ev) (q+:qt.key_left))
         (setf (q+:interval timer) (* (q+:interval timer) 2)))
        ((= (q+:key ev) (q+:qt.key_right))
	 (unless (< (q+:interval timer) 150)
	   (setf (q+:interval timer) (round (/ (q+:interval timer) 2)))))
	((= (q+:key ev) (q+:qt.key_1))
	 (setf update-mult 1))
	((= (q+:key ev) (q+:qt.key_2))
	 (setf update-mult 10))
	((= (q+:key ev) (q+:qt.key_3))
	 (setf update-mult 50))
	((= (q+:key ev) (q+:qt.key_4))
	 (setf update-mult 100))
	((= (q+:key ev) (q+:qt.key_0))
	 (q+:start timer)))
  (stop-overriding))

(defun main ()
  (with-main-window (window (make-instance 'canvas))
    (setf (q+:minimum-size window) (values *window-x* *window-y*))))
