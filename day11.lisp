(defparameter *grid-serial-number* 9005)

(defun hundreds (x)
  (/ (- (mod x 1000) (mod x 100))
     100))

(defun power-level (x y &optional (serial *grid-serial-number*))
  (let ((rack-id (+ x 10)))
    (- (hundreds (* (+ (* rack-id y) serial)
		    rack-id))
       5)))

(defun test-1 ()
  (= (power-level 3 5 8)
     4))

(defun test-2 ()
  (= (power-level 122 79 57)
     -5))

(defun test-3 ()
  (= (power-level 217 196 39)
     0))

(defun test-4 ()
  (= (power-level 101 153 71)
     4))

(defun grid (&optional (serial *grid-serial-number*))
  (let ((arr (make-array '(300 300))))
    (loop :for x :from 1 :upto 300
	  :do (loop :for y :from 1 :upto 300
		    :do (setf (aref arr (1- x) (1- y))
			      (power-level x y serial))))
    arr))

(defparameter *cache* (make-hash-table :test 'equal))

(defun square (grid x y &optional (size 3))
  (or (gethash (list x y size) *cache*)
      (let ((result (if (= size 1)
			(aref grid x y)
			(+ (square grid x y (1- size))
			   (square grid (1+ x) (1+ y) (1- size))
			   (square grid (+ x size -1) y 1)
			   (square grid x (+ y size -1) 1)
			   (if (>= size 3)
			       (- (square grid (1+ x) (1+ y) (- size 2)))
			       0)))))
	(setf (gethash (list x y size) *cache*) result)
	result)))

(defun largest-power (grid)
  (loop :with largest-square-val := 0
	:with largest-square-coordinates
	:for j :below (- 300 3)
	:do (loop :for i :below (- 300 3)
		  :do (let ((power (square grid j i)))
			(when (> power largest-square-val)
			  (setf largest-square-val power
				largest-square-coordinates (cons (1+ j) (1+ i))))))
	:finally (return largest-square-coordinates)))

(defun answer-1 ()
  (largest-power (grid)))

(defun largest-power-any-size (grid)
  (loop :with largest-square-val := 0
	:with largest-square-coordinates
	:with largest-size
	:for size :from 1 :below 300
	:do (loop :for j :below (- 300 size)
		  :do (loop :for i :below (- 300 size)
			    :do (let ((power (square grid j i size)))
				  (when (> power largest-square-val)
				    (setf largest-square-val power
					  largest-square-coordinates (cons (1+ j) (1+ i))
					  largest-size size)))))
	:finally (return (values largest-square-coordinates largest-size))))

(defun answer-2 ()
  (largest-power-any-size (grid)))
