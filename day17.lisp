(ql:quickload "cl-ppcre")

(defun n-or-iota (val)
  (if (find #\. val)
      (ppcre:register-groups-bind ((#'parse-integer start end))
	  ("(\\d+)..(\\d+)" val)
	(loop :for n :from start :upto end :collect n))
      (list (parse-integer val))))

(defun parse-line (str)
  (let (results)
    (ppcre:do-register-groups ((#'n-or-iota x y))
	("(?<=x=)(\\d+..\\d+|\\d+)|(?<=y=)(\\d+..\\d+|\\d+)" str results)
      (when x (setf results (list* :x x results)))
      (when y (setf results (list* :y y results))))))

(defun set-map (coord char map)
  (loop :for x :in (getf coord :x)
	:do (loop :for y :in (getf coord :y)
		  :do (setf (aref map x y) char))))

(defun find-max (&optional (file #p"day17-input.txt"))
  (apply #'max
	 (mapcar #'parse-integer
		 (ppcre:all-matches-as-strings "\\d+"
					       (uiop:read-file-string file)))))

(defun read-input (&optional (file #p"day17-input.txt"))
  (let ((map (make-array '(2000 2000) :initial-element #\.)))
    (mapcar (lambda (line)
	      (set-map (parse-line line) #\# map))
	    (uiop:read-file-lines file))
    (setf (aref map 500 0) #\+)
    map))

(defun print-map (map &optional
			(x1 0)
			(x2 (first (array-dimensions map)))
			(y1 0)
			(y2 (second (array-dimensions map))))
  (loop :for y :from y1 :below y2
	:do (loop :for x :from x1 :below x2
		  :do (format t "~a" (aref map x y))
		  :finally (format t "~%"))))

;;; (print-map (read-input #p"day17-test.txt") 494 507 0 14)

(defun sand-p (map x y)
  (char= #\. (aref map x y)))

(defun next (map water-x water-y)
  (unless (aref map water-x (1+ water-y))
   (if (sand-p map water-x (1+ water-y))
       ;; if down is sand make it new water
       (progn (setf (aref map water-x (1+ water-y)) #\|)
	      (values map water-x (1+ water-y)))
       ;; else, if it can't go further down...
       ;; fill horizontally
       (let (new-water-x)
	 (setf (aref map water-x water-y) #\~)
	 (loop :for left :from (1- water-x) :downto 0
	       :while (and (sand-p map left water-y)
			   (not (sand-p map left (1+ water-y))))
	       :do (setf (aref map left water-y) #\~)
	       :finally (when (sand-p map left water-y)
			  (setf (aref map left water-y) #\|)
			  (push left new-water-x)))
	 (loop :for right :from (1+ water-x) :upto (second (array-dimensions map))
	       :while (and (sand-p map right water-y)
			   (not (sand-p map right (1+ water-y))))
	       :do (setf (aref map right water-y) #\~)
	       :finally (when (sand-p map right water-y)
			  (setf (aref map right water-y) #\|)
			  (push right new-water-x)))
	 (values map (or new-water-x water-x) (if new-water-x
						  (make-list (length new-water-x)
							     :initial-element water-y)
						  (1- water-y)))))))

(defun run (map x y)
  (unless (>= y (find-max))
    (multiple-value-bind (new-map water-x water-y)
	(next map x y)
      (if (atom water-x)
	  (run new-map water-x water-y)
	  (mapc (lambda (x y)
		  (run new-map x y))
		water-x water-y)))))

(defun answer-1 (&optional (file #p"day17-input.txt"))
  (let ((map (read-input file)))
    (run map 500 0)
    (print-map map 470 530 0 (find-max file))
    (destructuring-bind (n m)
	(array-dimensions map)
      (loop :for i :from 0 :below n
	    :sum (loop :for j :from 0 :below m
		       :count (find (aref map i j) "+|~"))))))
