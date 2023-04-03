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

(defun read-input (&optional (file #p"day17-input.txt"))
  (let ((map (make-array '(2000 2000) :initial-element #\.)))
    (loop :initially (setf (aref map 500 0) #\+)
	  :for line :in (uiop:read-file-lines file)
	  :for coord := (parse-line line)
	  :do (set-map coord #\# map)
	  :minimize (apply #'min (getf coord :y)) :into min-y
	  :maximize (apply #'max (getf coord :y)) :into max-y
	  :finally (return (values map min-y max-y)))))

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

(defun down-possible-p (map x y)
  (member (aref map x (1+ y))
	  '(#\. #\|)
	  :test #'char=))

(defun down! (map x y)
  (setf (aref map x y) #\|)
  (list x (1+ y)))

(defun pool-p (map x y)
  (and (loop :for left from x :downto 0
	     :never (down-possible-p map left y)
	     :thereis (char= (aref map left y) #\#))
       (loop :for right :from x :upto (first (array-dimensions map))
	     :never (down-possible-p map right y)
	     :thereis (char= (aref map right y) #\#))))

(defun not-wall-p (map x y)
  (not (char= (aref map x y) #\#)))

(defun pool! (map x y)
  (loop :for left :from x :downto 0
	:while (not-wall-p map left y)
	:do (setf (aref map left y) #\~))
  (loop :for right :from x :upto (first (array-dimensions map))
	:while (not-wall-p map right y)
	:do (setf (aref map right y) #\~))
  (list x (1- y)))

(defun sideways! (map x y)
  (remove nil (list
	       (loop :for left :from x :downto 0
		     :while (not-wall-p map left y)
		     :do (setf (aref map left y) #\|)
		     :when (down-possible-p map left y)
		       :return (list left y))
	       (loop :for right :from x :upto (first (array-dimensions map))
		     :while (not-wall-p map right y)
		     :do (setf (aref map right y) #\|)
		     :when (down-possible-p map right y)
		       :return (list right y)))))

(defun ensure-pos-list (l)
  (if (listp (car l))
      l
      (list l)))

(defun turn (map pos-list)
  (loop :for (x y) :in pos-list
	:append
	(ensure-pos-list
	 (cond ((down-possible-p map x y) (down! map x y))
	       ((pool-p map x y) (pool! map x y))
	       (t (sideways! map x y))))))

(defun count-water (map min-y max-y &optional (chars "|~"))
  (loop :for i :from 0 :below (array-dimension map 0)
	:sum (loop :for j :from min-y :upto max-y
		   :count (find (aref map i j) chars))))

(defun answers ()
  (multiple-value-bind (map min-y max-y)
      (read-input #p"day17-input.txt")
    (loop :for i :from 1
	  :for pos-list := '((500 0))
	    :then (remove-if (lambda (pos)
			       (> (second pos) max-y))
			     (remove-duplicates (turn map pos-list) :test #'equal))
	  :while (some (lambda (pos)
			 (<= (second pos) max-y))
		       pos-list)
	  ;; :do (format t "~%Turn: ~a. Pos: ~a.~%" i pos-list)
	  ;; :do (print-map map 450 550 0 100)
	  ;; :do (break)
	  :finally (return (values (count-water map min-y max-y)
				   (count-water map min-y max-y "~"))))))
