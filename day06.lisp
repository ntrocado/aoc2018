;;; Part 1

(defun get-numbers-in-string (str &optional numbers)
  (let ((pos (position-if #'digit-char-p str)))
    (if (null pos)
	(reverse numbers)
	(multiple-value-bind (num end) (parse-integer str :junk-allowed t)
	  (if num
	      (get-numbers-in-string (subseq str end)
				     (cons num numbers))
	      (get-numbers-in-string (subseq str pos) numbers))))))

(defun input (&optional (file "day06-input.txt"))
  (with-open-file (data file)
    (loop :for l := (read-line data nil)
	  :while l
	  :collect (get-numbers-in-string l))))

(defun max-dimensions (coordinates)
  (list (1+ (apply #'max (mapcar #'first coordinates)))
	(1+ (apply #'max (mapcar #'second coordinates)))))

(defun minimum-manhattan-distance (x y coordinates)
  (loop :for c :in coordinates
	:for i :from 0
	:for dx := (abs (- (first c) x))
	:for dy := (abs (- (second c) y))
	:collect (list i (+ dx dy)) :into results
	:finally (return
		   (let ((sorted (sort (copy-seq results) #'< :key #'second)))
		     (if (= (cadar sorted) (cadadr sorted))
			 nil
			 (caar sorted))))))

(defun detect-infinites (coordinates)
  (destructuring-bind (max-x max-y) (max-dimensions ,coordinates)
    (loop :for x :in (list 0 max-x)
	  :do (loop :for y :from 0 :upto max-y
		    :do ,@body))
    (loop :for y :in (list 1 (1- max-y))
	  :do (loop :for x :from 1 :upto (1- max-x)
		    :do ,@body))))

(defun detect-infinites (coordinates)
  "Return a hash-table with coordinates' cardinals for areas that extend infinitely."
  (flet ((mark-inf (x y coordinates ht)
	   (let ((id (minimum-manhattan-distance x y coordinates)))
	     (unless (or (null id) (gethash id ht))
	       (setf (gethash id ht) t)))))
    (let ((infinites (make-hash-table)))
      (destructuring-bind (max-x max-y) (max-dimensions coordinates)
	(loop :for x :in (list 0 max-x)
	      :do (loop :for y :from 0 :upto max-y
			:do (mark-inf x y coordinates infinites)))
	(loop :for y :in (list 0 max-y)
	      :do (loop :for x :from 1 :upto (1- max-x)
			:do (mark-inf x y coordinates infinites))))
      infinites)))

(defun area-sizes (coordinates infinites)
  "Return a hash-table with key->coordinate id, value->area size."
  (destructuring-bind (max-x max-y) (max-dimensions coordinates)
    (loop :with results := (make-hash-table)
	  :for x :upto max-x
	  :do (loop :for y :upto max-y
		    :for id := (minimum-manhattan-distance x y coordinates)
		    :unless (or (null id) (gethash id infinites))
		      :do (incf (gethash id results 0)))
	  :finally (return results))))

(defun find-largest-area (ht)
  (loop :for val :being :the :hash-values :of ht
	:maximize val))

(defun answer-1 ()
  (let ((input (input "day06-input.txt")))
    (find-largest-area (area-sizes input (detect-infinites input)))))
