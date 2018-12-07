;;; Part 1

(defun count-duplicate-claims (claims)
  (let ((fabric (make-array '(1000 1000) :initial-element nil))
	(already-counted (make-hash-table :test 'equal)))
    (loop :for (start-x start-y length-x length-y) :in claims
	  :sum (loop :for x :from start-x :below (+ start-x length-x)
		     :sum (loop :for y :from start-y :below (+ start-y length-y)
				:for inch := (aref fabric x y)
				:when (and inch (not (gethash (list x y)
							      already-counted)))
				  :count inch :into result
				  :and :do (setf (gethash (list x y)
							  already-counted)
						 t)
				:do (setf (aref fabric x y) t)
				:finally (return result))))))

(defvar test-claims
  '((1 3 4 4)
    (3 1 4 4)
    (5 5 2 2)))

(defun get-numbers-in-string (str &optional numbers)
  (let ((pos (position-if #'digit-char-p str)))
    (if (null pos)
	(reverse numbers)
	(multiple-value-bind (num end) (parse-integer str :junk-allowed t)
	  (if num
	      (get-numbers-in-string (subseq str end)
				     (cons num numbers))
	      (get-numbers-in-string (subseq str pos) numbers))))))

(defun parse-input (file)
  (with-open-file (data file)
    (loop :for claim := (read-line data nil)
	  :while claim
	  :collect (get-numbers-in-string claim))))

(defun answer-1 ()
  (count-duplicate-claims (mapcar #'rest (parse-input "day03-input.txt"))))

;;; Part 2

(defun make-fabric (claims)
  (let ((fabric (make-array '(1000 1000) :initial-element nil)))
    (loop :for (id start-x start-y length-x length-y) :in claims
	  :do (loop :for x :from start-x :below (+ start-x length-x)
		    :do (loop :for y :from start-y :below (+ start-y length-y)
			      :for inch := (aref fabric x y)
			      :if inch
				:do (setf (aref fabric x y)
					  (append (list id)
						  (if (atom inch)
						      (list inch)
						      inch)))
			      :else :do (setf (aref fabric x y) id))))
    fabric))

(defun unique-claim (claims fabric)
  (loop :for id :in (mapcar #'first claims)
	:when (destructuring-bind (n m) (array-dimensions fabric)
		(loop :for x :from 0 :below n
		      :always (loop :for y :from 0 :below m
				    :for e := (aref fabric x y)
				    :never (and (listp e)
						(member id e)))))
	  :return id))

(defvar test-claims-2
  '((1 1 3 4 4)
    (2 3 1 4 4)
    (3 5 5 2 2)))

(defun answer-2 ()
  (let ((data (parse-input "day03-input.txt")))
    (unique-claim data (make-fabric data))))
