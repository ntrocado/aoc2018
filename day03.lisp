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

(setf test-claims
  '((1 3 4 4)
    (3 1 4 4)
    (3 3 2 2)))


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
	  :collect (rest (get-numbers-in-string claim)))))

(defun answer-1 ()
  (count-duplicate-claims (parse-input "day03-input.txt")))
