(defun parse-initial-state (initial-string)
  (loop :for i :below (length initial-string)
	:when (char= (aref initial-string i) #\#)
	  :collect i))

(defun parse-note (note-string)
  (map 'list (lambda (x) (char= x #\#)) note-string))

(defun input (&optional (file "day12-input.txt"))
  (with-open-file (data file)
    (values (parse-initial-state (subseq (read-line data nil) 15))
	    (let ((ht (make-hash-table :test 'equal)))
	      (read-line data nil) ;blank line
	      (loop :for l := (read-line data nil)
		    :while l
		    :do (setf (gethash (parse-note (subseq l 0 5))
				       ht)
			      (char= (aref l 9) #\#)))
	      ht))))

(defun seq (center plants)
  (loop :with plants := (remove-if (lambda (x) (< x (- center 2))) plants)
	:for n :from (- center 2) :upto (+ center 2)
	:if (member n plants)
	  :collect t
	  :and :do (pop plants)
	:else :collect nil))

(defun next-state (center plants notes)
  (gethash (seq center plants) notes))

(defun next-generation (plants notes)
  (loop :for i :from (- (first plants) 2) :below (+ (car (last plants)) 2)
	:when (next-state i plants notes)
	  :collect i))

(let ((previous-sum 0)
      (previous-delta 0))
  (defun repeat-delta-sum (sum)
    (let ((delta (- sum previous-sum)))
      (prog1 (when (= delta previous-delta)
	       delta)
	(setf previous-sum sum
	      previous-delta delta)))))

(defun sum-pots (generations &key (test nil))
  (multiple-value-bind (plants notes)
      (if test (input "day12-test.txt") (input))
    (loop :for i :below generations
	  :for generation := (next-generation plants notes)
	    :then (next-generation generation notes)
	  :for sum := (reduce #'+ generation)
	  :for delta := (repeat-delta-sum sum)
	  :until delta
	  :finally (return (if delta
			       (+ sum
				  (* delta (1- (- generations i))))
			       sum)))))

(defun answer-1 (&key (test nil))
  (sum-pots 20 :test test))

(defun answer-2 ()
  (sum-pots 50000000000))
