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

(defun read-record (str)
  "Return time in UT and event: either guard id, 'asleep or 'wake-up."
  (let ((numbers (rest (mapcar #'abs (get-numbers-in-string str)))))
    (list (apply #'encode-universal-time
		 (append '(0) (reverse (subseq numbers 0 4)) '(2000)))
	  (cond ((search "asleep" str) 'asleep)
		((search "wakes" str) 'wake-up)
		(t (car (last numbers)))))))

(defun parse-input (file)
  (with-open-file (data file)
    (sort (copy-seq (loop :for record := (read-line data nil)
			  :while record
			  :collect (read-record record)))
	  #'< :key #'first)))

(defun guard-records (events)
  "Return a hash-table where key->id, value->minutes slept."
  (let ((ht (make-hash-table)))
    (loop :for record :in events
	  :for guard := (if (integerp (second record))
			    (second record)
			    guard)
	  :for time-asleep := (if (eql (second record) 'wake-up)
				  (incf (gethash guard ht) (- (first record)
							      time-asleep))
				  (progn (unless (gethash guard ht)
					   (setf (gethash guard ht) 0))
					 (first record))))
    ht))

(defvar *test-data*
  "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

(defun test (&optional (test-data *test-data*))
  (with-input-from-string (data test-data) 
    (sort (copy-seq (loop :for record := (read-line data nil)
			  :while record
			  :collect (read-record record)))
	  #'< :key #'first)))

(defun test-ht (&optional (test-data *test-data*))
  (guard-records (test)))

(defun guard-who-sleeps-the-most (ht)
  (loop :for id :being :the :hash-keys :of ht :using (hash-value value)
	:for maximum := (list id value) :then (if (> value (second maximum))
						  (list id value)
						  maximum)
	:finally (return maximum)))

(defun ut->minute (ut)
  (nth-value 1 (decode-universal-time ut)))

(defun filter-events (events id &optional results)
  "Filter events for guard id."
  (let ((start (position id events :key #'second)))
    (if (or (null events) (null start))
	results
	(let* ((no-id (subseq events (1+ start)))
	       (end (position-if #'numberp no-id :key #'second)))
	  (filter-events no-id id (append results (subseq no-id 0 end)))))))

(defun minute-guard-sleeps-the-most (events)
  (let ((results
	  (loop :with minutes := (make-array 60)
		:for event :in events
		:for asleep := (if (eql (second event) 'asleep)
				   (ut->minute (first event))
				   asleep)
		:when (eql (second event) 'wake-up)
		  :do (loop :for i := asleep :then (mod (incf i) 60)
			    :while (< i (ut->minute (first event)))
			    :do (incf (aref minutes i)))
		:finally (return (coerce minutes 'list)))))
    (position (apply #'max results) results)))

(defun answer-1 ()
  (let* ((events (parse-input "day04-input.txt"))
	 (guard (first (guard-who-sleeps-the-most (guard-records events))))
	 (minute (minute-guard-sleeps-the-most (filter-events events guard))))
    (* guard minute)))

;;; Part 2

(defun guards-minutes (events)
  "Return a hash-table with key->guard id, value->vector of minute frequency."
  (let ((ht (make-hash-table)))
    (loop :for event :in events
	  :for current-id := (if (numberp (second event))
				 (second event)
				 current-id)
	  :for minute-asleep := (if (eql (second event) 'asleep)
				    (ut->minute (first event))
				    minute-asleep)
	  :when (eql (second event) 'wake-up)
	    :do (unless (gethash current-id ht)
		  (setf (gethash current-id ht) (make-array 60)))
	    :and :do (loop :for i :from minute-asleep
			     :below (ut->minute (first event))
			   :do (incf (aref (gethash current-id ht) i)))
	  :finally (return ht))))

(defun max-minute (ht)
  (loop :for key :being :the :hash-keys :of ht :using (hash-value value)
	:for maximum := (let* ((times (apply #'max (coerce value 'list)))
			       (minute (position times (coerce value 'list))))
			  (if (or (null maximum) (> times (first maximum)))
			      (list times minute key)
			      maximum))
	:finally (return (rest maximum))))

(defun answer-2 ()
  (apply #'* (max-minute (guards-minutes (parse-input "day04-input.txt")))))
