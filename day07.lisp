;;; Part 1

(defun get-capital-letters (str)
  (coerce (remove-if-not #'upper-case-p str)
	  'list))

(defun parse-input (&optional (file "day07-input.txt"))
  (with-open-file (data file)
    (loop :for line := (read-line data nil)
	  :while line
	  :collect (rest (get-capital-letters line)))))

(defun make-prerequisites (steps)
  (let ((ht (make-hash-table :size 26)))
    (loop :for letter :from (char-code #\A) :upto (char-code #\Z)
	  :do (setf (gethash (code-char letter) ht) nil)
	  :finally (mapc (lambda (step) (push (first step)
					      (gethash (second step) ht)))
			 steps))
    ht))

(defun complete-step (letter prerequisites)
  (maphash (lambda (key val) (setf (gethash key prerequisites) (remove letter val)))
	   prerequisites)
  (remhash letter prerequisites)
  prerequisites)

(defun available (prerequisites)
  (loop :for key :being :the :hash-keys :of prerequisites :using (hash-value value)
	:unless (gethash key prerequisites)
	  :collect key))

(defun choose-next (prerequisites)
  (first (available prerequisites)))

(defun do-all (prerequisites)
  (loop :for step := prerequisites :then (complete-step next prerequisites)
	:for next := (choose-next step)
	:while next
	:collect next :into result
	:finally (return (coerce result 'string))))

(defun answer-1 (&optional (file "day07-input.txt"))
  (do-all (make-prerequisites (parse-input file))))

;;; Part 2

(defstruct worker current-task time-to-go)

(defun letter-time (letter &optional (min 60))
  (+ min (- (char-code letter) 64)))

(defun no-one-working-p (workers task)
  (not (some (lambda (x) (eql task
			      (worker-current-task x)))
	     workers)))

(defun next-available (prerequisites workers)
  (find-if (lambda (x) (no-one-working-p workers x))
	   (available prerequisites)))

(defun do-all-with-workers (prerequisites)
  (let ((workers
	  (make-array 5 :element-type 'worker
			:initial-contents (loop :repeat 5
						:collect (make-worker
							  :time-to-go 0)))))
    (loop :with step := prerequisites
	  :for second :from 0
	  :do (loop :for w :across workers
		    :do (if (zerop (worker-time-to-go w))
			    (progn
			      (when (worker-current-task w)
				(complete-step (worker-current-task w) step)
				(setf (worker-current-task w) nil))
			      (let ((next-letter (next-available step workers)))
				(when next-letter
				  (setf (worker-current-task w) next-letter)
				  (setf (worker-time-to-go w)
					(1- (letter-time next-letter 60))))))
			    (decf (worker-time-to-go w))))
	  :maximize second
	  :until (every (lambda (x) (null (worker-current-task x)))
			workers))))

(defun answer-2 (&optional (file "day07-input.txt"))
  (do-all-with-workers (make-prerequisites (parse-input file))))
