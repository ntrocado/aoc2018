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

