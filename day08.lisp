;;; Part 1

(defun parse-input (&optional (file "day08-input.txt"))
  (with-open-file (data file)
    (loop :for i := (read data nil)
	  :while i
	  :collect i)))

(defvar *lst* (parse-input))

(defun read-lst (n)
  (let ((data (loop :repeat n
		    :collect (pop *lst*))))
    (reduce #'+ data)))

(defun sum-metadata ()
  (let ((child (pop *lst*))
	(metadata (pop *lst*)))
    (if (zerop child)
	(read-lst metadata)
	(loop :for ch-node :from child :downto 0
	      :if (zerop ch-node)
		:sum (read-lst metadata)
	      :else
		:sum (sum-metadata)))))

(defun answer-1 ()
  (sum-metadata))
