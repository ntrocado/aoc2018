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

;;; Part 2

(setf *lst* (parse-input))

(defun collect-data (n)
  (loop :repeat n
	:collect (pop *lst*)))

(defun value-of-root-node ()
  (let ((child (pop *lst*))
	(metadata (pop *lst*)))
    (if (zerop child)
	(read-lst metadata)
	(loop :for ch-node :from child :downto 0
	      :if (zerop ch-node)
		:return (loop :for entry :in (collect-data metadata)
			      :when (nth (1- entry) r) :sum :it)
	      :else
		:collect (value-of-root-node) :into r))))

(defun answer-2 ()
  (value-of-root-node))
