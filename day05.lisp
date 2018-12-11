;;; Part 1

(defun input (&optional (file "day05-input.txt"))
  (with-open-file (data file)
    (read-line data)))

(defun remove-units (str &optional (pointer 0))
  (let ((a (char str pointer))
	(b (char str (1+ pointer))))
    (cond ((= pointer (- (length str) 2)) str)
	  ((and (char-equal a b)
		(char/= a b))
	   (remove-units (concatenate 'string
				      (subseq str 0 pointer)
				      (subseq str (+ pointer 2)))
			 0))
	  (t (remove-units str (1+ pointer))))))

(defun answer-1 ()
  (length (remove-units (input))))
