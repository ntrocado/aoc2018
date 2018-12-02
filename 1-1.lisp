(defun read-input (file)
  (with-open-file (data file)
    (loop :for val := (read data nil)
	  :while val :collect val)))

(defun answer ()
  (reduce #'+ (read-input "1-1-input.txt")))
