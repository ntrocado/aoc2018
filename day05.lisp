;;; Part 1

(defun input (&optional (file "day05-input.txt"))
  (with-open-file (data file)
    (read-line data)))

(defun remove-units (str)
  (let ((stack))
    (loop :for char :across str
	  :while char
	  :do (if (and stack
		       (char-equal char (car stack))
		       (char/= char (car stack)))
		(pop stack)
		(push char stack)))
    (coerce (nreverse stack) 'string)))

(defun answer-1 ()
  (length (reduce-string (input))))

;;; Part 2

(defun remove-letter (str letter)
  (remove letter str :test 'char-equal))

(defun shortest (str)
  (loop :for ch :from (char-code #\a) :upto (char-code #\z)
	:minimize (length (remove-units (remove-letter str (code-char ch))))))

(defun answer-2 ()
  (shortest (input)))
