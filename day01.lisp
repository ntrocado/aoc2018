;;; Part 1

(defun read-input (file)
  (with-open-file (data file)
    (loop :for val := (read data nil)
	  :while val :collect val)))

(defun answer-1 ()
  (reduce #'+ (read-input "1-1-input.txt")))

;;; Part 2

(setf *print-circle* t)

(defun circular! (items)
  "Modifies the last cdr of list ITEMS, returning a circular list"
  (setf (cdr (last items)) items))

(defun answer-2 ()
  (loop :with results := '(0)
	:for v :in (circular! (read-input "1-1-input.txt"))
	:for sum := v :then (+ v sum)
	:when (member sum results)
	  :return sum
	:do (push sum results)))
