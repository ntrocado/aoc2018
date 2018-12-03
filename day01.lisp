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
  (loop :with results := (make-hash-table)
	:initially (setf (gethash 0 results) t)
	:for v :in (circular! (read-input "day01-input.txt"))
	:for sum := v :then (+ v sum)
	:when (gethash sum results)
	  :return sum
	:do (setf (gethash sum results) t)))
