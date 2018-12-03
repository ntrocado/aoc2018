;;; Part 1

(defun read-input (file)
  (with-open-file (data file)
    (loop :for val := (read data nil)
	  :while val :collect (symbol-name val))))

(defun count-duplicates (box-id)
  (loop :with letters := (remove-duplicates box-id)
	:for c1 :across letters
	:for count := (loop :for c2 :across box-id
			    :count (char-equal c1 c2))
	:for twos := (or twos (= count 2))
	:for threes := (or threes (= count 3))
	:finally (return (list twos threes))))

(defun answer-1 ()
  (loop :for id :in (read-input "day02-input.txt")
	:for (c2 c3) := (count-duplicates id)
	:counting c2 :into twos
	:counting c3 :into threes
	:finally (return (* twos threes))))

;;; Part 2

(defun compare-id (id1 id2)
  (loop :for a :across id1
	:for b :across id2
	:when (char-equal a b)
	  :collect a))

(defun answer-2 ()
  (loop :with data := (make-array 250
				  :initial-contents (read-input "day02-input.txt"))
	:for id1 :across data
	:for i :from 0
	:when (loop :for id2 :across (subseq data i)
		    :for comp := (compare-id id1 id2)
		    :when (and comp
			       (= 25 (length comp)))
		      :return comp)
	:return :it))
