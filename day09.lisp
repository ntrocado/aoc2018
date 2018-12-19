;;; Part 1

(defun insert-after (item pos list)
  (append (subseq list 0 (1+ pos)) (list item) (subseq list (1+ pos))))

(defun remove-pos (pos list)
  (remove-if (constantly t) list :start pos :count 1))

(defun game (players last-marble)
  (loop :with score := (make-hash-table :size players)
	:for player := -1 :then (mod (1+ player) players)
	:for marble :from 0 :upto last-marble
	:for circle := '(0)
	  :then (if (integerp (/ marble 23))
		    (let ((counter-clockwise (mod (- current 7)
						  (length circle))))
		      (incf (gethash player score 0)
			    (+ marble (elt circle counter-clockwise)))
		      (remove-pos counter-clockwise circle))
		    (insert-after marble
				  (mod (1+ current) (length circle))
				  circle))
	:for current := 0
	  :then (if (integerp (/ marble 23))
		    (mod (- current 7) (1+ (length circle)))
		    (mod (1+ (mod (1+ current) (1- (length circle))))
			 (length circle)))
	:maximize (gethash player score 0)))

(defun answer-1 ()
  (game 418 71339))
