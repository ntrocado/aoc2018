;;; Part 1

(defstruct node val previous next)

(defun insert-after (node new-node)
  (setf (node-next new-node) (node-next node)
	(node-previous new-node) node
	(node-previous (node-next node)) new-node
	(node-next node) new-node)
  new-node)

(defun remove-node (node)
  (setf (node-next (node-previous node)) (node-next node)
	(node-previous (node-next node)) (node-previous node))
  (node-next node))

(defun make-initial-node (val)
  (let ((initial-node (make-node :val val)))
    (setf (node-previous initial-node) initial-node
	  (node-next initial-node) initial-node)
    initial-node))

(defun game (players last-marble)
  (loop :with score := (make-hash-table :size players)
	:for player := -1 :then (mod (1+ player) players)
	:for marble :from 0 :upto last-marble
	:for current-node := (make-initial-node 0)
	  :then (if (integerp (/ marble 23))
		    (progn
		      (loop :repeat 7
			    :do (setf current-node (node-previous current-node)))
		      (incf (gethash player score 0)
			    (+ marble (node-val current-node)))
		      (remove-node current-node))
		    (insert-after (node-next current-node) (make-node :val marble)))
	:maximize (gethash player score 0)))

(defun answer-1 ()
  (game 418 71339))

;;; Part 2

(defun answer-2 ()
  (game 418 (* 71339 100)))

