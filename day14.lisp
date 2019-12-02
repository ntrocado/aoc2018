(defun new-recipe (recipe1 recipe2)
  (let ((sum (+ recipe1 recipe2)))
    (if (>= sum 10)
	(list 1 (mod sum 10))
	(list sum))))

(defun add-recipe (scoreboard elf1 elf2)
  (mapc (lambda (x) (vector-push x scoreboard))
	(new-recipe (aref scoreboard elf1)
		    (aref scoreboard elf2)))
  scoreboard)

(defun step-elf (scoreboard elf)
  (mod (+ elf 1 (aref scoreboard elf))
       (fill-pointer scoreboard)))

(defun answer-1 (&optional (input 760221))
  (let ((init-board (make-array (+ input 10) :fill-pointer 0)))
    (vector-push 3 init-board)
    (vector-push 7 init-board)
    (loop :repeat (+ input 7)
	  :for scoreboard := init-board :then (add-recipe scoreboard elf1 elf2)
	  :for elf1 := 0 :then (step-elf scoreboard elf1)
	  :for elf2 := 1 :then (step-elf scoreboard elf2)
	  :finally (return (subseq scoreboard (- (fill-pointer scoreboard) 10))))))
