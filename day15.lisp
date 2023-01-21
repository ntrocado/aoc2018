(ql:quickload '(:array-operations :alexandria))

(defconstant +wall+ #\#)
(defconstant +cavern+ #\.)
(defconstant +goblin+ #\G)
(defconstant +elf+ #\E)

(defparameter *goblin-hit-points* 200)
(defparameter *elf-hit-points* 200)
(defparameter *goblin-attack-power* 3)
(defparameter *elf-attack-power* 3)

(defclass unit ()
  ((alive :initform t) ; unnecessary
   (pos :initarg :pos
	:initform (error "Pos must be initialized when creating unit.")
	:accessor pos)
   (attack-power :initarg :attack-power
		 :reader attack-power)))

(defclass elf (unit)
  ((hp :initform *elf-hit-points*
       :accessor hp)))

(defclass goblin (unit)
  ((hp :initform *goblin-hit-points*
       :accessor hp)))

(defclass battle ()
  ((input :initarg :input
	  :initform #p"day15-input.txt")
   (x :initarg :x
      :initform 32)
   (y :initarg :y
      :initform 32)
   (cave :accessor cave)
   (units :accessor units)
   (elf-attack-power :initarg :elf-attack-power
		     :initform *elf-attack-power*
		     :reader elf-attack-power)
   (goblin-attack-power :initarg :goblin-attack-power
			:initform *goblin-attack-power*
			:reader goblin-attack-power)))

(defmethod army ((unit unit))
  (class-name (class-of unit)))

(defun init-cave (input x y)
  (with-open-file (data input)
    (make-array (list x y)
		:initial-contents (loop :for l := (read-line data nil)
					:while l
					:collect (coerce (subseq l 0 x)
							 'list)))))

(defmethod all-units-in-map ((battle battle))
  (destructuring-bind (n m)
      (array-dimensions (cave battle))
    (loop :for i :below n
	  :append (loop :for j :below m
			:for square := (aref (cave battle) i j)
			:when (char= square +elf+)
			  :collect (make-instance 'elf
						  :pos (list i j)
						  :attack-power (elf-attack-power battle))
			:when (char= square +goblin+)
			  :collect (make-instance 'goblin
						  :pos (list i j)
						  :attack-power (goblin-attack-power battle))))))

(defmethod initialize-instance :after ((battle battle) &key)
  (with-slots (input x y) battle
    (setf (slot-value battle 'cave) (init-cave input x y)
	  (slot-value battle 'units) (all-units-in-map battle))))

(defparameter *battle* (make-instance 'battle))
(defparameter *sample-battle*
  (make-instance 'battle :input #p"day15-sample.txt" :x 7 :y 7))
(defparameter *test-battle-1*
  (make-instance 'battle :input #p"day15-test1.txt" :x 7 :y 7))
(defparameter *test-battle-2*
  (make-instance 'battle :input #p"day15-test2.txt" :x 7 :y 7))

(defun print-cave (cave &optional (stream t))
  (destructuring-bind (n m) (array-dimensions cave)
    (loop :for i :below n
	  :do (loop :for j :below m
		    :initially (format stream "~2d " i)
	      	    :do (format stream "~a" (aref cave i j))
	      	    :finally (format stream "~%")))))

(defmethod print-object ((battle battle) stream)
  (print-cave (cave battle) stream))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream)
    (format stream "army: ~a; x: ~a; y: ~a; hp: ~a"
	    (class-name (class-of unit))
	    (car (pos unit)) (cadr (pos unit))
	    (hp unit))))

;; (defmethod update-cave-map ((battle battle))
;;   (with-accessors (cave units) battle
;;     ()))

(defmethod remove-unit ((unit unit) (battle battle))
  (setf (units battle) (delete unit (units battle)))
  (destructuring-bind (x y)
      (pos unit)
    (setf (aref (cave battle) x y) +cavern+))
  (when (eql (army unit) 'elf)
    'elf-died))

(defun wall-p (cave x y)
  (char= (aref cave x y) #\#))

(defun open-p (cave x y)
  (char= (aref cave x y) #\.))

(defun reading-order (xy1 xy2)
  (destructuring-bind ((x1 y1) (x2 y2)) (list xy1 xy2)
    (if (= x1 x2)
	(< y1 y2)
	(< x1 x2))))

(defun adjacent-pos (x y)
  (list (list (1- x) y)
	(list (1+ x) y)
	(list x (1- y))
	(list x (1+ y))))

(defun adjacent-squares (cave x y &key (filter :all))
  (loop :for pos :in (adjacent-pos x y)
	:when (and (apply #'array-in-bounds-p cave pos)
		   (or (eql filter :all)
		       (char= filter (aref cave (car pos) (cadr pos)))))
	  :collect pos))

(defun adjacent-open-squares (cave x y)
  (adjacent-squares cave x y :filter +cavern+))

(defmethod update-square ((battle battle) coordinates new-square)
  (destructuring-bind (x y) coordinates
    (setf (aref (cave battle) x y) new-square)))

(defmethod all-possible-targets ((unit unit) (battle battle))
  (remove-if (lambda (x) (eql (army unit) (army x))) (units battle)))

(defmethod squares-in-range ((unit unit) (battle battle))
  (loop :for target :in (all-possible-targets unit battle)
	:append	(destructuring-bind (target-x target-y)
		    (pos target)
		  (adjacent-open-squares (cave battle) target-x target-y))))

(defun pathfinder (path-nodes-ht origin)
  (let ((path-nodes (mapcar (lambda (x)
			      (push (gethash x path-nodes-ht) x))
			    (alexandria:hash-table-keys path-nodes-ht))))
    (loop :for i :from (caar path-nodes) :downto 0
	  :for current := origin
	    :then (first (sort (copy-seq (mapcar #'rest
						 (remove-if-not
						  (lambda (x)
						    (member (rest x)
							    (adjacent-pos (first current)
									  (second current))
							    :test 'equal))
						  (remove-if-not
						   (lambda (x)
						     (= (first x) i))
						   path-nodes))))
			       #'reading-order))
	  :collect current)))

(defun possible-paths (cave origin destination)
  (loop :with ht := (make-hash-table :test 'equal)
	:initially (setf (gethash destination ht) 0)
	:for i :from 1 :upto (* (apply #'max (array-dimensions cave))
				2) ;TODO too small?
	:do (loop :for square :in (alexandria:hash-table-keys ht)
		  :for adjacent := (remove-if (lambda (x)
						(let ((previous (gethash x ht)))
						  (and previous (<= previous i))))
					      (adjacent-open-squares cave
								     (first square)
								     (second square)))
		  :when (member origin (adjacent-pos (first square)
						     (second square))
				:test 'equal)
		    :return (setf (gethash origin ht) i)
		  :do (mapc (lambda (x) (setf (gethash x ht) i)) adjacent)
		  :finally (return ht))
	:when (gethash origin ht)
	  :return ht))

(defun shortest-path (cave origin destination)
  (when destination
    (pathfinder (possible-paths cave origin destination) origin)))

(defun reachable (unit squares-in-range cave)
  (remove-if-not (lambda (x)
		   (possible-paths cave (pos unit) x))
		 squares-in-range))

(defun manhattan-distance (x1 y1 x2 y2)
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defun closer (unit squares-in-range cave)
  (first (stable-sort (sort (copy-seq squares-in-range)
			    #'reading-order)
		      #'<
		      :key (lambda (x)
			     (length (shortest-path cave (pos unit) x))))))

(defmethod move ((unit unit) (battle battle))
  (update-square battle (pos unit) +cavern+)
  (alexandria:when-let
      ((path (shortest-path (cave battle)
			    (pos unit)
			    (closer unit
				    (reachable unit
					       (squares-in-range unit battle)
					       (cave battle))
				    (cave battle)))))
    (setf (pos unit) (second path)))
  (update-square battle (pos unit) (ecase (army unit)
				     (elf +elf+)
				     (goblin +goblin+))))

(defmethod adjacent-targets ((unit unit) (battle battle))
  (destructuring-bind (x y)
      (pos unit)
    (let ((squares (adjacent-squares (cave battle) x y :filter (ecase (army unit)
								 (elf +goblin+)
								 (goblin +elf+)))))
      (remove-if-not (lambda (x) (member (pos x) squares :test 'equal)) (units battle)))))

(defmethod attack ((unit unit) (battle battle))
  (alexandria:when-let*
      ((targets (adjacent-targets unit battle))
       (target (first (stable-sort (sort (copy-seq targets)
					 #'reading-order :key #'pos)
				   #'< :key #'hp))))
    (decf (hp target) (attack-power unit))
    (when (not (plusp (hp target)))
      (remove-unit target battle))))

(defun turn (unit battle &key (part-2 nil))
  (when (member unit (units battle))
    (if (all-possible-targets unit battle)
	(progn
	  (unless (adjacent-targets unit battle)
	    (move unit battle))
	  (let ((attack-result (attack unit battle)))
	    (when (and part-2 (eql attack-result 'elf-died)) 'combat-ended)))
	'combat-ended)))

(defmethod sort-units ((battle battle))
  (setf (units battle) (sort (units battle) #'reading-order :key #'pos)))

(defun one-round (battle &key (part-2 nil))
  (loop :for unit :in (sort-units battle)
	:when (eql (turn unit battle :part-2 part-2) 'combat-ended)
	  :return 'combat-ended))

(defun combat (battle &key (test nil) (part-2 nil))
  (loop :initially (when test (print battle))
	:for i :from 1
	:for round := (one-round battle :part-2 part-2)
	:when test
	  :do (format t "~&after round ~a~%~a~%~a~%" i battle (units battle))
	:when (zerop (mod i 10)) :do (progn (format t ".") (force-output))
	:until (eql round 'combat-ended)
	:finally (return (values battle (1- i)))))

(defun answer-1 ()
  (multiple-value-bind (battle full-rounds)
      (combat *battle*)
    (let ((hp-left (reduce #'+ (mapcar #'hp (units battle)))))
      (format t "Combat ends after ~a full rounds, with ~a hit points left.~%Outcome: ~a"
	      full-rounds hp-left (* full-rounds hp-left)))))

(defun answer-2 (&key (test nil))
  (loop :for ap :from 4
	:for (battle full-rounds)
	  := (multiple-value-list (combat (make-instance 'battle
							 :input #p"day15-input.txt"
							 :elf-attack-power ap)
					  :test nil
					  :part-2 t))
	:when test
	  :do (print ap)
	:until (notany (lambda (x) (eql (army x) 'goblin))
		       (units battle))
	:finally (return (let ((hp-left (reduce #'+ (mapcar #'hp (units battle)))))
			   (format t "Combat ends after ~a full rounds, with ~a hit points left.~%Outcome: ~a"
				   full-rounds hp-left (* full-rounds hp-left))))))
