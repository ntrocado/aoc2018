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
  ((alive :initform t)
   (pos :initarg :pos
	:initform (error "Pos must be initialized when creating unit.")
	:accessor pos)))

(defclass elf (unit)
  ((hp :initform *elf-hit-points*
       :accessor hp)
   (attack-power :initform *elf-attack-power*
		 :reader attack-power)))

(defclass goblin (unit)
  ((hp :initform *goblin-hit-points*
       :accessor hp)
   (attack-power :initform *goblin-attack-power*
		 :reader attack-power)))

(defclass battle ()
  ((input :initarg :input
	  :initform #p"day15-input.txt")
   (x :initarg :x
      :initform 32)
   (y :initarg :y
      :initform 32)
   (cave :accessor cave)
   (units :accessor units)))

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
  (with-accessors ((cave cave))
      battle
    (destructuring-bind (n m)
	(array-dimensions (cave battle))
      (loop :for i :below n
	    :append (loop :for j :below m
			  :for square := (aref cave i j)
			  :when (char= square +elf+)
			    :collect (make-instance 'elf :pos (list i j))
			  :when (char= square +goblin+)
			    :collect (make-instance 'goblin :pos (list i j)))))))

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

(defmethod update-cave-map ((battle battle))
  (with-accessors (cave units) battle
    ()))

(defmethod remove-unit ((unit unit) (battle battle))
  (setf (units battle) (delete unit (units battle)))
  (destructuring-bind (x y)
      (pos unit)
    (setf (aref (cave battle) x y) +cavern+)))

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
						 (remove-if-not (lambda (x)
								  (member (rest x)
									  (adjacent-pos (first current)
											(second current))
									  :test 'equal))
								(remove-if-not (lambda (x)
										 (= (first x) i))
									       path-nodes))))
			       #'reading-order))
	  :collect current)))

(defun old-possible-paths (cave origin destination)
  (loop :for i :from 0 :upto (apply #'max (array-dimensions cave))
	:for queue := (list (push i destination)) ;TODO Em vez de lista, hash-table
	  :then (loop :with new-queue := queue
		      :for square :in queue
		      :for adjacent := (remove-if (lambda (x)
						    (find x new-queue :key (lambda (x) (subseq x 1)) :test 'equal))
						  (adjacent-open-squares cave (second square) (third square)))
		      :when (member origin (adjacent-pos (second square)
							 (third square))
				    :test 'equal)
			:return (push (push i origin) new-queue)
		      :do (mapc (lambda (x) (push (push i x) new-queue)) adjacent)
		      :finally (return new-queue))
	:when (member origin queue :test 'equal)
	  :return queue))

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

(defun closer (unit squares-in-range)
  (first (stable-sort (sort (copy-seq squares-in-range) #'reading-order)
		      #'< :key (lambda (square)
				 (manhattan-distance (first (pos unit)) (second (pos unit))
						     (first square) (second square))))))

(defmethod move ((unit unit) (battle battle))
  (update-square battle (pos unit) +cavern+)
  (alexandria:when-let
      ((path (shortest-path (cave battle)
			    (pos unit)
			    (closer unit
				    (reachable unit
					       (squares-in-range unit battle)
					       (cave battle))))))
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

(defun unit-in-range-of-a-target-p 
  (not (null (adjacent-targets unit battle))))

(defmethod turn ((unit unit) (battle battle))
  (when (member unit (units battle))
    (if (all-possible-targets unit battle)
	(progn
	  (unless (adjacent-targets unit battle)
	    (move unit battle))
	  (attack unit battle))
	'combat-ended)))

(defmethod sort-units ((battle battle))
  (setf (units battle) (sort (units battle) #'reading-order :key #'pos)))

(defmethod one-round ((battle battle))
  (loop :for unit :in (sort-units battle)
	:when (eql (turn unit battle) 'combat-ended)
	  :return 'combat-ended))

(defun combat (&optional (battle *battle*) (test nil))
  (loop :for i :from 1
	:for round := (one-round battle)
	:when test
	  :do (format t "~&after round ~a~%~a~%~a~%" i battle (units battle))
	:when (zerop (mod i 10)) :do (format t ".")
	:until (eql round 'combat-ended)
	:finally (return (format t "Combat ends after ~a full rounds, with ~a hit points left.~%Outcome: ~a"
				 (1- i) (reduce #'+ (mapcar #'hp (units battle)))
				 (* (1- i) (reduce #'+ (mapcar #'hp (units battle))))))))
