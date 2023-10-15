(defun parse-input (&optional (file #p"day18-input.txt"))
  (make-array '(50 50) :initial-contents (uiop:read-file-lines file)))

(defun print-map (map &optional
			(x1 0)
			(x2 (first (array-dimensions map)))
			(y1 0)
			(y2 (second (array-dimensions map))))
  (loop :for y :from y1 :below y2
	:do (loop :for x :from x1 :below x2
		  :do (format t "~a" (aref map x y))
		  :finally (format t "~%"))))

(defun adjacent (map x y)
  (remove-if #'null
	     (mapcar (lambda (delta)
		       (let ((adj-x (+ (first delta) x))
			     (adj-y (+ (second delta) y)))
			 (unless (or (< adj-x 0) (>= adj-x 50)
				     (< adj-y 0) (>= adj-y 50))
			   (aref map adj-x adj-y))))
		     '((-1 -1) (0 -1) (1 -1) (-1 0) (1 0) (-1 1) (0 1) (1 1)))))

(defun new-acre (map x y)
  (let ((adjacent-acres (adjacent map x y)))
    (case (aref map x y)
      (#\. (if (>= (count #\| adjacent-acres) 3)
	       #\| #\.))
      (#\| (if (>= (count #\# adjacent-acres) 3)
	       #\# #\|))
      (#\# (if (and (find #\# adjacent-acres)
		    (find #\| adjacent-acres))
	       #\#
	       #\.)))))

(defun new-map (map)
  (let ((new (make-array '(50 50))))
    (loop :for i :from 0 :below 50
	  :do (loop :for j :from 0 :below 50
		    :do (setf (aref new i j)
			      (new-acre map i j))))
    new))

(defun resource (map)
  (loop :with wooded := 0
	:with lumberyards := 0
	:for i :from 0 :below 50
	:do (loop :for j :from 0 :below 50
		  :when (char= (aref map i j) #\|)
		    :do (incf wooded)
		  :when (char= (aref map i j) #\#)
		    :do (incf lumberyards))
	:finally (return (* wooded lumberyards))))

(defun answer ()
  (loop :repeat 11
	:for map := (parse-input)
	  :then (new-map map)
	:finally (return (resource map))))

;; (defun answer-2 ()
;;   (loop :repeat 1000; 1000000001
;; 	:for i :from 0
;; 	:for map := (parse-input)
;; 	  :then (new-map map)
;; 	:for result := (resource map)
;; 	:for p := (position result results)
;; 	:if p
;; 	  ;; Não funciona... deve ter um erro de +1 algures
;; 	  :return (nth (+ p (mod 100000000 (- i p))) results)
;; 	:else :collect result :into results))

(defun nth-calc (n l p1 p2)
  (nth (+ p1 (mod (- n p1) (1+ (- p2 p1)))) l))

(defun answer-2 ()
  (let ((results 
	  (loop :repeat 1000
		:for i :from 0
		:for map := (parse-input)
		  :then (new-map map)
		:for result := (resource map)
		:collect result)))
    (apply #'nth-calc 1000000000 results 
	   (find-cycle results))))
