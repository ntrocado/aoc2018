(defstruct cart pos direction (next-intersection 'left))

(defconstant +north+ 0)
(defconstant +east+ 1)
(defconstant +south+ 2)
(defconstant +west+ 3)

(defun read-track (file)
  (with-open-file (data file)
    (make-array '(150 150)
		:initial-contents (loop :for l := (read-line data nil)
					:while l
					:collect (subseq l 0 (1- (length l)))))))

(defun remove-carts (track)
  (loop :for i :below (array-total-size track)
	:when (find (row-major-aref track i) "<>")
	  :do (setf (row-major-aref track i) #\-)
	:when (find (row-major-aref track i) "v^")
	  :do (setf (row-major-aref track i) #\|))
  track)

(defun init-carts-and-track (track)
  (destructuring-bind (n m) (array-dimensions track)
    (values
     (loop :with id := 0
	   :for i :below n
	   :when (loop :for j :below m
		       :when (let ((square (aref track i j)))
			       (when (find square "<>v^")
				 (prog1 (make-cart :pos (list j i)
						   :direction (ecase square
								(#\< #.+west+)
								(#\> #.+east+)
								(#\v #.+south+)
								(#\^ #.+north+)))
				   (incf id))))
			 :collect :it)
	     :append :it)
     (remove-carts track))))

(defun turn-left (current-dir)
  (mod (1- current-dir) 4))

(defun turn-right (current-dir)
  (mod (1+ current-dir) 4))

(defun update-next-intersection (current-dir)
  (case current-dir
    (left 'straight)
    (straight 'right)
    (right 'left)))

(defun detect-crash (carts)
  (let ((ht (make-hash-table :test 'equal)))
    (loop :for cart :in carts
	  :if (gethash (cart-pos cart) ht)
	    :return (list cart (gethash (cart-pos cart) ht))
	  :else
	    :do (setf (gethash (cart-pos cart) ht) cart))))

(defun update-cart (cart track)
  (destructuring-bind (x y) (cart-pos cart)
    (setf (cart-pos cart) (ecase (cart-direction cart)
			    (#.+north+ (list x (1- y)))
			    (#.+east+ (list (1+ x) y))
			    (#.+south+ (list x (1+ y)))
			    (#.+west+ (list (1- x) y))))
    (setf (cart-direction cart)
	  (let ((new-square (aref track
				  (cadr (cart-pos cart))
				  (car (cart-pos cart)))))
	    (ecase new-square
	      ((#\| #\-) (cart-direction cart))
	      (#\\ (ecase (cart-direction cart)
		     (#.+north+ +west+)
		     (#.+east+ +south+)
		     (#.+south+ +east+)
		     (#.+west+ +north+)))
	      (#\/ (ecase (cart-direction cart)
		     (#.+north+ +east+)
		     (#.+east+ +north+)
		     (#.+south+ +west+)
		     (#.+west+ +south+)))
	      (#\+ (prog1 (ecase (cart-next-intersection cart)
			    (straight (cart-direction cart))
			    (left (turn-left (cart-direction cart)))
			    (right (turn-right (cart-direction cart))))
		     (setf (cart-next-intersection cart)
			   (update-next-intersection (cart-next-intersection cart)))))))))
  cart)

(defun update-all-carts (carts track)
  (loop :for cart :in carts
	:collect (update-cart cart track) :into results
	:when (detect-crash carts) :append :it :into crashed
	  :do (when (detect-crash carts)
		(format t "~%crash at pos ~a" (cart-pos (first (detect-crash carts)))))
	:finally (return (remove-if (lambda (x) (member x crashed)) results))))

(defun sort-carts (carts)
  (stable-sort (sort carts #'< :key (lambda (x) (car (cart-pos x))))
	       #'< :key (lambda (x) (cadr (cart-pos x)))))

(defun answer ()
  (multiple-value-bind (init-carts track)
      (init-carts-and-track (read-track #p"day13-input.txt"))
    (loop :for carts := init-carts
	    :then (sort-carts (copy-seq (update-all-carts carts track)))
	  :when (= (length carts) 1)
	    :return carts)))
