(defpackage #:day-16
  (:use #:cl))

(in-package #:day-16)

(defparameter *opcodes* '())

(defmacro defopcode (name &body body)
  (unless (member name *opcodes*) (push name *opcodes*))
  `(defun ,name (registers a b c)
     (setf (aref registers c)
	   ,@body)
     registers))

(defmacro reg-a ()
  `(aref registers a))

(defmacro reg-b ()
  `(aref registers b))

(defmacro reg-c ()
  `(aref registers c))

(defmacro reg-d ()
  `(aref registers d))

(defopcode addr
  (+ (reg-a) (reg-b)))

(defopcode addi
  (+ (reg-a) b))

(defopcode mulr
  (* (reg-a) (reg-b)))

(defopcode muli
  (* (reg-a) b))

(defopcode banr
  (logand (reg-a) (reg-b)))

(defopcode bani
  (logand (reg-a) b))

(defopcode borr
  (logior (reg-a) (reg-b)))

(defopcode bori
  (logior (reg-a) b))

(defopcode setr
  (reg-a))

(defopcode seti
  a)

(defopcode gtir
  (if (> a (reg-b))
      1 0))

(defopcode gtri
  (if (> (reg-a) b)
      1 0))

(defopcode gtrr
  (if (> (reg-a) (reg-b))
      1 0))

(defopcode eqir
  (if (= a (reg-b))
      1 0))

(defopcode eqri
  (if (= (reg-a) b)
      1 0))

(defopcode eqrr
  (if (= (reg-a) (reg-b))
      1 0))

(defun run-instruction (instruction registers)
  (let ((opcode (aref instruction 0))
	(a      (aref instruction 1))
	(b      (aref instruction 2))
	(c      (aref instruction 3)))
    (funcall opcode registers a b c)))

(defun behaves-like (before-registers instruction after-registers)
  (loop :with c := 0
	:for opcode :in *opcodes*
	:for %instruction := (progn (setf (aref instruction 0)
					  opcode)
				    instruction)
	:when (equalp (run-instruction %instruction (copy-seq before-registers))
		      after-registers)
	  :do (incf c)
	  :and :collect opcode :into opcodes
	:finally (return (values c opcodes))))

(defun read-input ()
  (uiop:read-file-string #p"day16-input.txt"))

(defparameter *input* (ppcre:split "\\n\\n\\n" *input*))

(defun input->numbers (str)
  (let ((matches))
   (ppcre:do-matches-as-strings (match "\\d+" str (reverse matches))
     (push (parse-integer match) matches))))

(defun parse-sample (sample)
  (let ((before-registers (make-array 4 :fill-pointer 0))
	(instruction      (make-array 4 :fill-pointer 0))
	(after-registers  (make-array 4 :fill-pointer 0)))
    (loop :repeat 4 :do (vector-push (pop sample) before-registers))
    (loop :repeat 4 :do (vector-push (pop sample) instruction))
    (loop :repeat 4 :do (vector-push (pop sample) after-registers))
    (behaves-like before-registers instruction after-registers)))

(defun count-samples-from-input-numbers (input)
  (loop :for sample :on input :by (lambda (x) (subseq x 12))
	:while sample
	:count (>= (parse-sample sample) 3)))

(defun answer-1 ()
  (count-samples-from-input-numbers (input->numbers (first *input*))))

;;; Part 2
(defun remove-duplicate-opcode (ht opcode)
  (maphash (lambda (k v)
	     (unless (= (length v) 1)
	       (setf (gethash k ht)
		     (delete (car opcode) v))))
	   ht)
  ht)

(defun flatten-ht-values (ht)
  (loop :for v :being :the :hash-keys :of ht
	:do (setf (gethash v ht) (car (gethash v ht)))
	:finally (return ht)))

(defun opcode-numbers (input)
  (loop :with ht := (make-hash-table :size 16 :test 'equalp)
	:for sample :on input :by (lambda (x) (subseq x 12))
	:for opcode-number := (nth 4 sample)
	:for opcodes := (nth-value 1 (parse-sample sample))
	:while sample
	:if (gethash opcode-number ht)
	  :do (setf (gethash opcode-number ht)
		    (intersection opcodes (gethash opcode-number ht)))
	:else :do (setf (gethash opcode-number ht) opcodes)
	;; single opcode remaining
	:when (= 1 (length (gethash opcode-number ht)))
	      :do (remove-duplicate-opcode ht (gethash opcode-number ht))
	:finally (return (flatten-ht-values ht))))

(defun execute (registers instruction opcode-ht)
  (funcall (gethash (aref instruction 0) opcode-ht)
	   registers
	   (aref instruction 1)
	   (aref instruction 2)
	   (aref instruction 3)))

(defun answer-2 ()
  (with-input-from-string (in (second *input*))
    (loop :with opcode-ht := (opcode-numbers (input->numbers (first *input*)))
	  :for instruction := (input->numbers (read-line in nil))
	  :for registers := #(0 0 0 0)
	    :then (if instruction
		      (execute registers (coerce instruction 'vector) opcode-ht)
		      (return (aref registers 0))))))
