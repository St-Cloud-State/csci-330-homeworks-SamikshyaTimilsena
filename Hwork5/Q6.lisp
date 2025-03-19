(defparameter *input* nil)

(defun match (expected)
  (when (and *input* (equal (car *input*) expected))
    (pop *input*) t))

(defun parse-I ()
  (when (match 'i)
    (and (parse-E) (parse-S) (parse-I'))))

(defun parse-I' ()
  (or (and (match 'e) (parse-S)) t))  ;; Ensure correct structure

(defun parse-E ()
  (and (parse-G) (parse-E')))  ;; Ensure `parse-G` succeeds first

(defun parse-E' ()
  (or (and (match 'o) (parse-G) (parse-E')) t))  ;; Allow `E -> G` case

(defun parse-G ()
  (if (member (car *input*) '(x y z w))
      (progn (pop *input*) t)
      nil))  ;; Ensure `t` only if valid symbol is found

(defun parse-S ()
  (or (match 's)
      (and (match 'd) (parse-L) (match 'b))))  ;; Enforce `d L b` structure

(defun parse-L ()
  (or (match 's) (and (match 's) (parse-L))))  ;; Avoid infinite recursion

(defun parse (tokens)
  (let ((*input* tokens))
    (and (parse-I) (null *input*))))  ;; Ensure full input is consumed

;; Test cases
(format t "~%Parsing 'i x o y o w d s s b e s': ~a" (parse '(i x o y o w d s s b e s)))
(format t "~%Parsing 'i x o y s': ~a" (parse '(i x o y s)))
