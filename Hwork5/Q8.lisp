;; Global variables to track position and error information
(defparameter *input* nil)
(defparameter *position* 0)
(defparameter *error-message* nil)

;; Helper function to report errors
(defun report-error (expected)
  (setf *error-message* 
        (format nil "Error at position ~a: Expected '~a', found '~a'" 
                *position* 
                expected 
                (if *input* (car *input*) "end of input"))))

;; Modified match function that tracks position
(defun match (expected)
  (if (and *input* (equal (car *input*) expected))
      (progn
        (pop *input*)
        (incf *position*)
        t)
      (progn
        (report-error expected)
        nil)))

;; Non-terminal parsers with error handling
(defun parse-I ()
  (if (eq (car *input*) 'i)
      (and (match 'i)
           (or (parse-E) 
               (progn 
                 (setf *error-message* 
                       (format nil "Error at position ~a: Invalid expression" *position*))
                 nil))
           (or (parse-S)
               (progn 
                 (setf *error-message*
                       (format nil "Error at position ~a: Invalid statement" *position*))
                 nil))
           (parse-I'))
      (progn
        (report-error 'i)
        nil)))

(defun parse-I' ()
  (if (eq (car *input*) 'e)
      (and (match 'e) 
           (or (parse-S)
               (progn
                 (setf *error-message*
                       (format nil "Error at position ~a: Invalid statement after 'e'" *position*))
                 nil)))
      t))

(defun parse-E ()
  (or (parse-G) 
      (progn
        (setf *error-message*
              (format nil "Error at position ~a: Expected 'x', 'y', 'z', or 'w'" *position*))
        nil))
  (parse-E'))

(defun parse-E' ()
  (if (and *input* (eq (car *input*) 'o))
      (and (match 'o)
           (or (parse-G)
               (progn
                 (setf *error-message*
                       (format nil "Error at position ~a: Expected 'x', 'y', 'z', or 'w' after 'o'" *position*))
                 nil))
           (parse-E'))
      t))

(defun parse-G ()
  (if (and *input* (member (car *input*) '(x y z w)))
      (progn (pop *input*) (incf *position*) t)
      (progn
        (setf *error-message*
              (format nil "Error at position ~a: Expected 'x', 'y', 'z', or 'w', found '~a'" 
                      *position* 
                      (if *input* (car *input*) "end of input")))
        nil)))

(defun parse-S ()
  (cond
    ((and *input* (eq (car *input*) 's'))
     (match 's))
    ((and *input* (eq (car *input*) 'd'))
     (and (match 'd)
          (or (parse-L)
              (progn
                (setf *error-message*
                      (format nil "Error at position ~a: Invalid list after 'd'" *position*))
                nil))
          (or (match 'b)
              (progn
                (setf *error-message*
                      (format nil "Error at position ~a: Expected 'b'" *position*))
                nil))))
    (t
     (setf *error-message*
           (format nil "Error at position ~a: Expected 's' or 'd', found '~a'" 
                   *position* 
                   (if *input* (car *input*) "end of input")))
     nil)))

(defun parse-L ()
  (if (and *input* (eq (car *input*) 's))
      (progn
        (match 's)
        (if (and *input* (eq (car *input*) 's))
            (parse-L)
            t))
      (progn
        (setf *error-message*
              (format nil "Error at position ~a: Expected 's', found '~a'" 
                      *position* 
                      (if *input* (car *input*) "end of input")))
        nil)))

;; Main parse function with error reporting
(defun parse (tokens)
  (let ((*input* tokens)
        (*position* 0)
        (*error-message* nil))
    (let ((result (and (parse-I) (null *input*))))
      (if result
          (format nil "Valid string")
          (or *error-message* 
              (if *input*
                  (format nil "Error at position ~a: Unexpected tokens at end" *position*)
                  "Unknown parsing error"))))))

;; Test function to demonstrate error messages
(defun test-parser ()
  ;; Valid strings
  (format t "~%Testing valid strings:~%")
  (format t "~%'i x o y o w d s s b e s': ~a" (parse '(i x o y o w d s s b e s)))
  (format t "~%'i x o y s': ~a" (parse '(i x o y s)))
  
  ;; Invalid strings with specific errors
  (format t "~%~%Testing invalid strings:~%")
  (format t "~%'x i o y s': ~a" (parse '(x i o y s)))  ;; Should start with 'i'
  (format t "~%'i a o y s': ~a" (parse '(i a o y s)))  ;; Invalid token 'a' where 'x', 'y', 'z', or 'w' expected
  (format t "~%'i x p y s': ~a" (parse '(i x p y s)))  ;; Invalid token 'p' where 'o' expected
  (format t "~%'i x o y d s': ~a" (parse '(i x o y d s)))  ;; Missing 'b' after 'd s'
  (format t "~%'i x o y d b': ~a" (parse '(i x o y d b)))  ;; Missing 's' after 'd'
  (format t "~%'i x o y s extra': ~a" (parse '(i x o y s extra)))) 