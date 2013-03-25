
(defun filter (p list)
  (delq nil (mapcar (lambda (x) (if (funcall p x) x)) list)))

(defun join (list sep)
  (mapconcat 'identity list sep))

(defun transpose (matrix)
  (reverse (transpose-iter matrix nil)))

(defun transpose-iter (matrix transposed)
  (if (null (car matrix))
      transposed
    (transpose-iter (mapcar 'cdr matrix) (cons (mapcar 'car matrix) transposed))))

(defun calc-char-width (char)
  (if (< char 128) 1 1.5))

(defun calc-string-width (str)
  (reduce '+ (mapcar 'calc-char-width (string-to-list str))))

(defun ever-recenter-string (str width)
  (let* ((str-width (calc-string-width str)) (spaces (- width str-width)))
	(concat " " str (make-string (round (1+ spaces)) ?\s))))

(defun zip (list1 list2)
  (cond
   ((null list1) nil)
   ((null list2) nil) 
   (t (cons (cons (car list1) (car list2)) (zip (cdr list1) (cdr list2))))))

;; [TEST]
;; (zip '(1 2 5) '(3 4 5)) ; => ((1 . 3) (2 . 4) (5 . 5))
;; (ever-recenter-string "test" 10) ; => " test       "
;; (mapcar 'calc-char-width (string-to-list "あa")) ; => (2 1)
;; (calc-string-width  "あa") ; => 3


(provide 'ever-routines)