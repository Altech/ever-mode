
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

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(provide 'ever-routines)