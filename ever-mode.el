(define-derived-mode ever-mode nil "EverMode" "Managements many notes."
  (define-key ever-mode-map (kbd "n") 'ever-goto-next-note)
  (define-key ever-mode-map (kbd "p") 'ever-goto-previous-note))

(defun ever-buffer-switch (buf)
  (if (string-match "ever" (buffer-name))
      (switch-to-buffer buf)
    (pop-to-buffer buf)))

(defun ever-notes ()
  (interactive)
  (ever-buffer-switch "*ever-notes*")
  (with-current-buffer "*ever-notes*"
    (ever-mode)
    (hl-line-mode))
  (setq buffer-read-only nil)
  (let* ((files (ever-notes-filter (directory-files ever-root-directroy t))) (lists (mapcar 'ever-file-to-list files)) (column-width (ever-get-column-width lists)))
    ;; lists ; => (("2012-10-01" "md" "test") ("2012-10-01" "txt" "test") ("2012-10-01" "org" "オレオレEvernote"))
    (setq lists (cons '("Update" "Ext" "Title") lists))
    (erase-buffer)
    (insert "\n")
    (point) ; => 2
    (insert (mapconcat (lambda (ls) (join ls "|")) (ever-make-table lists column-width) "\n")) ; => nil
    (insert "\n")
  )
  (goto-line 3)
  (setq buffer-read-only t))


(defvar ever-root-directroy nil
  "Directory which includes notes.")

(defun ever-goto-next-note ()
  (interactive)
  ;; 
  (re-search-forward "^\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)\n\\([^|]+\\)|")
  (beginning-of-line)
  ;;
  (ever-pop-buffer-of-line (thing-at-point 'line))
  (pop-to-buffer "*ever-notes*"))

(defun ever-goto-previous-note ()
  (interactive)
  (re-search-backward "|\\([^|]+\\)\n\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)\n")
  (end-of-line) (goto-char (1+ (point)))
  (ever-pop-buffer-of-line (thing-at-point 'line))
  (pop-to-buffer "*ever-notes*"))

(defun ever-pop-buffer-of-line (s)
  (when (string-match "^ \\([^| ]+\\) +| \\([^| ]+\\) +| \\([^| ]+\\) +$" s)
    (let ((file (concat (match-string 3 s) "." (match-string 2 s))))
      (with-current-buffer (pop-to-buffer nil)
	(condition-case err
	  (find-file (expand-file-name file ever-root-directroy))
	(error (insert "Warning: File not found."))))
      )))


(defun ever-notes-filter (list)
  (filter (lambda (s) (not (string-match "^\\..*" (file-name-nondirectory s)))) list)
  )

(defun ever-file-to-list (file)
  (let* ((attrs (file-attributes file)) (mtime "2012-10-01"))
    (list mtime (file-name-extension file) (file-name-nondirectory (file-name-sans-extension file)))))

(defun ever-file-to-summary (file))

;; routines
(defun ever-get-column-width (tables)
  (mapcar (lambda (list) (apply 'max (mapcar 'calc-string-width list))) (transpose tables))
  )

(defun ever-make-table (lists column-width)
  (mapcar (lambda (list)
	    (mapcar (lambda (cell)
		      (ever-recenter-string (car cell) (cdr cell)))
		    (zip list column-width)))
	  lists))

;; general-purpose routines

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

;; (zip '(1 2 5) '(3 4 5)) ; => ((1 . 3) (2 . 4) (5 . 5))

;; (ever-recenter-string "test" 10) ; => " test       "

;; (mapcar 'calc-char-width (string-to-list "あa")) ; => (2 1)
;; (calc-string-width  "あa") ; => 3

;; user settings

(setq ever-root-directroy "/Users/Altech/Notes")



(ever-notes)



(ever-goto-next-note)
