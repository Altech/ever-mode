(define-derived-mode ever-mode nil "EverMode" "Managements many notes."
  (define-key ever-mode-map (kbd "n") 'ever-goto-next-note)
  (define-key ever-mode-map (kbd "p") 'ever-goto-previous-note)
  (define-key ever-mode-map (kbd "s") 'ever-search-notes)
  (define-key ever-mode-map (kbd "q") 'ever-quit))

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
    files ; => ("/Users/Altech/Notes/oauth.md" "/Users/Altech/Notes/scala.org" "/Users/Altech/Notes/test.md" "/Users/Altech/Notes/test.txt" "/Users/Altech/Notes/オレオレEvernote.org")
    lists ; => (("2013-03-22" "md" "oauth") ("2013-03-22" "org" "scala") ("2013-03-19" "md" "test") ("2013-03-18" "txt" "test") ("2013-03-19" "org" "オレオレEvernote"))
    (setq lists (cons '("Updated-at" "Ext" "Title") lists))
    (erase-buffer)
    (insert "\n [n]: next-note [p]: previous-note [s]: search [q]: quit \n\n")
    (point) ; => 61
    (insert (mapconcat (lambda (ls) (join ls "|")) (ever-make-table lists column-width) "\n")) ; => nil
    (insert "\n")
  )
  (setq buffer-read-only t)
  ;; (setq grep-template "grep <X> <C> -nH -e <R> <F>")
  (setq grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} /dev/null \\;")
  (beginning-of-buffer)
  (goto-line 6)
  (ever-goto-previous-note)
  )


(defvar ever-root-directroy nil
  "Directory which includes notes.")

(defun ever-goto-next-note ()
  (interactive)
  (when (re-search-forward "^\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)\n\\([^|]+\\)|" nil t)
    (beginning-of-line)
    (ever-pop-buffer-of-line (thing-at-point 'line))
    (pop-to-buffer "*ever-notes*")))

(defun ever-goto-previous-note ()
  (interactive)
  (when (re-search-backward "|\\([^|]+\\)\n\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)\n" nil t)
    (end-of-line) (goto-char (1+ (point)))
    (ever-pop-buffer-of-line (thing-at-point 'line))
    (pop-to-buffer "*ever-notes*")))

(defun ever-pop-buffer-of-line (s)
  (when (string-match "^ \\([^| ]+\\) +| \\([^| ]+\\) +| \\([^| ]+\\) +$" s)
    (let ((file (concat (match-string 3 s) "." (match-string 2 s))))
      (with-current-buffer (pop-to-buffer nil)
	(condition-case err
	  (find-file (expand-file-name file ever-root-directroy))
	(error (insert "Warning: File not found."))))
      )))

(defun ever-search-notes (regexp)
  (interactive "sSearch: ")
  regexp ; => 
  (rgrep regexp "*" ever-root-directroy nil)
  )

(defun ever-quit ()
  (interactive)
  (kill-buffer "*ever-notes*")
  (dolist (path (ever-notes-filter (directory-files ever-root-directroy t)))
    (when (get-buffer (file-name-nondirectory path))
      (kill-buffer (file-name-nondirectory path)))
    ))

(defun ever-notes-filter (list)
  (filter (lambda (s) (not (string-match "^\\..*" (file-name-nondirectory s)))) list)
  )

(defun ever-file-to-list (file)
  (let* ((attrs (file-attributes file)) (mtime (nth 5 attrs)))
    ;attrs ; => (nil 1 501 20 (20811 62349) (20811 13373) (20811 13373) 10128 "-rw-r--r--" nil 13741330 16777217), (nil 1 501 20 (20811 62347) (20811 13242) (20811 13242) 14321 "-rw-r--r--" nil 13741227 16777217), (nil 1 501 20 (20811 62351) (20807 28495) (20807 28495) 16 "-rw-r--r--" nil 13435475 16777217), (nil 1 501 20 (20811 62351) (20807 9536) (20807 13024) 12 "-rw-r--r--" nil 13392020 16777217), (nil 1 501 20 (20811 62351) (20807 28413) (20807 28413) 1551 "-rw-r--r--" nil 13417785 16777217)
    (list (format-time-string "%Y-%m-%d" mtime) (file-name-extension file) (file-name-nondirectory (file-name-sans-extension file)))))

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

;; [TEST]
;; (zip '(1 2 5) '(3 4 5)) ; => ((1 . 3) (2 . 4) (5 . 5))
;; (ever-recenter-string "test" 10) ; => " test       "
;; (mapcar 'calc-char-width (string-to-list "あa")) ; => (2 1)
;; (calc-string-width  "あa") ; => 3



;; user settings
(setq ever-root-directroy "/Users/Altech/Notes")


;; start ever-mode
;; (ever-notes)




;; [TODO]
;; - sort note
;; - spotlight search
;; - improve search result 
;; - tag
;; - improve face