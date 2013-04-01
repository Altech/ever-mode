(define-derived-mode ever-mode nil "EverMode" "Managements many notes."
  (define-key ever-mode-map (kbd "n") 'ever-goto-next-note)
  (define-key ever-mode-map (kbd "p") 'ever-goto-previous-note)
  (define-key ever-mode-map (kbd "s") 'ever-search-notes)
  (define-key ever-mode-map (kbd "q") 'ever-quit)
  (define-key ever-mode-map (kbd "a") 'ever-add-note)
  (define-key ever-mode-map (kbd "d") 'ever-mark-delete)
  (define-key ever-mode-map (kbd "u") 'ever-unmark-delete)
  (define-key ever-mode-map (kbd "x") 'ever-mark-execute)
  (define-key ever-mode-map (kbd "r") 'ever-rename-note)
  )

(defun ever-buffer-switch (buf)
  (if (string-match "ever" (buffer-name))
      (switch-to-buffer buf)
    (pop-to-buffer buf)))

(defun ever-notes ()
  (interactive)
  (ever-buffer-switch "*ever-notes*")
  (with-current-buffer "*ever-notes*"
    (ever-mode)
    (hl-line-mode)
    (setq buffer-read-only t))
  (setq ever-delete-mark-list nil)
  (ever-render-file-list)
  (setq grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} /dev/null \\;")
  (goto-char (point-min))
  (goto-line 3)
  (ever-goto-previous-note)
  ;(put-text-property 1 10 'box (color-values "gray"))
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
	(error (insert "Warning: File not found.")))))))

(defun ever-search-notes (regexp)
  (interactive "sSearch: ")
  (rgrep regexp "*" ever-root-directroy nil))

(defun ever-quit ()
  (interactive)
  (kill-buffer "*ever-notes*")  
  (dolist (path (ever-notes-filter (directory-files ever-root-directroy t)))
    (when (get-buffer (file-name-nondirectory path))
      (kill-buffer (file-name-nondirectory path))))
  (delete-window (other-window 1))
  (setq ever-delete-mark-list nil))

(defun ever-add-note (title ext)
  (interactive "sTitle: \nsExtension: ")
  (if (file-exists-p (expand-file-name (concat title "." ext) ever-root-directroy))
      (message "File already exists.")
    (with-current-buffer (pop-to-buffer nil)
      (find-file (expand-file-name (concat title "." ext) ever-root-directroy))
      (insert "dummy")
      (erase-buffer)
      (save-buffer))
    (ever-render-file-list)))

(defun ever-render-file-list ()
  (with-current-buffer "*ever-notes*"
    (ever-buffer-writable
     (erase-buffer)
     (let* ((files (ever-notes-filter (directory-files ever-root-directroy t))) (lists (mapcar 'ever-file-to-list files)) (column-width (ever-get-column-width lists)))
       (setq lists (sort lists (lambda (l1 l2)
				 (not (string< (nth 0 l1) (nth 0 l2))))))
       (setq lists (cons '("Updated-at" "Ext" "Title") lists))
       (erase-buffer)
       (insert "\n")
       (insert (ever-set-faces-to-table (mapconcat (lambda (ls) (join ls "|")) (ever-make-table lists column-width) "\n")))
       (insert "\n\n\n [n]: next-note [p]: previous-note [a]: add-note [s]: search [q]: quit \n\n [r]: rename-note [d]: mark-delete [u]: unmark [x]: execute-mark\n")))))

(defvar ever-delete-mark-list nil
  "Marking list in ever-notes. It is a list of filename.")

(defun ever-mark-delete ()
  (interactive)
  (ever-buffer-writable
   (beginning-of-line)
   (unless (eq (char-after) ?D)
     (let ((s (thing-at-point 'line)))
       (when (string-match "^ \\([^| ]+\\) +| \\([^| ]+\\) +| \\([^| ]+\\) +$" s)
	 (setq ever-delete-mark-list (cons (concat (match-string 3 s) "." (match-string 2 s)) ever-delete-mark-list)))
       (delete-char 1) (insert "D")))
   (beginning-of-line)))

(defun ever-unmark-delete ()
  (interactive)
  (ever-buffer-writable
   (beginning-of-line)
   (if (eq (char-after) ?D)
     (let ((s (thing-at-point 'line)))
       (when (string-match "^D\\([^| ]+\\) +| \\([^| ]+\\) +| \\([^| ]+\\) +$" s)
	 (setq ever-delete-mark-list (remove (concat (match-string 3 s) "." (match-string 2 s)) ever-delete-mark-list)))
       (delete-char 1) (insert " "))))
  (beginning-of-line))

(defun ever-mark-execute ()
  (interactive)
  (dolist (file ever-delete-mark-list)
    (delete-file (expand-file-name file ever-root-directroy)))
  (ever-render-file-list))

(defun ever-rename-note (title ext)
  (interactive "sTitle: \nsExtension: ")
  (when (string-match "^.\\([^| ]+\\) +| \\([^| ]+\\) +| \\([^| ]+\\) +$" (thing-at-point 'line))
    (let* ((line (thing-at-point 'line)) (old-update-date (match-string 1 line)) (old-ext (match-string 2 line)) (old-title (match-string 3 line)))
      (ever-rename-note (expand-file-name (concat old-title "." old-ext) ever-root-directroy) (expand-file-name (concat title "." ext) ever-root-directroy)))
    (ever-render-file-list)))

(defun ever-notes-filter (list)
  (filter (lambda (s) (not (string-match "^\\..*" (file-name-nondirectory s)))) list))

(defun ever-file-to-list (file)
  (let* ((attrs (file-attributes file)) (mtime (nth 5 attrs)))
    ;attrs ; => (nil 1 501 20 (20811 62349) (20811 13373) (20811 13373) 10128 "-rw-r--r--" nil 13741330 16777217), (nil 1 501 20 (20811 62347) (20811 13242) (20811 13242) 14321 "-rw-r--r--" nil 13741227 16777217), (nil 1 501 20 (20811 62351) (20807 28495) (20807 28495) 16 "-rw-r--r--" nil 13435475 16777217), (nil 1 501 20 (20811 62351) (20807 9536) (20807 13024) 12 "-rw-r--r--" nil 13392020 16777217), (nil 1 501 20 (20811 62351) (20807 28413) (20807 28413) 1551 "-rw-r--r--" nil 13417785 16777217)
    (list (format-time-string "%Y-%m-%d" mtime) (file-name-extension file) (file-name-nondirectory (file-name-sans-extension file)))))

(defun ever-file-to-summary (file))

;; routines
(defun ever-get-column-width (tables)
  (mapcar (lambda (list) (apply 'max (mapcar 'calc-string-width list))) (transpose tables)))


(defun ever-make-table (lists column-width)
  (mapcar (lambda (list)
	    (mapcar (lambda (cell)
		      (ever-recenter-string (car cell) (cdr cell)))
		    (zip list column-width)))
	  lists))

(defun ever-set-faces-to-table (lines)
  (let ((list (split-string lines "\n")))
    (join (cons (propertize (concat (car list) "  ") 'face 'underline) (cdr list)) "\n")))

(defmacro ever-buffer-writable (&rest body)
  `(if (not buffer-read-only)
       (progn ,@body)
     (setq buffer-read-only nil)
     (progn ,@body)
     (setq buffer-read-only t)))


;; general-purpose routines
(require 'ever-routines)

;; user settings
(setq ever-root-directroy "/Users/Altech/Notes")

(provide 'ever-mode)


;; ;; start ever-mode
;; (ever-notes)



;; [TODO]
;; - split file (setting and debug-mode and boyd
;; - spotlight search
;; - improve search result 
;; - tag
;; - improve face
;; - help mode
;; - abstract mode
;; - managements cursor
;; - create-date using GetFileInfo
;; - sort create-date / update-date
;; - search by title
;; - category by folder
;; - change modify and create date