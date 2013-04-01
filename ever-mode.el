;; define
(define-derived-mode ever-mode nil "EverMode" "Managements many notes.")

;; interface
(let ((map ever-mode-map))
  (define-key map (kbd "t") 'ever-toggle-view)
  (define-key map (kbd "n") 'ever-goto-next-note)
  (define-key map (kbd "p") 'ever-goto-previous-note)
  (define-key map (kbd "s") 'ever-search-notes)
  (define-key map (kbd "a") 'ever-add-note)
  (define-key map (kbd "r") 'ever-rename-note)
  (define-key map (kbd "d") 'ever-mark-delete)
  (define-key map (kbd "u") 'ever-unmark-delete)
  (define-key map (kbd "x") 'ever-mark-execute)
  (define-key map (kbd "q") 'ever-quit)
  )

(defconst ever-view-type-list '(ever-view-type-table
				ever-view-type-summary))

;; variables
(defvar ever-root-directroy nil
  "Directory which includes notes.")

(defvar ever-view-type 0)

;; varibales(state)
(defvar ever-delete-mark-list nil
  "Marking list in ever-notes. It is a list of filename.")

;; interactive command
(defun ever-notes ()
  (interactive)
  (unless (get-buffer "*ever-notes*")
    (with-current-buffer (generate-new-buffer "*ever-notes*")
      (ever-mode)
      (hl-line-mode)
      (setq buffer-read-only t)))
  (ever-notes-init-state)
  (ever-render-view)
  (other-window-or-split) ; patch
  (switch-to-buffer "*ever-notes*")
  (goto-char (point-min))
  (goto-line 2)
  (ever-goto-next-note)
  ;; open-recent
  )

(defun ever-goto-next-note ()
  (interactive)
  (when (ever-exist-next-note)
    (next-line)
    (ever-pop-buffer-of-current-note)
    (pop-to-buffer "*ever-notes*")))

(defun ever-goto-previous-note ()
  (interactive)
  (when (ever-exist-previous-note)
    (previous-line)
    (ever-pop-buffer-of-current-note)
    (pop-to-buffer "*ever-notes*")))

(defun ever-quit ()
  (interactive)
  (kill-buffer "*ever-notes*")  
  (dolist (path (ever-get-note-filter (directory-files ever-root-directroy t)))
    (when (get-buffer (file-name-nondirectory path))
      (kill-buffer (file-name-nondirectory path))))
  (delete-window (other-window 0))
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
    (ever-render-view)
    (goto-line 3)))

(defun ever-rename-note (title ext)
  (interactive "sTitle: \nsExtension: ")
  (let ((note (ever-parse-note)) (line (line-number-at-pos (point))))
	(when note
	  (if (get-buffer  (concat (cdr (assq 'title note)) "." (cdr (assq 'ext note))))
	      (kill-buffer (concat (cdr (assq 'title note)) "." (cdr (assq 'ext note)))))
	  (rename-file (expand-file-name (concat (cdr (assq 'title note)) "." (cdr (assq 'ext note))) ever-root-directroy) (expand-file-name (concat title "." ext) ever-root-directroy))
	  (ever-render-view)
	  (goto-line line)
	  )))

(defun ever-mark-delete ()
  (interactive)
  (with-ever-notes
   (let ((note (ever-parse-note)))
     (unless (string-equal "D" (cdr (assq 'mark note)))
       (add-to-list 'ever-delete-mark-list (concat (cdr (assq 'title note)) "." (cdr (assq 'ext note))))
       (delete-char 1) (insert "D"))
     (beginning-of-line))))

(defun ever-unmark-delete ()
  (interactive)
  (with-ever-notes
   (let ((note (ever-parse-note)))
     (when (string-equal "D" (cdr (assq 'mark note)))
       (setq ever-delete-mark-list (remove (concat (cdr (assq 'title note)) "." (cdr (assq 'ext note))) ever-delete-mark-list))
       (delete-char 1) (insert " "))
     (beginning-of-line))))

(defun ever-mark-execute ()
  (interactive)
  (dolist (file ever-delete-mark-list)
    (delete-file (expand-file-name file ever-root-directroy)))
  (ever-render-view)
  (goto-line 3))

(defun ever-search-notes (regexp)
  (interactive "sSearch: ")
  (rgrep regexp "*" ever-root-directroy nil))

;; non interactive
(defun ever-notes-init-state ()
  (setq ever-delete-mark-list nil)
  (setq grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} /dev/null \\;") ;; patch
  )

(defun ever-render-view ()
  (nth ever-view-type ever-view-type-list)
  (with-ever-notes
   (erase-buffer)
   (insert "\n")
   (insert (funcall (nth ever-view-type ever-view-type-list)))
   (insert "\n\n\n [n]: next-note [p]: previous-note [a]: add-note [s]: search [q]: quit \n\n [r]: rename-note [d]: mark-delete [u]: unmark [x]: execute-mark\n")))

(defun ever-pop-buffer-of-current-note ()
  (let ((note (ever-parse-note)))
    (with-current-buffer (pop-to-buffer nil)
	(condition-case err
	  (find-file (expand-file-name (cdr (assq 'path note)) ever-root-directroy))
	(error (insert "Warning: File not found."))))))

(defun ever-view-type-table ()
  (let ((notes (ever-sort-note-list (ever-get-note-list))))
    notes ; => 
    (let* ((table (mapcar (lambda (notes) (mapcar 'cdr (list (assq 'updated notes) (assq 'ext notes) (assq 'title notes)))) notes)) (column-width (ever-calc-column-width table)))
      table ; => 
      (setq table (cons '("Updated-at" "Ext" "Title") table))
      (let ((indented-table (mapcar (lambda (list)
				      (mapcar (lambda (cell)
						(ever-recenter-string (car cell) (cdr cell)))
					      (zip list column-width)))
				    table)))
	(ever-add-faces-to-table (mapconcat (lambda (ls) (join ls "|")) indented-table "\n"))))))

(defun ever-get-note-list ()
  ;; return list of notes
  ;; note is alist. the key of the alist is (category title ext updated created path).
  (let ((paths (ever-get-all-path-of-note)))
    (mapcar (lambda (path)
	      (let ((fi (shell-command-to-string (concat "/usr/bin/GetFileInfo -d " path))))
		(string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" fi)
		(list
		 (cons 'category nil)
		 (cons 'title (file-name-nondirectory (file-name-sans-extension path)))
		 (cons 'ext (file-name-extension path))
		 (cons 'updated (format-time-string "%Y-%m-%d" (nth 5 (file-attributes path))))
		 (cons 'created (format "%s-%s-%s" (match-string 3 fi) (match-string 1 fi) (match-string 2 fi)))
		 (cons 'path path))
		))
	    paths)))

(defun ever-sort-note-list (notes)
  (sort notes (lambda (n1 n2)
		(not (string< (cdr (assq 'updated n1)) (cdr (assq 'updated n2)))))))

(defun ever-get-note-filter (list)
  (filter (lambda (s) (not (string-match "^[\\.#].*" (file-name-nondirectory s)))) list))

(defun ever-get-all-path-of-note ()
  "example: return (test.md work/memo.org ...)"
  ;; [TODO] deal with nest directory
  (ever-get-note-filter (directory-files ever-root-directroy t)))

(defun ever-calc-column-width (tables)
  (mapcar (lambda (list) (apply 'max (mapcar 'calc-string-width list))) (transpose tables)))

(defun ever-add-faces-to-table (table-str)
  (let ((list (split-string table-str "\n")))
    (join (cons (propertize (concat (car list) "  ") 'face 'underline) (cdr list)) "\n")))

(defun ever-exist-next-note ()
  (cond ((eq ever-view-type 0)
	 (next-line)
	 (let ((result (ever-parse-note)))
	   (previous-line)
	   result
	   ))
	((eq ever-view-type 1)
	 (error "unimplemented view-type.")
	 )
	(t (error "unexpected value ever-view-type"))))

(defun ever-exist-previous-note ()
  (cond ((eq ever-view-type 0)
	 (previous-line)
	 (let ((result (ever-parse-note)))
	   (next-line)
	   result
	   ))
	((eq ever-view-type 1)
	 (error "unimplemented view-type.")
	 )
	(t (error "unexpected value ever-view-type"))))

(defun ever-parse-note ()
  (cond ((eq ever-view-type 0)
	 (let ((str (thing-at-point 'line)))
	   (if (or (not (string-match "^\\([ D]\\)\\([^| ]+\\) +| +\\([^| ]+\\) +| \\([^|\n ]+\\) +" str)) (string-match " Ext " str))
	       nil
	     (zip (list 'category 'title 'ext 'updated 'created 'path 'mark)
		  (list nil (match-string 4 str) (match-string 3 str) (match-string 2 str) nil (format "%s.%s" (match-string 4 str) (match-string 3 str)) (match-string 1 str))))))
	((eq ever-view-type 1)
	 t
	 )))

(defmacro with-ever-notes (&rest body)
  `(with-current-buffer "*ever-notes*"
     (if (not buffer-read-only)
	 (progn ,@body)
       (setq buffer-read-only nil)
       (progn ,@body)
       (setq buffer-read-only t))))


;; general-purpose routines
(require 'ever-routines)

;; user settings
(setq ever-root-directroy "/Users/Altech/Notes")

(provide 'ever-mode)

;; ;; start ever-mode
;; (ever-notes)



;; [TODO]
;; - category using folder
;; - tag on filename
;; - spotlight search
;; - improve search result 
;; - improve face
;; - abstract mode
;; - managements cursor
;; - create-date using GetFileInfo
;; - sort create-date / update-date
;; - search by title
