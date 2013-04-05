;; define
(define-derived-mode ever-mode nil "EverMode" "Managements many notes.")

;; commands
(let ((map ever-mode-map))
  (define-key map (kbd "t")   'ever-toggle-view)
  (define-key map (kbd "n")   'ever-goto-next-note)
  (define-key map (kbd "p")   'ever-goto-previous-note)
  (define-key map (kbd "M-<") 'ever-goto-latest-note)
  (define-key map (kbd "M->") 'ever-goto-earliest-note)
  (define-key map (kbd "s")   'ever-search-notes)
  (define-key map (kbd "a")   'ever-add-note)
  (define-key map (kbd "r")   'ever-edit-title)
  (define-key map (kbd "t")   'ever-edit-tags)
  (define-key map (kbd "c")   'ever-edit-category)
  (define-key map (kbd "d")   'ever-mark-delete)
  (define-key map (kbd "u")   'ever-unmark-delete)
  (define-key map (kbd "x")   'ever-mark-execute)
  (define-key map (kbd "q")   'ever-quit)
  (define-key map (kbd "<f5>") 'ever-update)
  )

;; constants
(defconst ever-view-type-list '(ever-view-type-table
				ever-view-type-summary))

(defconst ever-view-regexp-list (list (cons 'ever-view-type-table   "^\\([ D]\\)\\([^| ]+\\) +| +\\([^| ]+\\) +| \\([^|\n]+\\) +")
				      (cons 'ever-view-type-summary nil)))

;; variables
(defvar ever-root-directroy nil
  "Directory which includes notes.")

(defvar ever-view-type 'ever-view-type-table
  "Visual of the note list.")

(defvar ever-delete-mark-list nil
  "An internal variable. Marking list in ever-notes. It is a list of filename.")

;; library
(require 'cl)
(require 'ever-version)
(require 'ever-note)
(require 'ever-routines)

;; interactive command
(defun ever-notes ()
  "Open the note list."
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
  )

(defun ever-goto-next-note ()
  "Go to the next note."
  (interactive)
  (when (ever-exist-next-note)
    (next-line)
    (ever-pop-buffer-of-current-note)
    (pop-to-buffer "*ever-notes*")
    (beginning-of-line)))

(defun ever-goto-previous-note ()
  "Go to the previous note."
  (interactive)
  (when (ever-exist-previous-note)
    (previous-line)
    (ever-pop-buffer-of-current-note)
    (pop-to-buffer "*ever-notes*")
    (beginning-of-line)))

(defun ever-goto-latest-note ()
  "Go to the latest note."
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward (cdr (assq ever-view-type ever-view-regexp-list)))
    (next-line)
    (ever-pop-buffer-of-current-note)
    (pop-to-buffer "*ever-notes*")
    (beginning-of-line)))

(defun ever-goto-earliest-note ()
  "Go to the earliest-note note."
  (interactive)
  (goto-char (point-max))
  (when (re-search-backward (cdr (assq ever-view-type ever-view-regexp-list)))
    (ever-pop-buffer-of-current-note)
    (pop-to-buffer "*ever-notes*")
    (beginning-of-line)))

(defun ever-update ()
  (interactive)
  (ever-render-view))

(defun ever-quit ()
  "Close all notes and quit."
  (interactive)
  (kill-buffer "*ever-notes*")  
  (dolist (note (ever-get-note-list))
    (when (get-buffer (ever-note-filename note))
      (kill-buffer (ever-note-filename note))))
  (delete-window (other-window 0))
  (setq ever-delete-mark-list nil))

(defun ever-add-note (title ext)
  "Create a new note."
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

(defun ever-edit-title (title ext)
  "Edit the title of the note."
  (interactive (list (read-string "Title: " (ever-note-title (ever-parse-note))) (read-string "Extension: " (ever-note-ext (ever-parse-note)))))
  (ever-edit-elements (list (cons 'title title) (cons 'ext ext))))

(defun ever-edit-category (category)
  "Edit the category of the note."
  (interactive (list (read-string "Category: " (join (ever-note-category (ever-parse-note)) "/"))))
  (ever-edit-elements (list (cons 'category (remove "" (split-string category "/"))))))

(defun ever-edit-tags (tags)
  "Edit the tags of the note."
  (interactive (list (read-string "Tags: " (join (ever-note-tags (ever-parse-note)) " "))))
  (ever-edit-elements (list (cons 'tags (sort (remove "" (split-string tags " ")) 'string<)))))

(defun ever-mark-delete ()
  "Mark the note to delete."
  (interactive)
  (with-ever-notes
   (let ((note (ever-parse-note)))
     (unless (string-equal "D" (ever-note-mark note))
       (add-to-list 'ever-delete-mark-list (ever-note-path note))
       (delete-char 1) (insert "D"))
     (beginning-of-line))))

(defun ever-unmark-delete ()
  "Unmark the note to delete."
  (interactive)
  (with-ever-notes
   (let ((note (ever-parse-note)))
     (when (string-equal "D" (ever-note-mark note))
       (setq ever-delete-mark-list (remove (ever-note-path note) ever-delete-mark-list))
       (delete-char 1) (insert " "))
     (beginning-of-line))))

(defun ever-mark-execute ()
  "Delete all the marked notes."
  (interactive)
  (dolist (file ever-delete-mark-list)
    (delete-file file))
  (setq ever-delete-mark-list nil)
  (ever-render-view)
  (goto-line 3))

(defun ever-search-notes (regexp)
  "Grep the contents of the notes."
  (interactive "sSearch: ")
  (rgrep regexp "*" ever-root-directroy nil))


;; non interactive
(defun ever-notes-init-state ()
  (setq ever-delete-mark-list nil)
  (setq grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} /dev/null \\;") ;; patch
  )

(defun ever-render-view ()
  (with-ever-notes
   (erase-buffer)
   (insert (concat
	    "\n"
	    (funcall ever-view-type)
	    "\n\n"
	    " [n]: next note [p]: previous note [a]: add note\n\n"
	    " [M-<]: latest note [M->]: earliest note\n\n"
	    " [r]: edit title [c]: edit category [t]: edit tags\n\n"
	    " [d]: mark to delete [u]: unmark to delete [x]: execute deletion\n\n"
	    " [s]: search contents [q]: quit\n"
	    ))))

(defun ever-pop-buffer-of-current-note ()
  (let ((note (ever-parse-note)))
    (with-current-buffer (pop-to-buffer nil)
	(condition-case err
	  (find-file (ever-note-path note))
	(error (insert "Warning: File not found."))))))

(defun ever-view-type-table ()
  (let ((notes (ever-sort-note-list (ever-get-note-list))))
    (let* ((table (mapcar (lambda (note) (list
					  (ever-note-updated note)
					  (ever-note-ext note)
					  (join (cons (join (remove "" (list (ever-note-reldir note) (ever-note-title note))) "/")
						      (if (ever-note-tags note) (mapcar 'ever-add-face-to-tag (ever-note-tags note)))) " "))) notes))
	   (column-width (ever-calc-column-width table)))
      (setq table (cons '("Updated-at" "Ext" "Title") table))
      (let ((indented-table (mapcar (lambda (list)
				      (mapcar (lambda (cell)
						(ever-recenter-string (car cell) (cdr cell)))
					      (zip list column-width)))
				    table)))
	(ever-add-face-to-table (mapconcat (lambda (ls) (join ls "|")) indented-table "\n"))))))

(defun ever-get-note-list ()
  (let ((paths (ever-get-all-path-of-note)))
    (mapcar (lambda (path)
	      (let ((ls (split-string (file-name-nondirectory (file-name-sans-extension path)) "_")))
		(list
		 (cons 'category (remove "" (split-string (substring (file-name-directory path) (length ever-root-directroy)) "/")))
		 (cons 'title (car ls))
		 (cons 'tags (cdr ls))
		 (cons 'ext (file-name-extension path))
		 (cons 'updated (format-time-string "%Y-%m-%d" (nth 5 (file-attributes path))))
		 (cons 'created nil)
		 (cons 'path path))
		))
	    paths)))

(defun ever-sort-note-list (notes)
  (sort notes (lambda (n1 n2)
		(not (string< (ever-note-updated n1) (ever-note-updated n2))))))

(defun ever-get-note-filter (list)
  (filter (lambda (s) (not (string-match "^[\\.#].*" (file-name-nondirectory s)))) list))

(defun ever-get-all-path-of-note ()
  (ever-get-all-path-of-note-iter (list ever-root-directroy) nil))

(defun ever-get-all-path-of-note-iter (paths result)
  (if paths
      (let ((path (car paths)) (rest_paths (cdr paths)))
	(if (car (file-attributes path))
	    (ever-get-all-path-of-note-iter (append (ever-get-note-filter (directory-files path t)) rest_paths) result)
	  (ever-get-all-path-of-note-iter rest_paths (cons path result))))
    result))

(defun ever-calc-column-width (tables)
  (mapcar (lambda (list) (apply 'max (mapcar 'calc-string-width list))) (transpose tables)))

(defun ever-add-face-to-table (table-str)
  (let ((list (split-string table-str "\n")))
    (join (cons (propertize (concat (car list) "  ") 'face 'underline) (cdr list)) "\n")))

(defun ever-add-face-to-tag (tag)
  (propertize tag 'font-lock-face
	      (list '(:box "gray")
		    '(:foreground "gray")
		    '(:background "black"))))

(defun ever-exist-next-note ()
  (case ever-view-type
    ('ever-view-type-table
     (next-line)
     (let ((result (ever-parse-note)))
       (previous-line)
       result))
    ('ever-view-type-summary
     (error "unimplemented view-type."))
    (otherwise
     (error "unexpected value ever-view-type"))))

(defun ever-exist-previous-note ()
  (case ever-view-type
    ('ever-view-type-table
	 (previous-line)
	 (let ((result (ever-parse-note)))
	   (next-line)
	   result))
    ('ever-view-type-summary
     (error "unimplemented view-type."))
    (otherwise
     (error "unexpected value ever-view-type"))))

(defun ever-parse-note ()
  (case ever-view-type
    ('ever-view-type-table
     (let ((str (thing-at-point 'line)))
       (if (or (not (string-match (cdr (assq 'ever-view-type-table ever-view-regexp-list)) str)) (string-match " Ext " str))
	   nil
	 (let ((mark (match-string 1 str)) (updated (match-string 2 str)) (ext (match-string 3 str)) (rel_path_and_tags (remove "" (split-string (match-string 4 str) " "))))
	   (list (cons 'category (butlast   (remove "" (split-string (car rel_path_and_tags) "/"))))
		 (cons 'title    (car (last (remove "" (split-string (car rel_path_and_tags) "/")))))
		 (cons 'tags     (cdr rel_path_and_tags))
		 (cons 'updated  updated)
		 (cons 'ext      ext)
		 (cons 'created  nil)
		 (cons 'path     (format "%s.%s" (car (last (remove "" (split-string (car rel_path_and_tags) "/")))) ext))
		 (cons 'mark     mark)
		 )))))
    ('ever-view-type-summary
     nil)))

(defun ever-edit-elements (elements)
  (let ((note (ever-parse-note)) (line (line-number-at-pos (point))))
    (when note
      (if (get-buffer  (ever-note-filename note))
	  (kill-buffer (ever-note-filename note)))
      (make-directory (ever-note-dir (ever-update-note elements note)) t)
      (rename-file (ever-note-path note)
		   (ever-note-path (ever-update-note elements note)))
      (if (and (eq 0 (length (ever-get-note-filter (directory-files (ever-note-dir note) t))))
	       (not (eq ever-root-directroy (ever-note-dir note)))
	       (string-equal ever-root-directroy (substring (ever-note-dir note) 0 (length ever-root-directroy))))
	  (delete-directory (ever-note-dir note)))
      (ever-render-view)
      (goto-line line)
      )))

(defmacro with-ever-notes (&rest body)
  `(with-current-buffer "*ever-notes*"
     (if (not buffer-read-only)
	 (progn ,@body)
       (setq buffer-read-only nil)
       (progn ,@body)
       (setq buffer-read-only t))))

(provide 'ever-mode)


;; user settings
(setq ever-root-directroy "/Users/Altech/Notes/")

;; ;; start ever-mode
;; (ever-notes)


;; [TODO]
;; - bind M-< and M->
;; - add create-date
;; - change update-date
;; - sort create-date / update-date
;; - abstract mode
;; - search by title, tag, category
;; - spotlight search
;; - improve search result 
;; - improve face
;; - normalize root directroy var at init
;; - Documentation
