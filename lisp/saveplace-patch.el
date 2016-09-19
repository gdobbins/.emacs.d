;; This alters saveplace such that it works with pdf-tools

(require 'saveplace)

(cond
 ((eval-when-compile (= emacs-major-version 25))
  (defun save-place-to-alist ()
    (or save-place-loaded (load-save-place-alist-from-file))
    (let* ((directory (and (derived-mode-p 'dired-mode)
			   (boundp 'dired-subdir-alist)
			   dired-subdir-alist
			   (dired-current-directory)))
	   (item (or buffer-file-name
		     (and directory
			  (expand-file-name (if (consp directory)
						(car directory)
					      directory))))))
      (when (and item
		 (or (not save-place-ignore-files-regexp)
		     (not (string-match save-place-ignore-files-regexp
					item))))
	(let ((cell (assoc item save-place-alist))
	      (position (cond ((eq major-mode 'hexl-mode)
			       (with-no-warnings
				 (1+ (hexl-current-address))))
			      ((and (derived-mode-p 'dired-mode) directory)
			       (let ((filename (dired-get-filename nil t)))
				 (if filename
				     `((dired-filename . ,filename))
				   (point))))
			      ((eq major-mode 'pdf-view-mode)
			       (pdf-view-current-page))
			      (t (point)))))
	  (if cell
	      (setq save-place-alist (delq cell save-place-alist)))
	  (if (and save-place
		   (not (and (integerp position)
			     (= position 1))))
	      (setq save-place-alist
		    (cons (cons item position)
			  save-place-alist))))))))

 ((eval-when-compile (< emacs-major-version 24))
  (defun save-place-to-alist ()
    (or save-place-loaded (load-save-place-alist-from-file))
    (let ((item (or buffer-file-name
		    (and (derived-mode-p 'dired-mode)
			 dired-directory
			 (expand-file-name (if (consp dired-directory)
					       (car dired-directory)
					     dired-directory))))))
      (when (and item
		 (or (not save-place-ignore-files-regexp)
		     (not (string-match save-place-ignore-files-regexp
					item))))
	(let ((cell (assoc item save-place-alist))
	      (position (cond ((eq major-mode 'hexl-mode)
			       (with-no-warnings
				 (1+ (hexl-current-address))))
			      ((and (derived-mode-p 'dired-mode)
				    dired-directory)
			       (let ((filename (dired-get-filename nil t)))
				 (if filename
				     `((dired-filename . ,filename))
				   (point))))
			      ((eq major-mode 'pdf-view-mode)
			       (pdf-view-current-page))
			      (t (point)))))
	  (if cell
	      (setq save-place-alist (delq cell save-place-alist)))
	  (if (and save-place
		   (not (and (integerp position)
			     (= position 1))))
	      (setq save-place-alist
		    (cons (cons item position)
			  save-place-alist)))))))))

(defun save-place-find-file-hook ()
  (or save-place-loaded (load-save-place-alist-from-file))
  (let ((cell (assoc buffer-file-name save-place-alist)))
    (if cell
	(progn
	  (or revert-buffer-in-progress-p
	      (and (integerp (cdr cell))
		   (if (eq major-mode 'pdf-view-mode)
		       (pdf-view-goto-page (cdr cell))
		     (goto-char (cdr cell)))))
          (setq save-place t)))))
