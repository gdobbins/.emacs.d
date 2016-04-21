;; -*- lexical-binding: t; -*-
(require 'edit-server)
(edit-server-start)

(defun markdown-codify-region (x)
  (interactive "p")
  (if (or (use-region-p) (not transient-mark-mode))
      (save-excursion
	(let ((p (point))
	      (m (mark)))
	  (if (< p m)
	      (untabify p m)
	    (untabify m p)
	    (exchange-point-and-mark)))
	(let ((p (progn
		   (beginning-of-line)
		   (point)))
	      (m (progn
		   (goto-char (mark))
		   (beginning-of-line)
		   (point)))
	      (pre (cond
		    ((= x 1) "    ")
		    ((= x 4) ">"))))
	  (string-rectangle p m pre)
	  (goto-char p)
	  (when (and (= p (point-min))
		     (not (looking-at "[ \t]*$")))
	    (newline))
	  (unless (looking-at "[ \t]*$")
	    (forward-line -1)
	    (unless (looking-at "[ \t]*$")
	      (forward-line 1)
	      (newline)))
	  (let ((m (mark)))
	    (goto-char m)
	    (forward-line)
	    (when (eql (point) m)
	      (move-end-of-line 1)
	      (newline)))))
    (save-excursion
      (cond
       ((= x 1) (mark-defun))
       ((= x 4) (mark-paragraph)))
      (markdown-codify-region x))))

(defun markdown-inline-codify-region ()
  "If region is active, surround in backquotes. Otherwise call self-insert-command."
  (interactive)
  (if (use-region-p)
      (progn
	(when (< (point) (mark))
	  (exchange-point-and-mark))
	(save-excursion
	  (insert "`")
	  (goto-char (mark))
	  (insert "`"))
	(forward-char 1))
    (call-interactively #'self-insert-command)))

(define-key edit-server-edit-mode-map (kbd "C-x c") #'edit-server-done)
(define-key edit-server-edit-mode-map (kbd "C-c C-k") #'edit-server-abort)
(define-key edit-server-edit-mode-map (kbd "C-c SPC") #'markdown-codify-region)
(define-key edit-server-edit-mode-map (kbd "`") #'markdown-inline-codify-region)

(add-to-list 'edit-server-url-major-mode-alist '("/r/\\(lisp\\|common_lisp\\)" . lisp-mode))
(add-to-list 'edit-server-url-major-mode-alist '("/r/emacs" . emacs-lisp-mode))

(add-hook 'edit-server-start-hook (lambda () (setq indent-tabs-mode nil)))
