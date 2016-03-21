;; -*- lexical-binding: t; -*-
(require 'edit-server)
(edit-server-start)

(defun markdown-codify-region (x)
  (interactive "p")
  (if mark-active
      (save-excursion
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
	  (if (< p m)
	      (progn
		(string-rectangle p m pre)
		(goto-char p)
		(when (and (= p (point-min))
			   (not (looking-at "[ \t]*$")))
		  (newline)))
	    (progn
	      (string-rectangle m p pre)
	      (goto-char m)
	      (when (and (= m (point-min))
			 (not (looking-at "[ \t]*$")))
		(newline))))
	  (unless (looking-at "[ \t]*$")
	    (forward-line -1)
	    (unless (looking-at "[ \t]*$")
	      (forward-line 1)
	      (newline)))))
    (progn
      (save-excursion
	(cond
	 ((= x 1) (mark-defun))
	 ((= x 4) (mark-paragraph)))
	(markdown-codify-region x)))))

(define-key edit-server-edit-mode-map (kbd "C-x c") #'edit-server-done)
(define-key edit-server-edit-mode-map (kbd "C-c C-k") #'edit-server-abort)
(define-key edit-server-edit-mode-map (kbd "C-c SPC") #'markdown-codify-region)

(add-to-list 'edit-server-url-major-mode-alist '("/r/\\(lisp\\|common_lisp\\)" . lisp-mode))
(add-to-list 'edit-server-url-major-mode-alist '("/r/emacs" . emacs-lisp-mode))

(add-hook 'edit-server-start-hook (lambda () (setq indent-tabs-mode nil)))


