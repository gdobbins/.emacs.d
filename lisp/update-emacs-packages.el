;; -*- lexical-binding: t; -*-
(unless user-init-file
  (push "--no-server" command-line-args)
  (push "--no-theme" command-line-args)
  (load "~/.emacs.d/init"))

(defalias 'y-or-n-p (lambda (&rest _) t))

(let ((count 0))
  (cl-labels
      ((force-update ()
	 (condition-case nil
	     (progn
	       (package-menu-execute))
	   (user-error
	    (when (< (incf count) 20)
	      (sleep-for 1)
	      (force-update))))))
    (package-list-packages)
    (package-menu-mark-upgrades)
    (force-update)))
