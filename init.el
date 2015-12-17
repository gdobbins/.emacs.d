;; -*- lexical-binding: t; -*-
(setq inhibit-startup-screen t)
(when (fboundp #'menu-bar-mode)
  (menu-bar-mode -1))
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq gc-cons-threshold 100000000)

(load "~/.emacs.d/secrets" t t)

(defvar hidden-minor-modes
  '(undo-tree-mode
    guide-key-mode
    paredit-mode
    elisp-slime-nav-mode
    eldoc-mode))

(defun purge-minor-modes ()
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
	(setcar trg "")))))

(add-hook 'after-change-major-mode-hook 'purge-minor-modes)

(defun command-line-diff (switch)
  (ignore switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

(defun command-line-magit (switch)
  (ignore switch)
  (let ((git-dir (pop command-line-args-left)))
    (magit-status-internal (expand-file-name git-dir))
    (delete-other-windows)))

(add-to-list 'command-switch-alist '("-magit" . command-line-magit))

(cl-defmacro make-last-key-repeating-function (func &optional trans-map (keep-map t))
  "Add advice to to the function such that repeating the
last key used to call the function repeats the call.
If trans-map, use trans-map as the transient map such that
multiple functions can call each other in repetition."
  (let ((new-func (intern (concat "last-key-repeating-" (symbol-name func)))))
    `(defadvice ,func (after ,new-func compile activate)
       ,@(if trans-map
	     `(,(format "Set the transient map to `%s'" trans-map)
	       (set-transient-map ,trans-map ,keep-map))
	   `("Set the transient map to one where `last-input-event' repeats the function call"
	     (set-transient-map
	      (let ((map (make-sparse-keymap)))
		(define-key map (vector last-input-event) ',func)
		map)))))))

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

(setq kmacro-ring-max 24)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring
	ioccur-history
	kmacro-ring
	last-kbd-macro
	kmacro-counter
	kmacro-counter-format
	register-alist))

(defmacro make-keyboard-macro (string)
  "Make a function which emulates pushing the key sequence string"
  (let ((new-func (intern (concat "push-" string))))
    `(defun ,new-func ()
       ,(concat "Emulate the keypress " string)
       (interactive)
       (kmacro-call-macro nil t nil (kbd ,string)))))

(setq sentence-end-double-space nil)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-c" "C-x 4" "C-x 8" "C-x a" "C-c k" "C-c p" "C-c m" "C-c w" "C-h"))
(guide-key-mode 1)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setf browse-url-browser-function '(("." . browse-url-default-browser)))

(autoload 'w3m-browse-url "w3m" "Ask a www browser to show a URL." t)

(defvar *w3m-number-of-windows-beforehand* 1 "The number of windows open before opening w3m.")

(defvar w3m-pop-up-windows)

(defun w3m-browse-url-other-window (url &optional newwin)
  "Set `*w3m-number-of-windows-beforehand*' to the current number of windows, then call `w3m-browse-url' in the `other-window', using `split-window' if necessary."
  (setq *w3m-number-of-windows-beforehand* (length (window-list)))
  (let ((w3m-pop-up-windows t))
    (if (one-window-p)
	(split-window nil nil t))
    (other-window 1)
    (w3m-browse-url url newwin)))

(defadvice w3m-close-window (after w3m-close-duplicate-buffer compile)
  "Delete the w3m window if the current number of windows is less than `*w3m-number-of-windows-beforehand*'."
  (unless (>= *w3m-number-of-windows-beforehand* (length (window-list)))    (delete-window)))

(add-to-list 'browse-url-browser-function '("^file" . w3m-browse-url-other-window))

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(global-set-key (kbd "C-x C-d") 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)
(global-set-key (kbd "C-x 4 C-d") 'dired-jump-other-window)

(eval-after-load 'dired
  '(progn
     (require 'dired-x)
     (setq-default dired-omit-files-p t)
     (setq dired-listing-switches "-alh")
     (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
     (define-key dired-mode-map (kbd "h") 'dired-omit-mode)
     (define-key dired-mode-map (kbd "e") 'read-only-mode)))

(eval-after-load 'wdired
  '(progn
     (defvar wdired-mode-map)
     (define-key wdired-mode-map (kbd "C-c C-g") 'wdired-abort-changes)
     (defvar wdired-allow-to-change-permissions)
     (setq wdired-allow-to-change-permissions t)))

(global-set-key (kbd "C-x p") 'proced)
(setq-default proced-filter 'emacs)

(setq ibuffer-saved-filter-groups
      `(("default"
	 ("Lisp" (or
		  (mode . lisp-mode)
		  (mode . slime-repl-mode)
		  (mode . slime-inspector-mode)
		  (filename . "^/usr/local/doc/HyperSpec/")))
	 ("Python" (or
		    (mode . python-mode)
		    (mode . inferior-python-mode)
		    (name . "^\\*Python \\(Check\\|Doc\\)\\*$")))
	 ("Shell" (or
		   (mode . shell-mode)
		   (mode . term-mode)
		   (mode . sh-mode)
		   (name . "^\\*Shell Command Output\\*$")))
	 ("Assembly" (or
		      (mode . asm-mode)
		      (filename . "^/usr/local/doc/64-ia-32-architectures-software-developer-manual-325462.pdf$")))
	 ("Data" (or
		  (filename . ".*\\.\\([ct]sv\\|dat\\)$")))
	 ("Text" (mode . text-mode))
	 ("Books" (or
		   (mode . pdf-view-mode)
		   (filename . ,(concat "^" (expand-file-name "~/Books")))))
	 ("Web" (mode . w3m-mode))
	 ("Org" (or (derived-mode . org-mode)
		    (mode . org-agenda-mode)))
	 ("Dired" (or
		   (mode . dired-mode)
		   (mode . wdired-mode)
		   (mode . archive-mode)
		   (mode . proced-mode)))
	 ("Git" (derived-mode . magit-mode))
	 ("Diff" (or
		  (mode . ediff-mode)
		  (name . "^\\*[Ee]?[Dd]iff.*\\*$")))
	 ("Emacs" (or
		   (mode . emacs-lisp-mode)
		   (mode . lisp-interaction-mode)
		   (mode . help-mode)
		   (mode . Info-mode)
		   (mode . package-menu-mode)
		   (mode . Custom-mode)
		   (mode . apropos-mode)
		   (mode . ioccur-mode)
		   (mode . occur-mode)
		   (mode . reb-mode)
		   (mode . messages-buffer-mode))))))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-jump-offer-only-visible-buffers t)

(defun ibuffer-mark-and-kill-lines ()
  "Mark the current line, then kill all marked lines."
  (interactive)
  (ibuffer-mark-forward 1)
  (ibuffer-do-kill-lines))

(eval-after-load "ibuffer"
  '(progn
     (define-key ibuffer-mode-map (kbd "H") nil)
     (define-key ibuffer-mode-map (kbd "k") 'ibuffer-do-delete)
     (define-key ibuffer-mode-map (kbd "C-k") 'ibuffer-mark-and-kill-lines)
     (define-key ibuffer-mode-map (kbd "M-o") nil)
     (defadvice ibuffer (around ibuffer-point-to-most-recent compile activate) ()
		"Open ibuffer with cursor pointed to most recent buffer name"
		(let ((recent-buffer-name (buffer-name)))
		  ad-do-it
		  (ibuffer-jump-to-buffer recent-buffer-name)))))

(eval-after-load "ibuf-ext"
  '(progn
     (add-to-list 'ibuffer-never-show-predicates "^\\*slime-events\\*$")
     (add-to-list 'ibuffer-never-show-predicates "^\\*inferior-lisp\\*$")
     (add-to-list 'ibuffer-never-show-predicates "^\\*Compile-Log\\*$")
     (add-to-list 'ibuffer-never-show-predicates "^\\*Completions\\*$")))

(defvar hidden-ibuffer-groups '("^\\[ Default \\]"))

(defun close-hidden-ibuffer-groups ()
  (interactive)
  (let ((start-point (point)))
    (dolist (group hidden-ibuffer-groups)
      (goto-char 1)
      (when (search-forward-regexp group nil t)
	(move-beginning-of-line 1)
	(ibuffer-toggle-filter-group)))
    (goto-char start-point)))

(add-hook 'ibuffer-mode-hook #'close-hidden-ibuffer-groups)

(add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

(eval-after-load "re-builder"
  '(progn
     (defvar reb-mode-map)
     (define-key reb-mode-map (kbd "C-c C-g") 'reb-quit)
     (defvar reb-re-syntax)
     (setq reb-re-syntax 'string)))

(eval-after-load "isearch"
  '(progn
     (require 'isearch+)
     (defvar isearch-mode-map)
     (define-key isearch-mode-map [remap ace-window] 'isearchp-open-recursive-edit)))

(load (expand-file-name "~/quicklisp/slime-helper"))

(defun port-file->swank-start-server (port-file _)
  (format "(swank:start-server %S)\n" port-file))

(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl" "--core" "/home/graham/lisp/sbcl.core-graham" "--dynamic-space-size" "3096")
	      :init port-file->swank-start-server)
	(sbcl-big ("/usr/bin/sbcl" "--core" "/home/graham/lisp/sbcl.core-graham" "--dynamic-space-size" "6192")
	      :init port-file->swank-start-server)
	(sbcl-debug ("/usr/bin/sbcl" "--dynamic-space-size" "3096" "--load" "/home/graham/quicklisp/setup.lisp"))
	(sbcl-raw ("/usr/bin/sbcl"))
	(sbcl-git ("/home/graham/sbcl/run-sbcl.sh" "--dynamic-space-size" "3096" "--load" "/home/graham/quicklisp/setup.lisp"))
	(ecl ("/usr/bin/ecl"))
	(clisp ("/usr/bin/clisp"))))

(when (file-exists-p "/usr/local/doc/HyperSpec/") (setq common-lisp-hyperspec-root "file://usr/local/doc/HyperSpec/"))

(setq
 backup-by-copying t			; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/saves"))		; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)			; use versioned backups

(show-paren-mode 1)
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(defun truncate-lines->t ()
  (interactive)
  (unless truncate-lines
    (setq truncate-lines t)))

(add-hook 'dired-mode-hook #'truncate-lines->t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(require 'saveplace)
(setq-default save-place t)
					;(setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
					;(require 'ess-site)

					;  (require 'discover)
					;  (global-discover-mode 1)

(require 'undo-tree)
(global-undo-tree-mode)
(eval-after-load 'undo-tree
  '(define-key undo-tree-visualizer-mode-map (kbd "RET") 'undo-tree-visualizer-quit))

(eval-after-load "x86-lookup"
  '(let ((location "/usr/local/doc/64-ia-32-architectures-software-developer-manual-325462.pdf"))
     (when (file-exists-p location)
       (defvar x86-lookup-pdf)
       (setq x86-lookup-pdf location))))

(defun eval-and-replace (x)
  "Replace the preceding sexp with its value."
  (interactive "p")
  (let ((y x))
    (if (> y 0)
	(let ((s (if (looking-back " " 1) 0 -1)))
	  (while (> y 0)
	    (setf s (+ s (if (= 0 (skip-chars-backward " \n\t")) 1 0)))
	    (kill-sexp -1)
	    (condition-case nil
		(prin1 (eval (read (current-kill 0)))
		       (current-buffer))
	      (error (message "Invalid expression")
		     (insert (current-kill 0))))
	    (forward-sexp -1)
	    (setq y (1- y)))
	  (forward-sexp (- x s)))
      (if (< y 0)
	  (let ((pointer-position (point)))
	    (while (< y 0)
	      (kill-sexp 1)
	      (condition-case nil
		  (prin1 (eval (read (current-kill 0)))
			 (current-buffer))
		(error (message "Invalid expression")
		       (insert (current-kill 0))))
	      (skip-chars-forward " \n\t")
	      (setq y (1+ y)))
	    (goto-char pointer-position))
	(error (message "Okay?"))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
	 (message "You can't rotate a single window!"))
	(t
	 (let ((i 1) (numWindows (count-windows)))
	   (while  (< i numWindows)
	     (let* ((w1 (elt (window-list) i))
		    (w2 (elt (window-list) (+ (% i numWindows) 1)))
		    (b1 (window-buffer w1))
		    (b2 (window-buffer w2))
		    (s1 (window-start w1))
		    (s2 (window-start w2)))
	       (set-window-buffer w1  b2)
	       (set-window-buffer w2 b1)
	       (set-window-start w1 s2)
	       (set-window-start w2 s1)
	       (setq i (1+ i))))))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun count-buffer-lines ()
  "Count the number of lines in the active region or buffer."
  (interactive)
  (princ
   (if mark-active
       (format "%d lines in region of %s" (let ((pt (point))
						(mk (mark)))
					    (if (> pt mk)
						(count-lines mk pt)
					      (count-lines pt mk)))
	       (buffer-name (current-buffer)))
     (format "%d lines in %s" (count-lines 1 (point-max)) (buffer-name (current-buffer))))))

(defun activate-word-column-region ()
  "Look at the symbol at point, search backward and place the point before a
symbol, and search forward and place the mark after a symbol such that all
lines have identical symbols at identical goal columns as the symbol at point."
  (interactive)
  (let (upper-pt lower-pt (next-line-add-newlines t))
    (save-excursion
      (let ((target (format "%s" (symbol-at-point))))
	(while (looking-back "\\(\\sw\\|\\s_\\)" 1)
	  (backward-char 1))
	(with-no-warnings
	  (save-excursion
	    (next-line 1)
	    (while (looking-at target)
	      (setf lower-pt (point))
	      (next-line 1)))
	  (save-excursion
	    (next-line -1)
	    (while (looking-at target)
	      (setf upper-pt (point))
	      (next-line -1))))))
    (when (or upper-pt lower-pt)
      (let ((upper-pt (or upper-pt (point)))
	    (lower-pt (or lower-pt (point))))
	(goto-char lower-pt)
	(while (looking-at "\\(\\sw\\|\\s_\\)")
	  (forward-char 1))
	(push-mark nil nil t)
	(goto-char upper-pt)
	(while (looking-back "\\(\\sw\\|\\s_\\)" 1)
	  (backward-char 1)))))
  (rectangle-mark-mode))

(defun insert-backquote ()
  (interactive)
  (insert "`"))

(defun insert-twidle ()
  (interactive)
  (insert "~"))

(defun insert-splice ()
  (interactive)
  (if (looking-back "[^ \n]" 1)
      (insert " "))
  (insert ",@()")
  (backward-char))

(autoload 'sh-show-shell "sh-script" "Pop the shell interaction buffer." t)
(autoload 'elpy-shell-switch-to-shell "elpy" "Switch to inferior Python process buffer.")

(defun smart-switch-to-output-buffer ()
  (interactive)
  (cond
   ((get-process "inferior-lisp") (slime-switch-to-output-buffer))
   ((get-process "shell") (sh-show-shell))
   ((get-process "Python") (elpy-shell-switch-to-shell))
   (t (princ "No available process to switch to."))))

(autoload 'magit-ido-completing-read "magit-utils" "Ido-based `completing-read' almost-replacement.")
(eval-after-load "magit"
  '(progn
     (defvar magit-completing-read-function)
     (setq magit-completing-read-function #'magit-ido-completing-read)))
(setq ioccur-buffer-completion-use-ido t)

(defun ioccur-follow-next ()
       "Move to the next line and follow in the connected buffer."
       (interactive)
       (ioccur-next-line)
       (ioccur-jump-without-quit))

(defun ioccur-follow-previous ()
  "Move to the previous line and follow in the connected buffer."
  (interactive)
  (ioccur-precedent-line)
  (ioccur-jump-without-quit))

(eval-after-load "ioccur"
  '(progn
     (defvar ioccur-mode-map)
     (define-key ioccur-mode-map (kbd "M-n") #'ioccur-follow-next)
     (define-key ioccur-mode-map (kbd "M-p") #'ioccur-follow-previous)
     (define-key ioccur-mode-map (kbd "n") #'ioccur-next-line)
     (define-key ioccur-mode-map (kbd "p") #'ioccur-precedent-line)
     (define-key ioccur-mode-map (kbd "z") #'ioccur-jump-without-quit)
     (define-key ioccur-mode-map (kbd "r") #'ioccur-restart)
     (define-key ioccur-mode-map (kbd "s") #'ioccur-restart)
     (define-key ioccur-mode-map (kbd "o") #'ioccur-restart)))

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

(defun really-desktop-clear ()
  (interactive)
  (when (yes-or-no-p "Really clear the desktop? ")
    (desktop-clear)))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (when (yes-or-no-p "Really kill all other buffers? ")
    (dolist (buffer (buffer-list))
      (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
	(kill-buffer buffer)))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(with-no-warnings
 (defun duplicate-other-window-buffer ()
   (interactive)
   (let ((this-buffer (current-buffer))
	 (len (length (window-list))))
     (cond
      ((= len 2)
       (other-window 1)
       (switch-to-buffer this-buffer t t)
       (other-window 1))
      ((= len 1)
       (split-window-right))
      ((error "Too many windows"))))))

(defmacro defun-other-window-do (name if-2-list &rest else)
  `(defun ,name ()
     (interactive)
     (if (= 2 (length (window-list)))
	 (progn
	   (other-window 1)
	   ,@if-2-list
	   (other-window 1))
       ,@(if else
	    else
	   '((error "Need two windows"))))))

(defun find-user-init-file ()
  "Edit the `user-init-file'"
  (interactive)
  (find-file user-init-file))

(defun-other-window-do other-window-imenu
  ((call-interactively #'imenu)))

(defun-other-window-do other-window-isearch-forward
  ((let ((isearch-mode-end-hook (append isearch-mode-end-hook '(exit-recursive-edit))))
     (call-interactively #'isearch-forward)
     (recursive-edit))))

(defun-other-window-do other-window-isearch-backward
  ((let ((isearch-mode-end-hook (append isearch-mode-end-hook '(exit-recursive-edit))))
     (call-interactively #'isearch-backward)
     (recursive-edit))))

(setq aw-keys (list ?h ?t ?n ?s ?a ?o ?e ?u))
(setq avy-keys aw-keys)

(define-prefix-command 'pop-repeating-map)
(define-key pop-repeating-map (kbd "j") 'jump-to-register)
(define-key pop-repeating-map (kbd "r") 'jump-to-register)
(define-key pop-repeating-map (kbd "s") 'imenu)
(define-key pop-repeating-map (kbd ".") (make-keyboard-macro "M-."))
(define-key pop-repeating-map (kbd ",") (make-keyboard-macro "M-,"))
(make-last-key-repeating-function push-M-\. pop-repeating-map)
(make-last-key-repeating-function push-M-\, pop-repeating-map)
(global-set-key (kbd "C-.") 'push-M-\.)
(global-set-key (kbd "C-,") 'push-M-\,)

(define-prefix-command 'my-other-window-map)
(make-last-key-repeating-function duplicate-other-window-buffer my-other-window-map nil)
(define-key my-other-window-map (kbd "C-c s") 'other-window-imenu)
(define-key my-other-window-map (kbd "C-s") 'other-window-isearch-forward)
(define-key my-other-window-map (kbd "C-r") 'other-window-isearch-backward)
(define-key my-other-window-map (kbd "C-v") 'scroll-other-window)
(define-key my-other-window-map (kbd "C-M-v") 'scroll-other-window)
(define-key my-other-window-map (kbd "M-v") 'scroll-other-window-down)

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c o") 'ioccur)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c w r") 'rotate-windows)
(global-set-key (kbd "C-c w t") 'toggle-window-split)
(global-set-key (kbd "C-c w C-k") 'really-desktop-clear)
(global-set-key (kbd "C-c w k") 'kill-other-buffers)
(global-set-key (kbd "C-c w u") 'revert-this-buffer)
(global-set-key (kbd "C-c w l") 'count-buffer-lines)
(global-set-key (kbd "C-c w f") 'rename-current-buffer-file)
(global-set-key (kbd "C-c w d") 'duplicate-other-window-buffer)
(global-set-key (kbd "C-c w c") 'emacs-uptime)
(global-set-key (kbd "C-c w i") 'find-user-init-file)
(global-set-key (kbd "C-c w e") 'erase-buffer)
(global-set-key (kbd "C-c w s") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c w n") 'column-number-mode)
(global-set-key (kbd "C-c w m") 'how-many)
(global-set-key (kbd "C-c d") 'duplicate-other-window-buffer)
(global-set-key (kbd "C-c C-z") 'smart-switch-to-output-buffer)
(global-set-key (kbd "C-c z") 'smart-switch-to-output-buffer)
(global-set-key (kbd "C-c p") 'pop-repeating-map)
(global-set-key (kbd "C-c p SPC") 'pop-to-mark-command)
(global-set-key (kbd "C-c p m") 'pop-to-mark-command)
(global-set-key (kbd "C-c p l") 'pop-to-mark-command)
(make-last-key-repeating-function pop-to-mark-command pop-repeating-map)
(global-set-key (kbd "C-c p g") 'pop-global-mark)
(global-set-key (kbd "C-c p C-SPC") 'pop-global-mark)
(make-last-key-repeating-function pop-global-mark pop-repeating-map)
(global-set-key (kbd "C-c p p") 'goto-last-change)
(make-last-key-repeating-function goto-last-change pop-repeating-map)
(global-set-key (kbd "C-c p n") 'goto-last-change-reverse)
(make-last-key-repeating-function goto-last-change-reverse pop-repeating-map)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-c '") 'insert-backquote)
(global-set-key (kbd "C-c -") 'insert-twidle)
(global-set-key (kbd "C-c ,") 'insert-splice)
(global-set-key (kbd "C-c b") 'previous-buffer)
(global-set-key (kbd "C-c f") 'next-buffer)
(global-set-key (kbd "C-c k") 'kmacro-keymap)
(global-set-key (kbd "C-c s") 'imenu)
(global-set-key (kbd "C-x r S") 'activate-word-column-region)
(global-set-key (kbd "C-h x") 'x86-lookup)
(global-set-key (kbd "C-h r") 're-builder)
(global-set-key (kbd "C-h s") 'string-edit-at-point)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-n") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-p") 'smartscan-symbol-go-backward)
(global-set-key (kbd "M-'") 'avy-goto-word-1)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

(autoload 'mpc-resume "mpc")
(autoload 'mpc-pause "mpc")
(autoload 'mpc-next "mpc")
(autoload 'mpc-prev "mpc")

(define-prefix-command 'mpc-repeating-map)
(global-set-key (kbd "C-c m") 'mpc-repeating-map)
(global-set-key (kbd "C-c m m") 'mpc)
(make-last-key-repeating-function mpc mpc-repeating-map)
(global-set-key (kbd "C-c m r") 'mpc-resume)
(make-last-key-repeating-function mpc-resume mpc-repeating-map)
(global-set-key (kbd "C-c m p") 'mpc-pause)
(make-last-key-repeating-function mpc-pause mpc-repeating-map)
(global-set-key (kbd "C-c m n") 'mpc-next)
(make-last-key-repeating-function mpc-next mpc-repeating-map)
(global-set-key (kbd "C-c m l") 'mpc-prev)
(make-last-key-repeating-function mpc-prev mpc-repeating-map)

(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)

(eval-after-load 'help-mode
  '(progn
     (define-key help-mode-map (kbd "<mouse-8>") 'help-go-back)
     (define-key help-mode-map (kbd "<mouse-9>") 'help-go-forward)
     (define-key help-mode-map (kbd "C-c b") 'help-go-back)
     (define-key help-mode-map (kbd "C-c f") 'help-go-forward)
     (define-key help-mode-map (kbd "n") 'next-line)
     (define-key help-mode-map (kbd "p") 'previous-line)
     (defadvice describe-bindings (after describe-bindings-move-to-major-mode compile activate)
       "Pop to the help buffer and search forward for the Major mode bindings."
       (pop-to-buffer (help-buffer))
       (search-forward "Major Mode Bindings:")
       (recenter-top-bottom 0))))

;(add-hook 'help-mode-hook #'set-help-mode)

(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(eval-after-load "elisp-slime-nav"
  '(define-key elisp-slime-nav-mode-map (kbd "C-c C-d") 'elisp-slime-nav-describe-elisp-thing-at-point))
(add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)

(require 'ido-sort-mtime)
(ido-sort-mtime-mode 1)
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)
(require 'ido-hacks)
(ido-hacks-mode 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'ido-hacks-execute-extended-command)
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
	try-expand-dabbrev
	try-expand-list
	try-expand-line
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-complete-file-name-partially
	try-complete-file-name))

(defadvice he-substitute-string (after he-paredit-fix compile activate)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

(defun pathname->~->home ()
  (interactive)
  (if (looking-back "/" 1)
      (insert "~/")
    (call-interactively 'self-insert-command)))

(defun pathname->/->root ()
  (interactive)
  (if (looking-back "/" 1)
      (insert "//")
    (call-interactively 'self-insert-command)))

(defmacro defset-function (function key-string &optional keymap)
  "Create a function which sets set-function to bind (kbd key-string) to function either locally, or optionally in keymap."
  (let ((function-symbol (intern (concat "set-" (symbol-name function)))))
    `(defun ,function-symbol ()
       ,(if keymap
	    (concat "Set the key " key-string " to `" (symbol-name function) "' in `" (symbol-name keymap) "'")
	  (concat "Set the key " key-string " to `" (symbol-name function) "' locally."))
       (interactive)
       ,(if keymap
	    `(define-key ,keymap (kbd ,key-string) #',function)
	  `(local-set-key (kbd ,key-string) #',function)))))

(defset-function pathname->~->home "~" ido-completion-map)
(defset-function pathname->/->root "/" ido-completion-map)

(add-hook 'ido-setup-hook #'set-pathname->~->home)
(add-hook 'ido-setup-hook #'set-pathname->/->root)

(add-to-list 'ido-ignore-files "^\\.smex-items$")
(add-to-list 'ido-ignore-files "^\\.slime-history\\.eld$")
(add-to-list 'ido-ignore-files "^\\.python_history$")
(add-to-list 'ido-ignore-files "^\\.w3m/$")
(add-to-list 'ido-ignore-files "^\\.histfile$")
(add-to-list 'ido-ignore-files "^ido\\.last$")

(setq ido-file-extensions-order '(".el" ".org" ".lisp" ".py" ".pdf" t))
(setq ido-use-filename-at-point 'guess)

(add-to-list 'default-frame-alist '(font . "Linux Libertine Mono:pixelsize=14:foundry=unknown:weight=normal:slant=normal:width=normal:scalable=true"))
(setq split-width-threshold 100)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(eval-after-load "shell"
  '(progn
     (defvar shell-mode-map)
     (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(defun fix-capitalization->paren ()
  (interactive)
  (let ((case-fold-search nil) (pointer-position (point)))
    (re-search-backward "[\n\t '`,@][A-Z]" nil)
    (replace-match (concat (substring (match-string 0) 0 1) "(" (downcase (substring (match-string 0) 1))) t)
    (move-end-of-line nil)
    (insert ")")
    (goto-char (+ 1 pointer-position))))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-c (") #'fix-capitalization->paren)
     (add-to-list 'paredit-space-for-delimiter-predicates
		  #'paredit-space-for-predicates-cl)))

(defun insert-earmuffs ()
  (interactive)
  (let ((pointer-position (point)))
    (if (looking-back "[^ \n]" 1)
	(if (not (looking-at "\*"))
	    (insert "*"))
      (insert "**"))
    (goto-char (+ 1 pointer-position))))

(defset-function insert-earmuffs "*")

(defun paredit-space-for-predicates-cl (endp delimiter)
  "Do not add a space when \" is preceded by #. reader macro or ( is preceded by ,@"
  (or endp
      (cond ((eq (char-syntax delimiter) ?\")
	     (not (looking-back "#." 2)))
	    ((eq (char-syntax delimiter) ?\()
	     (not (looking-back "[\n\s-],@" 3)))
	    (t t))))

(defun set-cl-help-mode (km)
  (define-key km (kbd "C-h f") 'slime-describe-function)
  (define-key km (kbd "C-h v") 'slime-describe-symbol)
  (define-key km (kbd "C-h d") 'slime-hyperspec-lookup)
  (define-key km (kbd "C-h ~") 'hyperspec-lookup-format)
  (define-key km (kbd "C-h #") 'hyperspec-lookup-reader-macro))

(defun slime-eval-and-replace ()
  "Replace the preceding sexp with its common-lisp value."
  (interactive)
  (if (get-process "inferior-lisp")
      (let ((origin-point (point)))
	(call-interactively #'slime-eval-print-last-expression)
	(let ((destination-distance (- (point) origin-point 1)))
	  (goto-char origin-point)
	  (backward-delete-char -1)
	  (backward-kill-sexp)
	  (forward-char destination-distance)))))

(eval-after-load "lisp-mode"
  '(progn
     (set-cl-help-mode lisp-mode-map)
     (define-key lisp-mode-map (kbd "C-c e") #'slime-eval-and-replace)
     (define-key lisp-mode-map (kbd "*") #'insert-earmuffs)
     (define-key lisp-mode-map (kbd "<tab>") #'slime-indent-and-complete-symbol)))

(defun slime-return-to-lisp-file ()
  (interactive)
  (dolist (buff (buffer-list) (current-buffer))
    (if (equal 'lisp-mode
	       (buffer-local-value 'major-mode buff))
	(progn
	  (pop-to-buffer buff)
	  (return buff)))))

(eval-after-load "slime-repl"
  '(progn
     (set-cl-help-mode slime-repl-mode-map)
     (define-key slime-repl-mode-map (kbd "C-c e") #'slime-eval-and-replace)
     (define-key slime-repl-mode-map (kbd "*") #'insert-earmuffs)
     (define-key slime-repl-mode-map (kbd "<up>") 'slime-repl-previous-input)
     (define-key slime-repl-mode-map (kbd "<down>") 'slime-repl-next-input)
     (define-key slime-repl-mode-map (kbd "<right>") 'forward-word)
     (define-key slime-repl-mode-map (kbd "<left>") 'backward-word)
     (define-key slime-repl-mode-map (kbd "C-c C-z") 'slime-return-to-lisp-file)
     (define-key slime-repl-mode-map (kbd "C-c q") 'slime-quit-lisp)))

(defadvice slime-scratch (after slime-scratch-add-top-line-comment compile activate)
  (unless (/= 1 (point))
    (unless (looking-at ".")
      (insert ";; This buffer is for notes you don't want to save, and for Common Lisp evaluation.\n\n"))))

(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches \"[\]\)}]\" then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at "[\]\)}]")
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defset-function electrify-return-if-match "RET")

;(add-hook 'lisp-mode-hook #'set-electrify-return-if-match)
;(add-hook 'lisp-interaction-mode-hook #'set-electrify-return-if-match)
;(add-hook 'emacs-lisp-mode-hook #'set-electrify-return-if-match)

(pdf-tools-install)
(define-key pdf-view-mode-map (kbd "a") 'image-bol)
(define-key pdf-view-mode-map (kbd "e") 'image-eol)

(defadvice abort-if-file-too-large (around no-abort-if-file-is-pdf compile activate)
  (if (string-match-p "\\.[pP][dD][fF]$" filename)
      nil
    ad-do-it))

(defun set-mode-line-numbers-at-front ()
  "Set mode-line numbers to be in front of the frame and buffer identification."
  (interactive)
  (setq mode-line-format
	'("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote "  " mode-line-position mode-line-frame-identification mode-line-buffer-identification
 (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))

(add-hook 'pdf-view-mode-hook 'set-mode-line-numbers-at-front)

(require 'org)
(add-to-list 'org-modules 'org-habit)
(require 'org-habit)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c i") 'interleave)
(define-key org-mode-map (kbd "M-n") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "M-p") 'outline-previous-visible-heading)

(setq org-agenda-sticky t)

(defun add-delete-trailing-whitespace-before-save-hook ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(defun python-repl-clear-buffer ()
  (interactive)
  (let ((start-point (point)))
    (goto-char (point-max))
    (move-beginning-of-line 1)
    (forward-char -4)
    (let* ((new-point (point))
	   (diff (- start-point new-point)))
      (delete-region (point-min) new-point)
      (if (<= 0 diff)
	  (forward-char diff)
	(goto-char (point-max))))))

(eval-after-load "python"
  '(progn
     (elpy-enable)
     (defvar inferior-python-mode-map)
     (define-key inferior-python-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
     (define-key inferior-python-mode-map (kbd "C-c M-o") 'python-repl-clear-buffer)
     (add-to-list 'safe-local-variable-values '(python-shell-interpreter . "python2"))))

(add-hook 'python-mode-hook #'add-delete-trailing-whitespace-before-save-hook)
(add-hook 'python-mode-hook #'electric-pair-mode)
(add-hook 'inferior-python-mode-hook #'electric-pair-mode)
(eval-after-load "elec-pair"
  '(progn
     (require 'paredit)
     (defvar electric-pair-mode-map)
     (define-key electric-pair-mode-map (kbd ")") #'paredit-close-round)))

(eval-after-load "elpy"
  '(progn
     (defvar elpy-mode-map)
     (define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)
     (define-key elpy-mode-map (kbd "C-c C-c") 'python-shell-send-defun)
     (define-key elpy-mode-map (kbd "C-c C-k") 'elpy-shell-send-region-or-buffer)))

(defvar flymake-no-changes-timeout)
(defun set-flymake-no-changes-timeout-to-one-hour ()
  (interactive)
  (set (make-local-variable 'flymake-no-changes-timeout) 3600))

(add-hook 'elpy-mode-hook #'set-flymake-no-changes-timeout-to-one-hour)

(eval-after-load "company"
  '(eval-after-load "yasnippet"
     '(progn
	(defvar company-active-map)
	(substitute-key-definition
	 'company-complete-common
	 'company-yasnippet-or-completion
	 company-active-map)(defvar yas-fallback-behavior)
	(defun company-yasnippet-or-completion ()
	  "Solve company yasnippet conflicts."
	  (interactive)
	  (let ((yas-fallback-behavior
		 '(apply 'company-complete-common nil)))
	    (with-no-warnings
	      (yas-expand)))))))

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun add-byte-compile-hook ()
  (add-hook 'after-save-hook #'byte-compile-current-buffer nil t))

(add-hook 'emacs-lisp-mode-hook 'add-byte-compile-hook)

(setq gc-cons-threshold 800000)
