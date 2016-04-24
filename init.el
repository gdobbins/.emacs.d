;; -*- lexical-binding: t; -*-
(setq inhibit-startup-screen t)
(when (fboundp #'menu-bar-mode)
  (menu-bar-mode -1))
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(setq ring-bell-function #'ignore)
(setq ad-redefinition-action 'accept)

(setq inhibit-startup-echo-area-message "graham")
(message "Live long and prosper.")
(setq initial-scratch-message
      ";;                                     ____
;;                           __...---~'    `~~~----...__
;;                        _===============================
;;   ,----------------._/'      `---..._______...---'
;;   (_______________||_) . .  ,--'
;;       /    /.---'         `/
;;      '--------_- - - - - _/
;;                `--------'\n\n")

(load "~/.emacs.d/secrets" t t)

(require 'server)
(unless (or (server-running-p) (string= user-login-name "root"))
  (add-hook 'emacs-startup-hook #'server-start))

(defvar hidden-minor-modes
  '(undo-tree-mode
    guide-key-mode
    abbrev-mode
    auto-revert-mode
    paredit-mode
    smartparens-mode
    elisp-slime-nav-mode
    yas-minor-mode
    eldoc-mode))

(defun purge-minor-modes ()
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
	(setcar trg "")))))

(add-hook 'after-change-major-mode-hook #'purge-minor-modes)

(defun command-line-diff (switch)
  (ignore switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(setq ediff-split-window-function #'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

(defun command-line-magit (switch)
  (ignore switch)
  (let ((git-dir (pop command-line-args-left)))
    (magit-status-internal (expand-file-name git-dir))
    (delete-other-windows)))

(add-to-list 'command-switch-alist '("-magit" . command-line-magit))

(eval-and-compile (require 'cl))

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
		(define-key map (vector last-input-event) #',func)
		map)))))))

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

(defadvice mark-sexp (before maybe-mark-prev-sexp compile activate)
  (when (looking-at ")")
    (backward-sexp)))

(defadvice pop-to-mark-command (around ensure-new-mark-position compile activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
	ad-do-it))))

(setq set-mark-command-repeat-pop t)

(setq kmacro-ring-max 24)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	ioccur-history
	kmacro-ring
	last-kbd-macro
	kmacro-counter
	kmacro-counter-format
	register-alist))

(setq sentence-end-double-space nil)

(with-eval-after-load 'warnings
  (defvar warning-suppress-types)
  (add-to-list 'warning-suppress-types '(undo discard-info)))

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setf browse-url-browser-function '(("." . browse-url-default-browser)))

(with-eval-after-load "eww"
  (defvar eww-mode-map)
  (define-key eww-mode-map (kbd "<mouse-8>") 'eww-back-url)
  (define-key eww-mode-map (kbd "<mouse-9>") 'eww-forward-url)
  (define-key eww-mode-map (kbd "C-c b") 'eww-back-url)
  (define-key eww-mode-map (kbd "C-c f") 'eww-forward-url))

(add-to-list 'browse-url-browser-function '("^file" . eww-browse-url))
(add-to-list 'browse-url-browser-function '("^https?://en\\.wiktionary\\.org/wiki/" . eww-browse-url))

(autoload 'ispell-get-word "ispell")

(defun wiktionary-word (word)
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (browse-url (format "https://en.wiktionary.org/wiki/%s" word)))

(global-set-key (kbd "M-#") #'wiktionary-word)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(global-set-key (kbd "C-x C-d") #'dired-jump)
(global-set-key (kbd "C-x 4 C-d") #'dired-jump-other-window)

(with-eval-after-load 'dired
  (require 'dired-x)
  (setq-default dired-omit-files-p t)
  (setq dired-listing-switches "-alhv")
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (define-key dired-mode-map (kbd "C-o") nil)
  (define-key dired-mode-map (kbd "M-p") #'dired-up-directory)
  (define-key dired-mode-map (kbd "M-n") #'quit-window)
  (define-key dired-mode-map (kbd "/") #'dired-narrow-fuzzy)
  (define-key dired-mode-map (kbd "h") #'dired-omit-mode)
  (define-key dired-mode-map (kbd "e") #'read-only-mode))

(with-eval-after-load 'wdired
  (defvar wdired-mode-map)
  (define-key wdired-mode-map (kbd "C-c C-g") 'wdired-abort-changes)
  (define-key wdired-mode-map (kbd "C-o") nil)
  (defvar wdired-allow-to-change-permissions)
  (setq wdired-allow-to-change-permissions t))

(global-set-key (kbd "C-x p") #'proced)
(setq-default proced-filter 'emacs)

(setq ibuffer-saved-filter-groups
      `(("default"
	 ("Lisp" (or
		  (mode . lisp-mode)
		  (mode . slime-repl-mode)
		  (mode . slime-inspector-mode)
		  (name . "^\\*slime-\\(description\\|compilation\\|xref\\)\\*$")
		  (name . "^\\*sldb .*\\*$")
		  (filename . "^/usr/local/doc/HyperSpec/")))
	 ("Python" (or
		    (mode . python-mode)
		    (mode . inferior-python-mode)
		    (name . "^\\*Python \\(Check\\|Doc\\)\\*$")))
	 ("Shell" (or
		   (mode . shell-mode)
		   (mode . term-mode)
		   (mode . sh-mode)
		   (derived-mode . conf-mode)
		   (derived-mode . shell-script-mode)
		   (name . "^\\*.* std\\(out\\|err\\)\\*$")
		   (name . "^\\*Shell Command Output\\*$")))
	 ("Assembly" (or
		      (mode . asm-mode)
		      (filename . "^/usr/local/doc/64-ia-32-architectures-software-developer-manual-325462\\.pdf$")))
	 ("Man" (or
		 (mode . Man-mode)
		 (mode . woman-mode)))
	 ("Data" (or
		  (filename . ".*\\.\\([ct]sv\\|dat\\)$")
		  (mode . hexl-mode)))
	 ("LaTeX" (or
		   (mode . latex-mode)
		   (mode . tex-shell)
		   (mode . TeX-output-mode)
		   (name . "^\\*\\(Latex Preview Pane \\(Welcome\\|Errors\\)\\|pdflatex-buffer\\)\\*$")))
	 ("Text" (mode . text-mode))
	 ("Books" (or
		   (mode . pdf-view-mode)
		   (filename . ,(concat "^" (expand-file-name "~/Books")))))
	 ("Web" (or
		 (mode . w3m-mode)
		 (mode . eww-mode)))
	 ("Org" (or (derived-mode . org-mode)
		    (mode . org-agenda-mode)))
	 ("Dired" (or
		   (mode . dired-mode)
		   (mode . wdired-mode)
		   (mode . archive-mode)
		   (mode . proced-mode)))
	 ("Git" (or (derived-mode . magit-mode)
		    (filename . "\\.git\\(ignore\\|attributes\\)$")))
	 ("Diff" (or
		  (mode . ediff-mode)
		  (name . "^\\*[Ee]?[Dd]iff.*\\*$")))
	 ("Gnus" (or (mode . message-mode)
		     (mode . bbdb-mode)
		     (mode . mail-mode)
		     (mode . gnus-group-mode)
		     (mode . gnus-summary-mode)
		     (mode . gnus-article-mode)
		     (name . "^\\.bbdb$")
		     (name . "^\\.newsrc-dribble$")
		     (name . "^\\*imap-log\\*$")
		     (mode . newsticker-mode)))
	 ("Emacs" (or
		   (mode . emacs-lisp-mode)
		   (mode . lisp-interaction-mode)
		   (mode . help-mode)
		   (mode . Info-mode)
		   (mode . package-menu-mode)
		   (mode . finder-mode)
		   (mode . Custom-mode)
		   (mode . apropos-mode)
		   (mode . ioccur-mode)
		   (mode . occur-mode)
		   (mode . reb-mode)
		   (mode . calc-mode)
		   (mode . calc-trail-mode)
		   (mode . messages-buffer-mode))))))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-jump-offer-only-visible-buffers t)

(defun ibuffer-mark-and-kill-lines ()
  "Mark the current line, then kill all marked lines."
  (interactive)
  (ibuffer-mark-forward 1)
  (ibuffer-do-kill-lines))

(with-eval-after-load "ibuffer"
  (define-key ibuffer-mode-map (kbd "H") nil)
  (define-key ibuffer-mode-map (kbd "k") #'ibuffer-do-delete)
  (define-key ibuffer-mode-map (kbd "C-k") #'ibuffer-mark-and-kill-lines)
  (define-key ibuffer-mode-map (kbd "M-o") nil)
  (defadvice ibuffer (around ibuffer-point-to-most-recent compile activate)
	     "Open ibuffer with cursor pointed to most recent buffer name"
	     (let ((recent-buffer-name (buffer-name)))
	       ad-do-it
	       (unless (string= recent-buffer-name "*Ibuffer*")
		 (ibuffer-jump-to-buffer recent-buffer-name))))
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (let ((buf-size (buffer-size)))
      (cond
       ((> buf-size 1048576)
	(format "%7.1fM" (/ buf-size 1048576.0)))
       ((> buf-size 1024)
	(format "%7.1fk" (/ buf-size 1024.0)))
       (t (format "%8d" buf-size)))))
  (setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename))))

(with-eval-after-load "ibuf-ext"
  (add-to-list 'ibuffer-never-show-predicates "^\\*slime-events\\*$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*inferior-lisp\\*\\(<[0-9]>\\)?$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Compile-Log\\*$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Completions\\*$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*tramp/sudo root@localhost\\*$")
  (add-to-list 'ibuffer-never-show-predicates "^\\*magit-process:.*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*WoMan-Log\\*$"))

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

(with-eval-after-load "re-builder"
  (defvar reb-mode-map)
  (define-key reb-mode-map (kbd "C-c C-g") 'reb-quit)
  (defvar reb-re-syntax)
  (setq reb-re-syntax 'string))

(with-eval-after-load "isearch"
  (require 'isearch+)
  (defvar isearch-mode-map)
  (define-key isearch-mode-map [remap ace-window] #'isearchp-open-recursive-edit)
  (defun endless/goto-match-beginning ()
    "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
    (when (and isearch-forward
	       (number-or-marker-p isearch-other-end)
	       (not mark-active)
	       (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end)))
  (add-hook 'isearch-mode-end-hook #'endless/goto-match-beginning))

(require 'slime)
(slime-setup '(slime-fancy))

(let ((quicklisp-setup-file (expand-file-name "~/quicklisp/setup.lisp")))
  (when (file-exists-p "/usr/bin/clisp")
    (add-to-list 'slime-lisp-implementations
		 '(clisp ("/usr/bin/clisp"))))

  (when (file-exists-p "/usr/bin/ecl")
    (add-to-list 'slime-lisp-implementations
		 '(ecl ("/usr/bin/ecl"))))

  (when (file-exists-p "~/sbcl/")
    (add-to-list 'slime-lisp-implementations
		 `(sbcl-git (,(expand-file-name "~/sbcl/run-sbcl.sh") "--dynamic-space-size" "3096" "--load" ,quicklisp-setup-file))))

  (when (file-exists-p "/usr/bin/sbcl")
    (add-to-list 'slime-lisp-implementations
		 '(sbcl-raw ("/usr/bin/sbcl")))

    (when (file-exists-p quicklisp-setup-file)
      (add-to-list 'slime-lisp-implementations
		   `(sbcl-debug ("/usr/bin/sbcl" "--dynamic-space-size" "3096" "--load" ,quicklisp-setup-file))))

    (when (file-exists-p "~/lisp/sbcl.core-graham")
      (let ((sbcl-core-graham (expand-file-name "~/lisp/sbcl.core-graham")))
	(add-to-list 'slime-lisp-implementations
		     `(sbcl-big ("/usr/bin/sbcl" "--core" ,sbcl-core-graham "--dynamic-space-size" "6192")))
	(add-to-list 'slime-lisp-implementations
		     `(sbcl ("/usr/bin/sbcl" "--core" ,sbcl-core-graham "--dynamic-space-size" "3096")))))))

(when (file-exists-p "/usr/local/doc/HyperSpec/")
  (setq common-lisp-hyperspec-root "file:///usr/local/doc/HyperSpec/"))

(setq
 backup-by-copying t			; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/saves"))		; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)			; use versioned backups

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(setq show-paren-delay 0)
(show-paren-mode 1)

(setq-default line-move-visual nil)

(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(require 'ido-sort-mtime)
(ido-sort-mtime-mode 1)
(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)
(require 'ido-hacks)
(ido-hacks-mode 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode)
(global-set-key (kbd "M-x") #'smex)

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

(defun pathname->~->home ()
  (interactive)
  (if (looking-back "/" 1)
      (insert "~/")
    (call-interactively #'self-insert-command)))

(defun pathname->/->root ()
  (interactive)
  (if (looking-back "/" 1)
      (insert "//")
    (call-interactively #'self-insert-command)))

(defset-function pathname->~->home "~" ido-completion-map)
(defset-function pathname->/->root "/" ido-completion-map)

(add-hook 'ido-setup-hook #'set-pathname->~->home)
(add-hook 'ido-setup-hook #'set-pathname->/->root)
(add-hook 'ido-setup-hook (lambda ()
			    (define-key ido-completion-map (kbd "C-c") nil)
			    (define-key ido-completion-map (kbd "M-c") #'ido-toggle-case)))

(add-to-list 'ido-ignore-files "^\\.smex-items$")
(add-to-list 'ido-ignore-files "^\\.slime-history\\.eld$")
(add-to-list 'ido-ignore-files "^\\.python_history$")
(add-to-list 'ido-ignore-files "^\\.w3m/$")
(add-to-list 'ido-ignore-files "^\\.histfile$")
(add-to-list 'ido-ignore-files "^ido\\.last$")
(add-to-list 'ido-ignore-files "^\\.newsrc\\(\\.eld\\)?$")

(setq ido-file-extensions-order '(".el" ".org" ".lisp" ".py" ".pdf" t))
(setq ido-use-filename-at-point 'guess)

(setq password-cache-expiry 300)

(defadvice ido-find-file (after find-file-sudo compile activate)
  "Find file as root if necessary."
  (when (and buffer-file-name
	     (not (file-writable-p buffer-file-name))
	     (file-directory-p default-directory))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "M-/") #'hippie-expand)

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

(setq tab-always-indent 'complete)

(require 'projectile)

(setq projectile-switch-project-action #'projectile-commander
      projectile-sort-order 'modification-time
      projectile-ignored-project-function #'file-remote-p)

(def-projectile-commander-method ?s
  "Open a *shell* buffer for the project."
  (shell (get-buffer-create
          (format "*shell %s*"
                  (projectile-project-name)))))

(def-projectile-commander-method ?\C-?
  "Go back to project selection."
  (projectile-switch-project))

(def-projectile-commander-method ?d
  "Open project root in dired."
  (projectile-dired))

(def-projectile-commander-method ?D
  "Find directory in project."
  (projectile-find-dir))

(define-key projectile-mode-map (kbd "C-c p d") #'projectile-dired)
(define-key projectile-mode-map (kbd "C-c p D") #'projectile-find-dir)

(defun projectile-jack-in ()
  "Attempt to jack-in to the project."
  (delete-other-windows)
  (let ((root (projectile-project-root)))
    (cond ((string-match (expand-file-name "~/quicklisp/local-projects/\\(.*\\)/$")
			 root)
	   (let ((cl-package (match-string 1 root)))
	     (if (directory-files root nil ".*\\.lisp$" t)
		 (find-file (concat root "*.lisp") t)
	       (dired root))
	     (if cl-package
		 (progn
		   (slime-repl-eval-string
		    (format "(when (ql:quickload :%s :silent t) (in-package :%s))"
			    cl-package
			    cl-package))
		   (slime-switch-to-output-buffer)
		   (cd root))
	       (projectile-vc root))))
	  ((string-match (expand-file-name "~/.emacs.d/") root)
	   (find-file user-init-file)
	   (projectile-vc root))
	  (t (dired root)
	     (projectile-vc root)))))

(def-projectile-commander-method ?j
  "Jack in."
  (projectile-jack-in))

(defun projectile-direct-jack-in ()
  "Switch to a project and jack-in."
  (interactive)
  (let ((projectile-switch-project-action #'projectile-jack-in))
    (projectile-switch-project)))

(define-key projectile-mode-map (kbd "H-p") #'projectile-direct-jack-in)

(setq projectile-mode-line
      '(:eval
	(if (file-remote-p default-directory)
	    " Proj"
	  (format " Proj[%s]"
		  (projectile-project-name)))))

(autoload 'dired-read-shell-command "dired-aux")

(defun projectile-dired-command-files ()
  (interactive)
  (let ((files (projectile-current-project-files)))
    (dired-do-shell-command
     (dired-read-shell-command "! on %s: " current-prefix-arg files)
     current-prefix-arg
     files)))

(define-key projectile-mode-map (kbd "C-c p x !") #'projectile-dired-command-files)

(projectile-global-mode)

(defun truncate-lines->t ()
  (interactive)
  (unless truncate-lines
    (setq truncate-lines t)))

(add-hook 'dired-mode-hook #'truncate-lines->t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(require 'saveplace)
(setq-default save-place t)

(defun undo-tree-save-history-ignore-file (orig &rest args)
  "Ignore files matching the regex, otherwise save history"
  (unless (string-match ".*\\.gpg$" buffer-file-name)
    (apply orig args)))

(require 'undo-tree)
(global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (define-key undo-tree-visualizer-mode-map (kbd "RET") #'undo-tree-visualizer-quit)
  (unless (string= user-login-name "root")
    (setq undo-tree-auto-save-history t))
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/")))
  (advice-add 'undo-tree-save-history :around #'undo-tree-save-history-ignore-file))

(with-eval-after-load "x86-lookup"
  (let ((location "/usr/local/doc/64-ia-32-architectures-software-developer-manual-325462.pdf"))
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

(defun insert-splice ()
  (interactive)
  (if (looking-back "[^ \n]" 1)
      (insert " "))
  (insert ",@()")
  (backward-char)
  (run-hooks 'post-self-insert-hook))

(autoload 'sh-show-shell "sh-script" "Pop the shell interaction buffer." t)
(autoload 'elpy-shell-switch-to-shell "elpy" "Switch to inferior Python process buffer.")

(defun smart-switch-to-output-buffer ()
  (interactive)
  (cond
   ((get-process "inferior-lisp") (slime-switch-to-output-buffer))
   ((get-process "shell") (sh-show-shell))
   ((get-process "Python") (elpy-shell-switch-to-shell))
   (t (user-error "No available process to switch to."))))

(autoload 'magit-ido-completing-read "magit-utils" "Ido-based `completing-read' almost-replacement.")
(with-eval-after-load "magit"
  (defvar magit-completing-read-function)
  (setq magit-completing-read-function #'magit-ido-completing-read))

(add-to-list 'safe-local-variable-values
	     '(magit-commit-arguments . ("--gpg-sign=F1F2C078BAEE096B!")))

(setq ioccur-buffer-completion-use-ido t)

(setq vc-follow-symlinks t)

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

(with-eval-after-load "ioccur"
  (defvar ioccur-mode-map)
  (define-key ioccur-mode-map (kbd "M-n") #'ioccur-follow-next)
  (define-key ioccur-mode-map (kbd "M-p") #'ioccur-follow-previous)
  (define-key ioccur-mode-map (kbd "n") #'ioccur-next-line)
  (define-key ioccur-mode-map (kbd "p") #'ioccur-precedent-line)
  (define-key ioccur-mode-map (kbd "z") #'ioccur-jump-without-quit)
  (define-key ioccur-mode-map (kbd "r") #'ioccur-restart)
  (define-key ioccur-mode-map (kbd "s") #'ioccur-restart)
  (define-key ioccur-mode-map (kbd "o") #'ioccur-restart))

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer t t t)
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

(defun message-current-time ()
  "Print the current time in the mini-buffer."
  (interactive)
  (message (format-time-string "%T %A %B %e, %Y")))

(with-eval-after-load "alert"
  (defvar alert-default-style)
  (setq alert-default-style 'libnotify))

(defun egg-timer (x)
  "Set a timer to go off in x minutes, default to 5."
  (interactive "P")
  (let ((time (cond
	       ((equal x nil) "5")
	       ((listp x) (if (= (first x) 4)
			      (read-string "Number of minutes: " nil nil "10")
			    (format "%d"
				    (* (string-to-number
					(read-string "Number of hours: " nil nil "1"))
				       60))))
	       (t (format "%d" x)))))
    (run-at-time (concat time " min") nil
		 (lambda ()
		   (let ((visible-bell t)
			 (ring-bell-function nil))
		     (ding t)
		     (alert (concat time " minutes timer has finished.")
			    :category 'egg-timer))))
    (message (concat "Timer set for " time " minutes."))))

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

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(setq aw-keys (list ?h ?t ?n ?s ?a ?o ?e ?u))
(setq avy-keys aw-keys)

(define-prefix-command 'my-other-window-map)
(make-last-key-repeating-function duplicate-other-window-buffer my-other-window-map nil)
(define-key my-other-window-map (kbd "C-c s") #'other-window-imenu)
(define-key my-other-window-map (kbd "C-s") #'other-window-isearch-forward)
(define-key my-other-window-map (kbd "C-r") #'other-window-isearch-backward)
(define-key my-other-window-map (kbd "C-v") #'scroll-other-window)
(define-key my-other-window-map (kbd "C-M-v") #'scroll-other-window)
(define-key my-other-window-map (kbd "M-v") #'scroll-other-window-down)

(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-c o") #'ioccur)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c l") #'slime-switch-to-output-buffer)
(global-set-key (kbd "C-c t") #'sh-show-shell)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-k") #'kill-this-buffer)
(global-set-key (kbd "C-c w r") #'rotate-windows)
(global-set-key (kbd "C-c w t") #'toggle-window-split)
(global-set-key (kbd "C-c w C-k") #'really-desktop-clear)
(global-set-key (kbd "C-c w k") #'kill-other-buffers)
(global-set-key (kbd "C-c w u") #'revert-this-buffer)
(global-set-key (kbd "C-c w l") #'count-buffer-lines)
(global-set-key (kbd "C-c w f") #'rename-current-buffer-file)
(global-set-key (kbd "C-c w d") #'duplicate-other-window-buffer)
(global-set-key (kbd "C-c w c") #'emacs-uptime)
(global-set-key (kbd "C-c w i") #'find-user-init-file)
(global-set-key (kbd "C-c w e") #'erase-buffer)
(global-set-key (kbd "C-c w s") #'delete-trailing-whitespace)
(global-set-key (kbd "C-c w n") #'column-number-mode)
(global-set-key (kbd "C-c w m") #'calc)
(global-set-key (kbd "C-c w w") #'message-current-time)
(global-set-key (kbd "C-c w p") #'package-list-packages)
(global-set-key (kbd "C-c d") #'duplicate-other-window-buffer)
(global-set-key (kbd "C-c C-z") #'smart-switch-to-output-buffer)
(global-set-key (kbd "C-c z") #'smart-switch-to-output-buffer)
(global-set-key (kbd "C-c e") #'eval-and-replace)
(global-set-key (kbd "C-c '") (kbd "`"))
(global-set-key (kbd "C-c -") (kbd "~"))
(global-set-key (kbd "C-c ,") #'insert-splice)
(global-set-key (kbd "C-c b") #'previous-buffer)
(global-set-key (kbd "C-c f") #'next-buffer)
(global-set-key (kbd "C-c k") #'kmacro-keymap)
(global-set-key (kbd "C-c s") #'imenu)
(global-set-key (kbd "C-c y") #'browse-kill-ring)
(global-set-key (kbd "C-c c") #'endless/ispell-word-then-abbrev)
(global-set-key (kbd "C-c n") #'gnus)
(global-set-key (kbd "C-x w b") #'hi-lock-write-interactive-patterns)
(global-set-key (kbd "C-x w r") #'unhighlight-regexp)
(global-set-key (kbd "C-x w h") #'highlight-regexp)
(global-set-key (kbd "C-x w p") #'highlight-phrase)
(global-set-key (kbd "C-x w l") #'highlight-lines-matching-regexp)
(global-set-key (kbd "C-x w i") 'hi-lock-find-patterns)
(global-set-key (kbd "C-x r S") #'activate-word-column-region)
(global-set-key (kbd "C-h x") #'x86-lookup)
(global-set-key (kbd "C-h r") #'re-builder)
(global-set-key (kbd "C-h s") #'string-edit-at-point)
(global-set-key (kbd "C-h C-f") #'find-function)
(global-set-key (kbd "C-h C-k") #'find-function-on-key)
(global-set-key (kbd "C-h C-v") #'find-variable)
(global-set-key (kbd "C-x o") #'ace-window)
(global-set-key (kbd "M-o") #'ace-window)
(global-set-key (kbd "M-n") #'smartscan-symbol-go-forward)
(global-set-key (kbd "M-p") #'smartscan-symbol-go-backward)
(global-set-key (kbd "C-o") #'avy-goto-char)
(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "C-z") #'repeat)
(global-set-key (kbd "<f5>") #'egg-timer)
(global-set-key (kbd "C-x c") #'server-edit)
(global-set-key (kbd "C-x #") nil)
(global-set-key (kbd "C-a") #'smarter-move-beginning-of-line)

(autoload 'mpc-resume "mpc")
(autoload 'mpc-pause "mpc")
(autoload 'mpc-next "mpc")
(autoload 'mpc-prev "mpc")

(define-prefix-command 'mpc-repeating-map)
(global-set-key (kbd "C-c m") #'mpc-repeating-map)
(global-set-key (kbd "C-c m m") #'mpc)
(make-last-key-repeating-function mpc mpc-repeating-map)
(global-set-key (kbd "C-c m r") #'mpc-resume)
(make-last-key-repeating-function mpc-resume mpc-repeating-map)
(global-set-key (kbd "C-c m p") #'mpc-pause)
(make-last-key-repeating-function mpc-pause mpc-repeating-map)
(global-set-key (kbd "C-c m n") #'mpc-next)
(make-last-key-repeating-function mpc-next mpc-repeating-map)
(global-set-key (kbd "C-c m l") #'mpc-prev)
(make-last-key-repeating-function mpc-prev mpc-repeating-map)
(global-set-key (kbd "C-c m w") #'woman)

(global-set-key (kbd "<mouse-8>") #'previous-buffer)
(global-set-key (kbd "<mouse-9>") #'next-buffer)

(defun setup-input-decode-map ()
  (define-key input-decode-map (kbd "C-t") (kbd "C-x"))
  (define-key input-decode-map (kbd "C-x") (kbd "C-t"))
  (define-key input-decode-map (kbd "M-t") (kbd "M-x"))
  (define-key input-decode-map (kbd "M-x") (kbd "M-t"))
  (define-key input-decode-map (kbd "C-M-t") (kbd "C-M-x"))
  (define-key input-decode-map (kbd "C-M-x") (kbd "C-M-t"))

  (when (display-graphic-p)
    (define-key input-decode-map (kbd "C-m") (kbd "C-SPC"))
    (define-key input-decode-map (kbd "C-M-m") (kbd "C-M-SPC")))

  (define-key input-decode-map (kbd "C-h") (kbd "DEL"))
  (define-key input-decode-map (kbd "M-h") (kbd "M-DEL"))
  (define-key input-decode-map (kbd "H-h") (kbd "C-h")))

(setup-input-decode-map)

(add-hook 'tty-setup-hook #'setup-input-decode-map)

(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "<mouse-8>") #'help-go-back)
  (define-key help-mode-map (kbd "<mouse-9>") #'help-go-forward)
  (define-key help-mode-map (kbd "C-c b") #'help-go-back)
  (define-key help-mode-map (kbd "C-c f") #'help-go-forward)
  (define-key help-mode-map (kbd "n") #'next-line)
  (define-key help-mode-map (kbd "p") #'previous-line)
  (defadvice describe-bindings (after describe-bindings-move-to-major-mode compile activate)
    "Pop to the help buffer and search forward for the Major mode bindings."
    (pop-to-buffer (help-buffer))
    (search-forward "Major Mode Bindings:" nil t)
    (recenter-top-bottom 0)))

(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(with-eval-after-load "elisp-slime-nav"
  (define-key elisp-slime-nav-mode-map (kbd "C-c M-e") #'macrostep-expand)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-d") #'elisp-slime-nav-describe-elisp-thing-at-point)
  (define-key elisp-slime-nav-mode-map (kbd "C-c C-c") #'eval-defun))
(add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(defadvice he-substitute-string (after he-paredit-fix compile activate)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

(add-to-list 'default-frame-alist '(font . "Linux Libertine Mono:pixelsize=14"))
(setq split-width-threshold 100)

(setq-default comint-prompt-read-only t)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(with-eval-after-load "shell"
  (add-hook 'shell-mode-hook #'truncate-lines->t)
  (defvar shell-mode-map)
  (define-key shell-mode-map (kbd "C-d") #'comint-delchar-or-eof-or-kill-buffer))

(with-eval-after-load "em-term"
  (defvar eshell-visual-commands)
  (add-to-list 'eshell-visual-commands "htop"))

(autoload 'LaTeX-math-mode "latex" "A minor mode with easy access to TeX math macros.")
(add-hook 'LaTeX-mode-hook #'flyspell-mode)
(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'latex-preview-pane-mode)

(defun fix-capitalization->paren ()
  "Search backward and replace a capital letter with (<lowercase letter>"
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (re-search-backward "[A-Z]" (point-at-bol))
      (replace-match (concat "(" (downcase (match-string 0))) t t nil 0))
    (insert ")")
    (backward-char 1)
    (run-hooks 'post-self-insert-hook)))

(with-eval-after-load "paredit"
  (define-key paredit-mode-map (kbd "C-c (") #'fix-capitalization->paren)
  (add-to-list 'paredit-space-for-delimiter-predicates
	       #'paredit-space-for-predicates-cl))

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
  "Do not add a space when \" is preceded by #. reader macro or ( is preceded by [ (],@ or #.=?"
  (or endp
      (cond ((eq (char-syntax delimiter) ?\")
	     (not (looking-back "#." 2)))
	    ((eq (char-syntax delimiter) ?\()
	     (not (looking-back "\\([\n\s-(],@\\)\\|\\(#.=?\\)" 3)))
	    (t t))))

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

(with-eval-after-load "lisp-mode"
  (font-lock-add-keywords 'lisp-mode
			  '(("(\\(iter\\(ate\\)?\\|defmacro-\\(driver\\|clause\\)\\)" 1 'font-lock-keyword-face)
			    ("(define-constant[ \n\r\t]+\\(\\(\\sw\\|\\s_\\)*\\)" 1 'font-lock-variable-name-face))
			  t)
  (define-key lisp-mode-map (kbd "C-c e") #'slime-eval-and-replace)
  (define-key lisp-mode-map (kbd "C-c C-s") #'slime-scratch))

(defun slime-return-to-lisp-file ()
  "Go backwards through the buffer list until one in lisp mode is found."
  (interactive)
  (dolist (buff (buffer-list) (current-buffer))
    (if (equal 'lisp-mode
	       (buffer-local-value 'major-mode buff))
	(progn
	  (pop-to-buffer buff)
	  (return buff)))))

(with-eval-after-load "slime-repl"
  (setq slime-auto-start 'always)
  (setq slime-repl-history-size 500)
  (define-key slime-repl-mode-map (kbd "C-c C-s") #'slime-scratch)
  (define-key slime-repl-mode-map (kbd "C-c e") #'slime-eval-and-replace)
  (define-key slime-repl-mode-map (kbd "<up>") #'slime-repl-previous-input)
  (define-key slime-repl-mode-map (kbd "<down>") #'slime-repl-next-input)
  (define-key slime-repl-mode-map (kbd "C-c C-z") #'slime-return-to-lisp-file)
  (define-key slime-repl-mode-map (kbd "C-c C-q") #'slime-quit-lisp))

(defadvice slime-scratch (after slime-scratch-add-top-line-comment compile activate)
  (unless (/= 1 (point))
    (unless (looking-at ".")
      (insert ";; This buffer is for notes you don't want to save, and for Common Lisp evaluation.\n\n"))))

(add-hook 'emacs-lisp-mode-hook				#'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook	#'enable-paredit-mode)
(add-hook 'ielm-mode-hook				#'enable-paredit-mode)
(add-hook 'lisp-mode-hook				#'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook			#'enable-paredit-mode)
(add-hook 'scheme-mode-hook				#'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook				#'enable-paredit-mode)
(add-hook 'eshell-mode-hook				#'enable-paredit-mode)

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

(pdf-tools-install)
(define-key pdf-view-mode-map (kbd "a") #'image-bol)
(define-key pdf-view-mode-map (kbd "e") #'image-eol)

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

(add-hook 'pdf-view-mode-hook #'set-mode-line-numbers-at-front)

(with-eval-after-load "org"
  (defvar org-modules)
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (defvar org-mode-map)
  (define-key org-mode-map (kbd "C-c i") #'interleave)
  (define-key org-mode-map (kbd "M-n") #'outline-next-visible-heading)
  (define-key org-mode-map (kbd "M-p") #'outline-previous-visible-heading)
  (defvar org-agenda-sticky)
  (setq org-agenda-sticky t))

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

(with-eval-after-load "python"
  (elpy-enable)
  (defvar inferior-python-mode-map)
  (define-key inferior-python-mode-map (kbd "C-d") #'comint-delchar-or-eof-or-kill-buffer)
  (define-key inferior-python-mode-map (kbd "C-c M-o") #'python-repl-clear-buffer)
  (add-to-list 'safe-local-variable-values '(python-shell-interpreter . "python2"))
  (add-hook 'python-mode-hook #'smartparens-strict-mode)
  (add-hook 'inferior-python-mode-hook #'smartparens-strict-mode)
  (require 'smartparens-python))

(with-eval-after-load "smartparens"
  (sp-use-paredit-bindings)
  (with-no-warnings
    (defun sp-paredit-like-close-round ()
      "If the next character is a closing character as according to smartparens skip it, otherwise insert `last-input-event'"
      (interactive)
      (let ((pt (point)))
	(if (and (< pt (point-max))
		 (sp--char-is-part-of-closing (buffer-substring-no-properties pt (1+ pt))))
	    (forward-char 1)
	  (call-interactively #'self-insert-command))))
    (define-key smartparens-mode-map (kbd ")") #'sp-paredit-like-close-round)))

(with-eval-after-load "elpy"
  (add-hook 'elpy-mode-hook #'set-flymake-no-changes-timeout-to-one-hour)
  (defvar elpy-mode-map)
  (define-key elpy-mode-map (kbd "M-,") #'pop-tag-mark)
  (define-key elpy-mode-map (kbd "C-c C-c") 'python-shell-send-defun)
  (define-key elpy-mode-map (kbd "C-c C-k") 'elpy-shell-send-region-or-buffer))

(defvar flymake-no-changes-timeout)
(defun set-flymake-no-changes-timeout-to-one-hour ()
  (interactive)
  (set (make-local-variable 'flymake-no-changes-timeout) 3600))

(with-eval-after-load "company"
  (with-eval-after-load "yasnippet"
    (defvar company-active-map)
    (substitute-key-definition
     'company-complete-common
     'company-yasnippet-or-completion
     company-active-map)
    (defvar yas-fallback-behavior)
    (defun company-yasnippet-or-completion ()
      "Solve company yasnippet conflicts."
      (interactive)
      (let ((yas-fallback-behavior
	     '(apply 'company-complete-common nil)))
	(with-no-warnings
	  (yas-expand))))))

(with-eval-after-load "yasnippet"
  (with-no-warnings
    (yas-reload-all)))

(put 'set-goal-column	'disabled nil)
(put 'erase-buffer	'disabled nil)
(put 'downcase-region	'disabled nil)
(put 'upcase-region	'disabled nil)

(defun add-delete-trailing-whitespace-hook ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'add-delete-trailing-whitespace-hook)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun add-byte-compile-hook ()
  (add-hook 'after-save-hook #'byte-compile-current-buffer nil t))

(add-hook 'emacs-lisp-mode-hook #'add-byte-compile-hook)
