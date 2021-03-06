;; -*- lexical-binding: t; -*-
(setq inhibit-startup-screen t)
(when (fboundp #'menu-bar-mode)
  (menu-bar-mode -1))
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq gc-cons-threshold 100000000)

(defun delayed-initialization ()
  "Return `gc-cons-threshold' to it's original value and perform
some other initialization operations which slow startup time."
  (require 'magit)
  (require 'avy)
  (require 'dired)
  (require 'hippie-exp)
  (remove-hook 'focus-out-hook #'delayed-initialization)
  (setq gc-cons-threshold 800000)
  (garbage-collect)
  (message "Finished delayed initialization."))

(defun setup-delayed-initialization ()
  "Setup `delayed-initialization' to run when emacs is idle or
  loses focus."
  (run-with-idle-timer
   60
   nil
   #'delayed-initialization)
  (add-hook 'focus-out-hook #'delayed-initialization))

(add-hook 'emacs-startup-hook #'setup-delayed-initialization)

(defvar my/on-mobile-device
  (eval-when-compile
    (string=
     (shell-command-to-string "uname -o") "Android\n")))

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
  (define-key input-decode-map (kbd "H-h") (kbd "C-h"))

  (define-key input-decode-map (kbd "C-S-g") (kbd "C-g")))

(unless my/on-mobile-device
  (setup-input-decode-map)
  (add-hook 'tty-setup-hook #'setup-input-decode-map))

(setq ring-bell-function #'ignore)
(setq ad-redefinition-action 'accept)

(setq inhibit-startup-echo-area-message "graham")
(message "Live long and prosper.")
(setq initial-scratch-message
      (concat
       ";;                                     ____
;;                           __...---~'    `~~~----...__
;;                        _===============================
;;   ,----------------._/'      `---..._______...---'
;;   (_______________||_) . .  ,--'
;;       /    /.---'         `/
;;      '--------_- - - - - _/
;;                `--------'\n;;\n"
       (let ((fortune-file "~/.emacs.d/fortunes"))
	 (when (and (file-readable-p fortune-file)
		    (eval-when-compile
		      (file-exists-p "/usr/share/fonts/TTF/Klingon-pIqaD-HaSta.ttf")))
	   (with-temp-buffer
	     (insert-file-contents fortune-file)
	     (let ((num-fortunes (prog1 (thing-at-point 'number t)
				   (forward-line)))
		   (lines-per (prog1 (thing-at-point 'number t)
				(forward-line))))
	       (dotimes (_ (* (random num-fortunes) (1+ lines-per)))
		 (forward-line))
	       (apply #'concat
		      (cl-loop repeat lines-per collect
			   (concat
			    ";;   "
			    (thing-at-point 'line t))
			 do (forward-line)))))))
       "\n\n"))

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(when (eval-when-compile (file-exists-p "~/home-row-numbers"))
  (add-to-list 'load-path (expand-file-name "~/home-row-numbers")))

(package-initialize)

(eval-and-compile
  (defun my/ensure-cons (cons)
    "If CONS is a cons it is returned, else a fresh cons is made
with CONS as its car."
    (if (consp cons) cons (list cons))))

(defvar used-command-flags nil
  "List of command flags used by `if-command-flag' and related.")

(defun my-cf/check (flag)
  "Workhorse of `if-command-flag'."
  (or
   (and (member flag command-line-args)
	(setf command-line-args (delete flag command-line-args))
	(push flag used-command-flags))
   (member flag used-command-flags)))

(defmacro if-command-flag (flag then &rest else)
  "If FLAG is present in `command-line-args' or
  `used-command-flags' execute THEN , otherwise execute ELSE. If
  present in `command-line-args', then move it to
  `used-command-flags'. If FLAG is a list of flags then check
  sequentially with an implicit or."
  (declare (indent 1))
  `(if (or ,@(cl-loop for f in (my/ensure-cons flag) collect `(my-cf/check ,f)))
       ,then
     ,@else))

(defmacro unless-command-flag (flag &rest body)
  "Unless FLAG is present in `command-line-args' execute BODY.
FLAG is then removed if found and added to `used-command-flags'."
  (declare (indent 1))
  `(if-command-flag ,flag nil ,@body))

(unless-command-flag
    "--no-secrets"
  (or
   (load "~/.emacs.d/secrets" t t)
   (copy-file "~/.emacs.d/secrets-skeleton.el"
	      (setq custom-file "~/.emacs.d/secrets.el"))))

(when (and (eval-when-compile (>= emacs-major-version 25))
	   (not package-selected-packages))
  (setq package-selected-packages
	'(ace-window alert auctex avy avy-zap browse-kill-ring cider dired-narrow edit-server elpy expand-region flycheck ido-sort-mtime ido-completing-read+ ido-yes-or-no interleave ioccur latex-preview-pane lua-mode magit multi-term paredit pdf-tools pkgbuild-mode projectile sage-shell-mode slime smartparens smartscan smex solarized-theme string-edit undo-tree writegood-mode wttrin x86-lookup))
  (package-refresh-contents)
  (package-install-selected-packages)
  (dolist (file (cons user-init-file
		      (directory-files-recursively
		       (concat user-emacs-directory "lisp/")
		       "\\.el\\'")))
    (unless (file-exists-p (byte-compile-dest-file file))
      (byte-compile-file file))))

(unless-command-flag
    "--no-theme"
  (require 'theme))

(autoload #'server-running-p "server")
(autoload #'server-edit "server")

(unless-command-flag
    "--no-server"
  (require 'server)
  (unless (or (server-running-p) (string= user-login-name "root"))
    (add-hook 'emacs-startup-hook #'server-start)))

(unless-command-flag
    ("--no-pinentry" "--no-server")
  (when (eval-when-compile
	  (and
	   (>= emacs-major-version 25)
	   (file-exists-p "~/.gnupg/gpg-agent.conf")
	   (with-temp-buffer
	     (insert-file-contents "~/.gnupg/gpg-agent.conf")
	     (search-forward-regexp "^allow-emacs-pinentry" nil t))))
    (unless (server-running-p)
      (require 'epa)
      (setq epa-pinentry-mode 'loopback))))

(defvar hidden-minor-modes
  '(undo-tree-mode
    abbrev-mode
    auto-revert-mode
    paredit-mode
    smartparens-mode
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
  "For use with `command-switch-alist'. When emacs is passed the
flag SWITCH followed by two or three file names, call `ediff' on
those files."
  (add-to-list 'used-command-flags switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left))
	(file3 (and
		command-line-args-left
		(file-readable-p (car command-line-args-left))
		(pop command-line-args-left))))
    (if file3
	(ediff3 file1 file2 file3)
      (ediff file1 file2))))

(with-eval-after-load 'ediff
  (with-no-warnings
    (setq ediff-split-window-function #'split-window-horizontally)
    (setq ediff-window-setup-function #'ediff-setup-windows-plain)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))

(defun command-line-magit (switch)
  (add-to-list 'used-command-flags switch)
  (let ((git-dir (pop command-line-args-left)))
    (magit-status-setup-buffer (expand-file-name git-dir))
    (delete-other-windows)))

(add-to-list 'command-switch-alist '("-magit" . command-line-magit))

;; When using StumpWM, starting Emacs in fullscreen removes black
;; borders, but leaving it fullscreen interferes with windowing
;; commands. Using this function as a command switch in conjunction
;; with -fs allows both problems to be fixed.
(defun de-fullscreen (switch)
  "When the current frame is fullscreen, `toggle-frame-fullscreen'."
  (add-to-list 'used-command-flags switch)
  (when (frame-parameter nil 'fullscreen)
    (run-with-timer 2 nil #'toggle-frame-fullscreen)))

(add-to-list 'command-switch-alist '("-de-fs" . de-fullscreen))

(with-no-warnings
  (eval-and-compile (require 'cl)))

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

(defmacro last-key-repeating (&optional function)
  "Make the command call FUNCTION by the pressing of
`last-input-event'. If nil repeat `this-command'."
  `(set-transient-map
    (let ((map (make-sparse-keymap)))
      (define-key map (vector last-input-event)
	,(cond
	   ((null function) 'this-command)
	   ((symbolp function) `#',function)
	   (t function)))
      map)))

(eval-and-compile
  (defun my/maybe-append-map (keymap)
    "Append -map to the symbol in KEYMAP if not already present."
    (let ((symbol-name (symbol-name keymap)))
      (intern
       (if (string-match "-map\\'" symbol-name)
	   symbol-name
	 (concat symbol-name "-map"))))))

(defmacro defkey (key def &optional keymap)
  "Convenience macro for defining keybindings."
  (let ((map (if keymap
		 (my/maybe-append-map keymap)
	       '(current-global-map))))
    `(define-key ,map
	 ,(if (stringp key) `(kbd ,key) key)
       ,(cond
	  ((symbolp def) `#',def)
	  ((stringp def) `(kbd ,def))
	  ((and (listp def)
		(eq (first def) 'quote))
	   (second def))
	  (t def)))))

(defmacro defkeys (&rest args)
  "Passes args pairwise to `defkey', optional first argument is
  used as the KEYMAP argument. If KEYMAP is a list, keys are
  bound in each keymap in the list. If the keymaps in said list
  are a list then the first position denotes the map, the second
  whether to defvar the map and the third whether to defer
  binding until after loading."
  (let* ((keymap
	  (when (cl-oddp (length args))
	    (pop args))))
    `(progn
       ,@(cl-loop for map in (my/ensure-cons keymap) collect
	      (append (if (and (listp map) (third map))
			  `(with-eval-after-load
			       ',(if (eq t (third map))
				     (let ((symbol-name (symbol-name (first map))))
				       (intern
					(if (string-match "-mode\\'" symbol-name)
					    (substring symbol-name 0 -5)
					  symbol-name)))
				   (third map)))
			'(progn))
		      (when (and (listp map) (second map))
			`((defvar ,(if (eq (second map) t)
				       (my/maybe-append-map (first map))
				     (second map)))))
		      (cl-loop
			 for (key def) on args by #'cddr collect
			   `(defkey ,key ,def ,(if (listp map) (first map) map))))))))

(defun my/push-key (key)
  "Return an interactive function which simulates pushing KEY.
  This gets around several problems with binding to a keyboard
  macro."
  (declare (pure t))
  (let* ((key (kbd key))
	 (event (aref key (1- (length key)))))
    (lambda ()
      (interactive)
      (let ((last-command-event event))
	(call-interactively (key-binding key t))))))

(eval-and-compile
  (defun my/regexp-part->char-match (regexp)
    (or
     (case (length regexp)
       (1
	(cons
	 (case (aref regexp 0)
	   (?$ '(eolp))
	   (?^ '(bolp))
	   (t
	    `(= direction ,(aref regexp 0))))
	 nil))
       (3 (and (= ?\\ (aref regexp 0)) (= ?s (aref regexp 1))
	       (cons `(= char-syntax-direction ,(let ((temp (aref regexp 2)))
						  (if (= temp 45)
						      32
						    temp)))
		     t))))
     (error "Can not optimize %s" regexp)))

  (defun my/regexp->char-match (form regexp backward)
    (if (stringp regexp)
	(condition-case nil
	    (cl-subst (if backward
			  '(preceding-char)
			'(following-char))
		      'direction
		      (cl-subst `(char-syntax direction) 'char-syntax-direction
				(cl-loop for r in (split-string regexp "\\\\|" t)
				   for (c . d) = (my/regexp-part->char-match r)
				   collect c into res
				   collect d into var
				   finally return
				     (if (cdr res)
					 (let ((sym1 (cl-gensym)))
					   (nconc
					    `(let ((,sym1 direction)))
					    (list
					     (cl-subst sym1 'direction
						       (if (cl-loop for dd in var
							      when dd return t)
							   (let ((sym2 (cl-gensym)))
							     (nconc
							      `(let ((,sym2 (char-syntax ,sym1))))
							      (list (cons 'or (cl-subst sym2 'char-syntax-direction res)))))
							 (cons 'or res))))))
				       (car res)))))
	  (error form))
      form)))

(defalias 'my/looking-at #'looking-at)

(cl-define-compiler-macro my/looking-at (&whole form regexp)
  (my/regexp->char-match form regexp nil))

(defalias 'my/looking-back #'looking-back)

(cl-define-compiler-macro my/looking-back (&whole form regexp limit &optional greedy)
  (or
   (and
    (not greedy)
    (numberp limit)
    (= 1 limit)
    (my/regexp->char-match form regexp t))
   form))

(defmacro defun-smarter-movement (original backward forward key &optional not-use-region no-repeat map look-string)
  "Define a new function which operates better than ORIGINAL. If
`looking-at' LOOK-STRING or `eobp' first go BACKWARD then
`call-interactively' ORIGINAL then go FORWARD. Bind the new
function to KEY in MAP. If NOT-USE-REGION then only do this when
`not' `use-region-p'. If NO-REPEAT then only when the new command
hasn't been repeated."
  (declare (indent 1))
  (let ((new-func (intern (concat "smarter-" (symbol-name original)))))
    `(progn
       (defun ,new-func ()
	 (interactive)
	 (if (and (or (my/looking-at ,(or look-string
				       "\\s)\\|\\s-\\|\n\\|\\s\""))
		      (eobp))
		  ,(if not-use-region
		       '(not (use-region-p))
		     t)
		  ,(if no-repeat
		       '(not (eq this-command last-command))
		     t))
	     (progn
	       ,backward
	       (call-interactively #',original)
	       ,forward)
	   (call-interactively #',original)))
       ,(when key
	  `(defkey ,key ,new-func ,map))
       #',new-func)))

(defun-smarter-movement capitalize-word (backward-word) nil "M-c" nil t)
(defun-smarter-movement downcase-word	(backward-word) nil "M-l" nil t)
(defun-smarter-movement upcase-word	(backward-word) nil "M-u" nil t)

(defun-smarter-movement mark-sexp
  (backward-sexp) (exchange-point-and-mark) "C-M-SPC" t)

(defun-smarter-movement kill-sexp
  (backward-sexp) nil "C-M-k" t)

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

(setq sentence-end-double-space nil
      scroll-conservatively 6)

(when user-mail-address
  (setq epa-file-encrypt-to user-mail-address))

(with-eval-after-load 'warnings
  (defvar warning-suppress-types)
  (add-to-list 'warning-suppress-types '(undo discard-info)))

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setf browse-url-browser-function '(("." . browse-url-default-browser)))

(with-eval-after-load 'eww
  (defvar eww-mode-map)
  (define-key eww-mode-map (kbd "<mouse-8>") 'eww-back-url)
  (define-key eww-mode-map (kbd "<mouse-9>") 'eww-forward-url)
  (define-key eww-mode-map (kbd "C-c b") 'eww-back-url)
  (define-key eww-mode-map (kbd "C-c f") 'eww-forward-url))

(with-eval-after-load 'shr
  (defvar shr-use-fonts)
  (setq shr-use-fonts nil))

(add-to-list 'browse-url-browser-function '("^file" . eww-browse-url))
(add-to-list 'browse-url-browser-function '("^https?://en\\.wiktionary\\.org/wiki/" . eww-browse-url))

(autoload 'ispell-get-word "ispell")

(defun wiktionary-word (word)
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (browse-url (format "https://en.wiktionary.org/wiki/%s" word)))

(global-set-key (kbd "M-#") #'wiktionary-word)

;;; The following two functions and their uses are based on
;;; https://fuco1.github.io/2017-05-06-Enhanced-beginning--and-end-of-buffer-in-special-mode-buffers-(dired-etc.).html
(defmacro my/special-beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE."
  (declare (indent 1))
  (let ((fname (intern (concat "my--" (symbol-name mode) "-beginning-of-buffer")))
	(mode-map (intern (concat (symbol-name mode) "-mode-map"))))
    `(progn
       (defun ,fname ()
         (interactive)
	 (let ((p (point)))
	   (goto-char (point-min))
	   (unless (eq last-command this-command)
	     ,@forms
	     (when (= p (point))
	       (goto-char (point-min))))))
       (define-key ,mode-map [remap beginning-of-buffer] (with-no-warnings #',fname)))))

(defmacro my/special-end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE."
  (declare (indent 1))
  (let ((fname (intern (concat "my--" (symbol-name mode) "-end-of-buffer")))
	(mode-map (intern (concat (symbol-name mode) "-mode-map"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
	   (unless (eq last-command this-command)
	     ,@forms
	     (when (= p (point))
	       (goto-char (point-max))))))
       (define-key ,mode-map [remap end-of-buffer] (with-no-warnings #',fname)))))

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(global-set-key (kbd "C-x C-d") #'dired-jump)
(global-set-key (kbd "C-x 4 C-d") #'dired-jump-other-window)

(defvar dired-bind-jump nil)
(defvar dired-x-hands-off-my-keys t)

(with-eval-after-load 'dired
  (defvar dired-dwim-target)
  (setq dired-dwim-target t)
  (require 'dired-x)
  (setq-default dired-omit-files-p t)
  (setq dired-listing-switches "-alhv")
  (defvar dired-omit-files)
  (setq dired-omit-files "^\\.\\|^#.#\\'\\|.~\\'")
  (with-no-warnings
    (define-key dired-mode-map (kbd "M-p") #'dired-up-directory))
  (defvar dired-mode-map)
  (define-key dired-mode-map (kbd "M-n") #'quit-window)
  (define-key dired-mode-map (kbd "/") #'dired-narrow-fuzzy)
  (with-no-warnings
    (define-key dired-mode-map (kbd "h") #'dired-omit-mode))
  (define-key dired-mode-map (kbd "e") #'read-only-mode)
  (my/special-beginning-of-buffer dired
    (while (not (ignore-errors (dired-get-filename)))
      (dired-next-line 1)))
  (my/special-end-of-buffer dired
    (dired-previous-line 1)))

(with-eval-after-load 'wdired
  (defvar wdired-allow-to-change-permissions)
  (setq wdired-allow-to-change-permissions t)
  (with-no-warnings
    (my/special-beginning-of-buffer wdired
      (while (not (ignore-errors (wdired-get-filename)))
	(wdired-next-line 1)))
    (my/special-end-of-buffer wdired
      (wdired-previous-line 1))))

(global-set-key (kbd "C-x p") #'proced)
(with-eval-after-load 'proced
  (defvar proced-filter)
  (setq-default proced-filter 'emacs))

(setq ibuffer-saved-filter-groups
      `(("default"
	 ("Root"
	  (filename . "^/sudo:root"))
	 ("Lisp" (or
		  (mode . lisp-mode)
		  (mode . slime-repl-mode)
		  (mode . slime-inspector-mode)
		  (name . "^\\*slime-\\(description\\|compilation\\|xref\\|error\\)\\*\\'")
		  (name . "^\\*sldb .*\\*\\'")
		  (filename . "^\\(file://\\)?/usr/local/doc/HyperSpec/")))
	 ("Clojure" (or
		  (mode . clojure-mode)
		  (mode . cider-repl-mode)
		  (mode . cider-inspector-mode)
		  (name . "^\\*cider-\\(description\\|compilation\\|xref\\|error\\)\\*\\'")
		  (name . "^\\*nrepl-*")))
	 ("Sage"
	  (or
	   (mode . sage-shell:sage-mode)
	   (mode . sage-shell-mode)
	   (mode . sage-shell:help-mode)
	   (mode . sage-mode)))
	 ("Python" (or
		    (mode . python-mode)
		    (mode . inferior-python-mode)
		    (name . "^\\*Python \\(Check\\|Doc\\)\\*\\'")))
	 ("Lua" (or
		 (mode . lua-mode)))
	 ("C" (or
	       (mode . c-mode)
	       (mode . c++-mode)
	       (mode . tags-table-mode)))
	 ("Perl" (or
		  (mode . perl-mode)))
	 ("Shell" (or
		   (mode . shell-mode)
		   (mode . term-mode)
		   (mode . sh-mode)
		   (derived-mode . conf-mode)
		   (derived-mode . shell-script-mode)
		   (mode . compilation-mode)
		   (name . "^\\*.* std\\(out\\|err\\)\\*\\'")
		   (name . "^\\*Shell Command Output\\*\\'")))
	 ("Assembly" (or
		      (mode . asm-mode)
		      (filename . "^/usr/local/doc/64-ia-32-architectures-software-developer-manual-325462\\.pdf\\'")))
	 ("Man" (or
		 (mode . Man-mode)
		 (mode . woman-mode)))
	 ("Data" (or
		  (filename . ".*\\.\\([ct]sv\\|dat\\)\\'")
		  (mode . hexl-mode)))
	 ("LaTeX" (or
		   (mode . latex-mode)
		   (mode . tex-shell)
		   (mode . TeX-output-mode)
		   (mode . plain-tex-mode)
		   (name . "^\\*\\(Latex Preview Pane \\(Welcome\\|Errors\\)\\|pdflatex-buffer\\)\\*\\'")))
	 ("Text" (or (derived-mode . text-mode)
		     (mode . nroff-mode)
		     (mode . change-log-mode)))
	 ("Books" (or
		   (mode . pdf-view-mode)
		   (mode . doc-view-mode)
		   (filename . ,(concat "^" (expand-file-name "~/Books")))))
	 ("Web" (or
		 (mode . w3m-mode)
		 (mode . eww-mode)))
	 ("Org" (or (derived-mode . org-mode)
		    (mode . org-agenda-mode)))
	 ("Dired" (or
		   (mode . dired-mode)
		   (mode . wdired-mode)
		   (name . "^\\*Dired log\\*\\'")
		   (mode . archive-mode)
		   (mode . tar-mode)
		   (mode . proced-mode)))
	 ("Git" (or (derived-mode . magit-mode)
		    (mode . magit-repolist-mode)
		    (filename . "\\.git\\(ignore\\|attributes\\)\\'")))
	 ("Diff" (or
		  (mode . ediff-mode)
		  (mode . diff-mode)
		  (name . "^\\*[Ee]?[Dd]iff.*\\*\\'")))
	 ("Gnus" (or (mode . message-mode)
		     (mode . bbdb-mode)
		     (mode . mail-mode)
		     (mode . gnus-group-mode)
		     (mode . gnus-summary-mode)
		     (mode . gnus-article-mode)
		     (name . "^\\.bbdb\\'")
		     (name . "^\\.newsrc-dribble\\'")
		     (name . "^\\*imap-log\\*\\'")
		     (mode . newsticker-mode)))
	 ("Emacs Lisp" (or
			(mode . emacs-lisp-mode)
			(mode . elisp-byte-code-mode)
			(mode . lisp-interaction-mode)
			(mode . inferior-emacs-lisp-mode)
			(mode . eshell-mode)))
	 ("Emacs" (or
		   (mode . help-mode)
		   (mode . Info-mode)
		   (mode . package-menu-mode)
		   (mode . finder-mode)
		   (mode . Custom-mode)
		   (name . "^\\*Backtrace\\*\\'")
		   (mode . apropos-mode)
		   (mode . ioccur-mode)
		   (mode . occur-mode)
		   (mode . xref--xref-buffer-mode)
		   (mode . grep-mode)
		   (mode . reb-mode)
		   (mode . calendar-mode)
		   (mode . calc-mode)
		   (mode . calc-trail-mode)
		   (mode . browse-kill-ring-mode)
		   (mode . process-menu-mode)
		   (mode . messages-buffer-mode))))))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-jump-offer-only-visible-buffers t)

(defun ibuffer-mark-and-kill-lines ()
  "Mark the current line, then kill all marked lines."
  (interactive)
  (if (eval-when-compile (< emacs-major-version 25))
      (ibuffer-mark-forward 1)
    (ibuffer-mark-forward nil nil 1))
  (ibuffer-do-kill-lines))

(with-eval-after-load 'ibuffer
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
	   " " filename)))
  (my/special-beginning-of-buffer ibuffer
    (ibuffer-forward-line 1))
  (my/special-end-of-buffer ibuffer
    (ibuffer-backward-line 1)))

(with-eval-after-load 'ibuf-ext
  (add-to-list 'ibuffer-never-show-predicates "^\\*slime-events\\*\\'")
  (add-to-list 'ibuffer-never-show-predicates "^\\*inferior-lisp\\*\\(<[0-9]>\\)?\\'")
  (add-to-list 'ibuffer-never-show-predicates "^\\*nrepl-server*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Compile-Log\\*\\'")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Completions\\*\\'")
  (add-to-list 'ibuffer-never-show-predicates "^\\*tramp/sudo root@localhost\\*\\'")
  (add-to-list 'ibuffer-never-show-predicates "^\\*magit-process:.*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*WoMan-Log\\*\\'"))

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

(defun window-dedicated->t (&optional window)
  (set-window-dedicated-p (or window (selected-window)) t))

(with-eval-after-load 're-builder
  (add-hook 'reb-mode-hook #'window-dedicated->t)
  (defvar reb-mode-map)
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit)
  (defvar reb-re-syntax)
  (setq reb-re-syntax 'string))

(with-eval-after-load 'string-edit
  (add-hook 'string-edit-mode-hook #'window-dedicated->t))

(setq search-whitespace-regexp ".*"
      isearch-allow-scroll t)

(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
	     (number-or-marker-p isearch-other-end)
	     (not mark-active)
	     (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))
(add-hook 'isearch-mode-end-hook #'endless/goto-match-beginning)

(with-eval-after-load 'imenu
  (defvar imenu-auto-rescan)
  (setq imenu-auto-rescan t))

(with-eval-after-load 'slime
  (slime-setup '(slime-fancy))
  (defvar slime-mode-map)
  (let ((doc-fun (lookup-key slime-mode-map (kbd "C-c C-d h"))))
    (defkeys (slime-mode (slime-repl-mode t))
      "C-c C-d DEL" 'doc-fun
      "C-c C-k" nil
      "M-?" nil)))

(setf slime-lisp-implementations
      (eval-when-compile
	(let (temp-lisp-implementations)
	  (let ((quicklisp-setup-file (expand-file-name "~/quicklisp/setup.lisp")))
	    (when (file-exists-p "/usr/bin/clisp")
	      (push '(clisp ("/usr/bin/clisp"))
		    temp-lisp-implementations))

	    (when (file-exists-p "/usr/bin/ecl")
	      (push '(ecl ("/usr/bin/ecl"))
		    temp-lisp-implementations))

	    (when (file-exists-p "~/sbcl/")
	      (push `(sbcl-git (,(expand-file-name "~/sbcl/run-sbcl.sh") "--dynamic-space-size" "3096" "--load" ,quicklisp-setup-file))
		    temp-lisp-implementations))

	    (when (file-exists-p "/usr/bin/sbcl")
	      (push '(sbcl-raw ("/usr/bin/sbcl"))
		    temp-lisp-implementations)

	      (when (file-exists-p quicklisp-setup-file)
		(push `(sbcl-debug ("/usr/bin/sbcl" "--dynamic-space-size" "3096" "--load" ,quicklisp-setup-file))
		      temp-lisp-implementations))

	      (when (file-exists-p "~/lisp/sbcl.core-graham")
		(let ((sbcl-core-graham (expand-file-name "~/lisp/sbcl.core-graham")))
		  (push `(sbcl-big ("/usr/bin/sbcl" "--core" ,sbcl-core-graham "--dynamic-space-size" "6192"))
			temp-lisp-implementations)
		  (push `(sbcl ("/usr/bin/sbcl" "--core" ,sbcl-core-graham "--dynamic-space-size" "3096"))
			temp-lisp-implementations)))))
	  temp-lisp-implementations)))

(setq common-lisp-hyperspec-root
      (eval-when-compile
	(if (file-exists-p "/usr/local/doc/HyperSpec/")
	    "file:///usr/local/doc/HyperSpec/"
	  "http://www.lispworks.com/reference/HyperSpec/")))

(add-to-list 'interpreter-mode-alist '("sbcl" . lisp-mode))

(setq
 backup-by-copying t			; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/saves"))		; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)			; use versioned backups

(setq auto-save-file-name-transforms
      (append
       auto-save-file-name-transforms
       '((".*" "~/.emacs.d/saves-auto/" t))))

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
                     nil		; End the loop.
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

(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(require 'ido-sort-mtime)
(ido-sort-mtime-mode 1)
(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode)
(global-set-key (kbd "M-x") #'smex)

(defmacro defset-function (function key &optional keymap)
  "Create a function which sets set-function to bind KEY to
FUNCTION either locally, or optionally in KEYMAP"
  (let ((function-symbol (intern (concat "set-" (symbol-name function)))))
    `(defun ,function-symbol ()
       ,(concat "Set the key " key " to `" (symbol-name function) "' "
		(if keymap
		    (concat "in `" (symbol-name keymap) "'")
		  "locally."))
       (interactive)
       ,(if keymap
	    `(define-key ,keymap (kbd ,key) #',function)
	  `(local-set-key (kbd ,key) #',function)))))

(defun pathname->~->home (arg)
  (interactive "P")
  (if (and (not arg) (my/looking-back "/" 1))
      (progn
	(when (my/looking-at ".")
	  (kill-line))
	(insert "~/"))
    (let (current-prefix-arg)
      (call-interactively #'self-insert-command))))

(defun pathname->/->root ()
  (interactive)
  (if (my/looking-back "/" 1)
      (insert "//")
    (call-interactively #'self-insert-command)))

(defset-function pathname->~->home "~" ido-completion-map)
(defset-function pathname->/->root "/" ido-completion-map)

(add-hook 'ido-setup-hook #'set-pathname->~->home)
(add-hook 'ido-setup-hook #'set-pathname->/->root)
(add-hook 'ido-setup-hook (lambda ()
			    (define-key ido-completion-map (kbd "C-c") nil)
			    (define-key ido-completion-map (kbd "M-c") #'ido-toggle-case)))

(add-to-list 'ido-ignore-files "^\\.smex-items\\'")
(add-to-list 'ido-ignore-files "^\\.slime-history\\.eld\\'")
(add-to-list 'ido-ignore-files "^\\.python_history\\'")
(add-to-list 'ido-ignore-files "^\\.w3m/\\'")
(add-to-list 'ido-ignore-files "^\\.histfile\\'")
(add-to-list 'ido-ignore-files "^ido\\.last\\'")
(add-to-list 'ido-ignore-files "^\\.newsrc\\(\\.eld\\)?\\'")

(setq ido-file-extensions-order '(".el" ".org" ".lisp" ".clj" ".py" ".tex" ".pdf" t))

(eval-when-compile
  (defvar ido-temp-list))

(defun my/resort-ido-by-extension ()
  "Sort files with the same name according to
`ido-file-extensions-order' when they are next to each other
within the sorted list already. This is useful in conjunction
with mtime sorted lists."
  (let (last-cons)
    (cl-do ((itl ido-temp-list (cdr itl)))
	   ((null itl))
      (let* ((name (file-name-sans-extension (car itl)))
	     (sub
	      (cl-loop
		 with count = 0
		 for el in (cdr itl)
		 if (string= (file-name-sans-extension el) name)
		 collect el into ret
		 and do (incf count)
		 else do (return (cons ret count))
		 finally (return (cons ret count)))))
	(when (> (cdr sub) 0)
	  (let ((tail (nthcdr (1+ (cdr sub)) itl))
		(sorted (sort (cons (car itl) (car sub)) #'ido-file-extension-lessp)))
	    (rplacd (last sorted) tail)
	    (if last-cons
		(rplacd last-cons sorted)
	      (setq ido-temp-list sorted)
	      (setq itl ido-temp-list))))
	(setq itl (nthcdr (cdr sub) itl))
	(setq last-cons itl)))))

(add-hook 'ido-make-file-list-hook #'my/resort-ido-by-extension t)
(add-hook 'ido-make-dir-list-hook #'my/resort-ido-by-extension t)

(defvar my/ido-file-push-to-end '(".log" ".out"))

(defun my/ido-push-extensions-to-end ()
  "Push files with extensions in `my/ido-file-push-to-end' to the
  end of the list."
  (cl-loop for el in ido-temp-list
     if (member (file-name-extension el t) my/ido-file-push-to-end)
     collect el into pushed
     else collect el into regular
     finally (setq ido-temp-list (nconc regular pushed))))

(add-hook 'ido-make-file-list-hook #'my/ido-push-extensions-to-end t)
(add-hook 'ido-make-dir-list-hook #'my/ido-push-extensions-to-end t)

(setq ido-use-filename-at-point 'guess)

(defvar my/user-has-root-privileges 'ask)

(defun my/user-has-root-privileges ()
  (if (eq my/user-has-root-privileges 'ask)
      (setq my/user-has-root-privileges
	    (or
	     (with-temp-buffer
	       (call-process "id" nil (current-buffer) nil "-nG")
	       (cl-loop for group in (split-string (buffer-string))
		  when (string= "wheel" group)
		  return t))
	     (yes-or-no-p "Do you have root privileges? ")))
    my/user-has-root-privileges))

(defadvice ido-find-file (after find-file-sudo compile activate)
  "Find file as root if necessary."
  (when (and buffer-file-name
	     (not (file-writable-p buffer-file-name))
	     (file-directory-p default-directory)
	     (my/user-has-root-privileges))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun copy-current-file-path (arg)
  "Copy the current file path to the `kill-ring'. With C-u prefix
then copy without directory."
  (interactive "P")
  (let ((temp
	 (if (string-match "/sudo:root@localhost:" buffer-file-name)
	     (substring buffer-file-name 21)
	   buffer-file-name)))
    (if (consp arg)
	(kill-new (file-name-nondirectory temp))
      (kill-new temp))))

(global-set-key (kbd "C-c C-f") #'copy-current-file-path)

(global-set-key (kbd "M-/") #'hippie-expand)

(defun he-closing-delimiter-fix (&rest _)
  "Remove extra closing delimiter when expanding line."
  (let (end)
    (when (and (= (char-syntax (preceding-char)) ?\))
	       (= (preceding-char)
		  (save-excursion
		    (skip-syntax-forward "-")
		    (setq end (1- (point)))
		    (following-char))))
      (delete-char -1)
      (if (region-modifiable-p (point) end)
	  (delete-region (point) end)
	(goto-char end))
      (forward-char))))

(with-eval-after-load 'hippie-exp
  (defvar hippie-expand-try-functions-list)
  (setq hippie-expand-try-functions-list
	'(try-expand-all-abbrevs
	  try-expand-dabbrev-visible
	  try-expand-dabbrev
	  try-expand-list
	  try-expand-line
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol
	  try-complete-file-name-partially
	  try-complete-file-name))

  (with-no-warnings
    (advice-add #'he-substitute-string :after #'he-closing-delimiter-fix)))

(defun my/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) 1048576))

(setq dabbrev-friend-buffer-function #'my/dabbrev-friend-buffer)

(setq tab-always-indent 'complete)

(defun adjust-tab-width ()
  "Cycle `tab-width' through the values 2 4 and 8. This function
can be repeated by pressing the last key used to call the
function."
  (interactive)
  (setq tab-width (mod (* tab-width 2) 16))
  (when (= 0 tab-width)
    (setq tab-width 2))
  (message (format "Tab width set to %d" tab-width))
  (last-key-repeating))

(global-set-key (kbd "C-c TAB") #'adjust-tab-width)

(defun indent-tabs-mode ()
  "Toggle whether `indent-tabs-mode' is true."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message (format "Indent-tabs-mode set to %s" indent-tabs-mode))
  (last-key-repeating))

(global-set-key (kbd "C-c C-<tab>") #'indent-tabs-mode)

(require 'projectile)

(defun projectile-ignored-project-function (file)
  (or (file-remote-p file)
      (string-match "\\(^/tmp/\\|/.cache/\\)" file)))

(setq projectile-switch-project-action #'projectile-commander
      projectile-sort-order 'modification-time
      projectile-ignored-project-function #'projectile-ignored-project-function)

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

(autoload 'slime-repl-eval-string "slime")

(defun projectile-jack-in ()
  "Attempt to jack-in to the project."
  (delete-other-windows)
  (let ((root (projectile-project-root)))
    (cond ((string-match (expand-file-name "~/quicklisp/local-projects/\\(.*\\)/\\'")
			 root)
	   (let ((cl-package (match-string 1 root)))
	     (let ((files (directory-files root t "^[^.].*\\.lisp\\'" t)))
	       (if files
		   (dolist (f (setq files (sort files
						(lambda (a b)
						  (cond
						    ((file-remote-p a) t)
						    ((file-remote-p b) nil)
						    (t (file-newer-than-file-p b a)))))))
		     (find-file f))
		 (dired root)))
	     (if cl-package
		 (progn
		   (require 'slime)
		   (slime-repl-eval-string
		    (format "(when (ql:quickload :%s :silent t) (in-package :%s))"
			    cl-package
			    cl-package))
		   (slime-switch-to-output-buffer)
		   (cd root))
		 (projectile-vc root))))
	  ((string-match (expand-file-name "~/clojure/\\(.*\\)/\\'"))
	   (let ((files (directory-files root t "^[^.].*\\.clj\\'" t)))
	     (if files
		 (dolist (f (setq files (sort files
					      (lambda (a b)
						(cond
						  ((file-remote-p a) t)
						  ((file-remote-p b) nil)
						  (t (file-newer-than-file-p b a)))))))
		   (find-file f))
		 (dired root))
	     (require 'cider)
	     (call-interactively #'cider-jack-in)))
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

(setq projectile-mode-line
      '(:eval
	(if (file-remote-p default-directory)
	    " [--]"
	  (format " [%s]"
		  (projectile-project-name)))))

(autoload 'dired-read-shell-command "dired-aux")

(defun projectile-dired-command-files ()
  (interactive)
  (let ((files (projectile-current-project-files)))
    (with-no-warnings
      (dired-do-shell-command
       (dired-read-shell-command "! on %s: " current-prefix-arg files)
       current-prefix-arg
       files))))

(define-key projectile-mode-map (kbd "C-c p x !") #'projectile-dired-command-files)

(projectile-mode)

(defkey "H-p" projectile-command-map)

(defun truncate-lines->t ()
  (interactive)
  (unless truncate-lines
    (setq truncate-lines t)))

(add-hook 'dired-mode-hook #'truncate-lines->t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(load "~/.emacs.d/lisp/saveplace-patch" nil t)
(if (eval-when-compile (< emacs-major-version 25))
    (setq-default save-place t)
  (save-place-mode 1))

(defun undo-tree-save-history-ignore-file (orig &rest args)
  "Ignore files matching the regex, otherwise save history"
  (unless (string-match "\\(\\.gpg\\'\\|^/tmp/\\|/.emacs.d/elpa/\\|/.git/\\)" buffer-file-name)
    (apply orig args)))

(require 'undo-tree)
(global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (define-key undo-tree-visualizer-mode-map (kbd "RET") #'undo-tree-visualizer-quit)
  (unless (string= user-login-name "root")
    (setq undo-tree-auto-save-history t))
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/")))
  (advice-add 'undo-tree-save-history :around #'undo-tree-save-history-ignore-file)
  (defkeys undo-tree
    "<S-mouse-5>" undo-tree-redo
    "<S-mouse-4>" undo-tree-undo))

(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t)

(with-eval-after-load 'x86-lookup
  (let ((location "/usr/local/doc/64-ia-32-architectures-software-developer-manual-325462.pdf"))
    (when (file-exists-p location)
      (defvar x86-lookup-pdf)
      (setq x86-lookup-pdf location))))

(defun eval-and-replace (x)
  "Replace the preceding sexp with its value."
  (interactive "p")
  (let ((y x))
    (if (> y 0)
	(let ((s (if (my/looking-back " " 1) 0 -1)))
	  (while (> y 0)
	    (setf s (+ s (if (= 0 (skip-chars-backward " \n\t")) 1 0)))
	    (kill-sexp -1)
	    (condition-case nil
		(prin1 (eval (read (current-kill 0)) t)
		       (current-buffer))
	      (user-error "Invalid expression")
	      (insert (current-kill 0)))
	    (forward-sexp -1)
	    (setq y (1- y)))
	  (forward-sexp (- x s)))
      (if (< y 0)
	  (let ((pointer-position (point)))
	    (while (< y 0)
	      (kill-sexp 1)
	      (condition-case nil
		  (prin1 (eval (read (current-kill 0)) t)
			 (current-buffer))
		(user-error "Invalid expression")
		(insert (current-kill 0)))
	      (skip-chars-forward " \n\t")
	      (setq y (1+ y)))
	    (goto-char pointer-position))
	(user-error "Okay?")))))

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

(define-prefix-command 'my/window-map)

(defkey "C-r" rotate-windows my/window)

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

(defkey "C-x" toggle-window-split my/window)

(defun activate-word-column-region ()
  "Look at the symbol at point, search backward and place the point before a
symbol, and search forward and place the mark after a symbol such that all
lines have identical symbols at identical goal columns as the symbol at point."
  (interactive)
  (let (upper-pt lower-pt (next-line-add-newlines t))
    (save-excursion
      (let ((target (format "%s" (symbol-at-point))))
	(while (my/looking-back "\\sw\\|\\s_" 1)
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
	(while (my/looking-at "\\sw\\|\\s_")
	  (forward-char 1))
	(push-mark nil nil t)
	(goto-char upper-pt)
	(while (my/looking-back "\\sw\\|\\s_" 1)
	  (backward-char 1)))))
  (rectangle-mark-mode))

;;; To facilitate functions defined in init which change behavior
;;; based on smartparens being active or not
(defvar smartparens-mode nil)

(defun insert-splice (arg)
  "Insert ,@ into the buffer, with a space before and parens
after as appropriate. With \\[universal-argument] or when point
is not at whitespace splice the containing sexp instead. For use
in lisp programming languages."
  (interactive "*P")
  (let ((move (or arg (not (thing-at-point 'whitespace))))
	(chars 2))
    (save-excursion
      (when move
	(do () ((my/looking-back "(" 1) (backward-char))
	  (forward-thing 'sexp -1)))
      (when (not (my/looking-back "\\s-\\|\n" 1))
	(insert " ")
	(incf chars))
      (insert ",@")
      (unless (my/looking-at "(")
	(insert "()")
	(backward-char)
	(incf chars))
      (let (smartparens-mode)
	(run-hooks 'post-self-insert-hook))
      (when paredit-mode
	(paredit-reindent-defun)))
    (unless move
      (forward-char chars))))

(autoload 'slime-switch-to-output-buffer "slime")
(autoload 'sh-show-shell "sh-script" "Pop the shell interaction buffer." t)
(autoload 'elpy-shell-switch-to-shell "elpy" "Switch to inferior Python process buffer.")

(defun my/check-for-process (process)
  "Return a list, the first element is t if PROCESS exists,
second if PROCESS does not belong to the `current-buffer'."
  (let ((process (get-process process)))
    (list process
	  (not
	   (eq process
	       (get-buffer-process
		(current-buffer)))))))

(defmacro my/do-next-cond (&rest clauses)
  "Like `cond' except conditions evaluate to a list of two
elements. A clause is only eligible if the first element in the
list is t. The second eligible clause with the second condition t
is executed. Clause tests may be executed twice. A condition of a
lone t is treated specially."
  (let ((do-next (cl-gensym))
	(top (cl-gensym))
	(second-time (cl-gensym))
	(otherwise (cdr (assoc t clauses)))
	(temp (cl-gensym)))
    `(let ((,do-next nil)
	   (,second-time nil))
       (cl-tagbody
	  ,top
	  (cond
	    ,@(cl-loop for clause in clauses
		 unless (eq t (car clause)) collect
		   (cons
		    `(let ((,temp ,(car clause)))
		       (and (car ,temp)
			    (if (cadr ,temp)
				,do-next
			      (setq ,do-next t)
			      nil)))
		    (cdr clause)))
	    (t
	     (if ,second-time
		 (progn ,@otherwise)
	       (setq ,second-time t
		     ,do-next t)
	       (go ,top))))))))

(defun smart-switch-to-output-buffer ()
  "Attempt to switch to an output buffer, cycling in order
through lisp, clojure, shell, multi-term, sage, python, then
ielm. Make `last-key-repeating'."
  (interactive)
  (my/do-next-cond
   ((list (get-process "inferior-lisp")
	  (not (eq major-mode
		   'slime-repl-mode)))
    (slime-switch-to-output-buffer))
   ((my/check-for-process "nrepl-server")
    (switch-to-buffer
     (with-no-warnings
       (dolist (buf (buffer-list) buf)
	 (with-current-buffer buf
	   (when (derived-mode-p 'cider-repl-mode)
	     (return buf)))))))
   ((my/check-for-process "shell")
    (sh-show-shell))
   ((with-no-warnings
      (and (featurep 'multi-term)
	   (list
	    multi-term-buffer-list
	    (or
	     (rest multi-term-buffer-list)
	     (not (eq (current-buffer)
		      (car multi-term-buffer-list)))))))
    (multi-term-next))
   ((my/check-for-process "Sage")
    (with-no-warnings
      (sage-shell-edit:pop-to-process-buffer)))
   ((my/check-for-process "Python")
    (elpy-shell-switch-to-shell))
   ((my/check-for-process "ielm")
    (ielm))
   (t
    (user-error "No available process to switch to.")))
  (last-key-repeating))

(defun my/override-xref-and-reuse-window (buffer-or-name _)
  "Match when there is only one frame with two or fewer windows
and one of them is dedicated to displaying the xref buffer."
  (ignore buffer-or-name)
  (and (featurep 'xref)
       (not (cdr (frame-list)))
       (not (cddr (window-list)))
       (cl-loop for w in (window-list)
	  when (string= (buffer-name (window-buffer w))
			(with-no-warnings xref-buffer-name))
	  return (window-dedicated-p w))))

(with-eval-after-load 'xref
  (add-to-list 'display-buffer-alist (cons #'my/override-xref-and-reuse-window
					   (cons #'display-buffer-same-window
						 (cons
						  (cons 'inhibit-same-window nil)
						  nil)))))

(defun buffer-has-process-p (buffer-or-name &rest _)
  "True if BUFFER-OR-NAME has a process associated with it or has
`major-mode' eq to `slime-repl-mode'."
  (or (get-buffer-process buffer-or-name)
      (with-current-buffer buffer-or-name
	(eq major-mode 'slime-repl-mode))))

(defun buffer-derives-from-prog (buffer-or-name &rest _)
  "True if the `major-mode' of BUFFER-OR-NAME derives from
  `prog-mode'."
  (with-current-buffer buffer-or-name
    (derived-mode-p 'prog-mode)))

(defun my/keep-right-and-inhibit-same (buffer-or-name _)
  (with-current-buffer buffer-or-name
    (or
     (derived-mode-p
      'magit-status-mode
      'Man-mode
      'woman-mode
      'help-mode)
     (and
      (featurep 'xref)
      (string= (with-no-warnings xref-buffer-name) (buffer-name))))))

(cl-defun display-buffer-rightmost-window (buffer alist)
  "For use with `display-buffer-alist'. Display BUFFER in the
rightmost window of the current frame. Call `split-window-right'
first if there is only one window. Pop to window if BUFFER is
already displayed. If accept-current-with-process has true
association in ALIST then reuse current window if it is
displaying a buffer with a process associated with it according
to `buffer-has-process-p'."
  (window--display-buffer
   buffer
   (or
    (cl-loop for win in (window-list)
       when (eq buffer (window-buffer win))
       return win)
    (and (cdr (assq 'accept-current-with-process alist))
	 (buffer-has-process-p (window-buffer))
	 (selected-window))
    (unless (cdr (window-list))
      (split-window-right)
      nil)
    (let ((inhibit-same (cdr (assq 'inhibit-same-window alist)))
	  (current-window (selected-window)))
      (cl-do ((win
	       (cl-do ((win current-window win2)
		       (win2 current-window (window-right win2)))
		      ((null win2) win))
	       (or (window-left win)
		   (when (cdr (assq 'allow-no-window alist))
		     (cl-return-from display-buffer-rightmost-window))
		   (if inhibit-same
		       (cl-loop for w in (window-list)
			  when (not (eq current-window w))
			  return w)
		     current-window))))
	     ((not (or
		    (not (window-live-p win))
		    (window-minibuffer-p win)
		    (window-dedicated-p win)
		    (and inhibit-same
			 (eq win current-window))))
	      win))))
   'reuse alist))

(cl-defun display-buffer-leftmost-window (buffer alist)
  "For use with `display-buffer-alist'. Display BUFFER in the
leftmost window of the current frame. Call `split-window-right'
first if there is only one window. Pop to window if BUFFER is
already displayed. If accept-current-with-process has true
association in ALIST then reuse current window if it is
displaying a buffer with a process associated with it according
to `buffer-has-process-p'."
  (window--display-buffer
   buffer
   (or
    (cl-loop for win in (window-list)
       when (eq buffer (window-buffer win))
       return win)
    (and (cdr (assq 'accept-current-with-process alist))
	 (buffer-has-process-p (window-buffer))
	 (selected-window))
    (unless (cdr (window-list))
      (split-window-right)
      (other-window 1)
      nil)
    (let ((inhibit-same (cdr (assq 'inhibit-same-window alist)))
	  (current-window (selected-window)))
      (cl-do ((win
	       (cl-do ((win current-window win2)
		       (win2 current-window (window-left win2)))
		      ((null win2) win))
	       (or (window-right win)
		   (when (cdr (assq 'allow-no-window alist))
		     (cl-return-from display-buffer-leftmost-window))
		   (if inhibit-same
		       (cl-loop for w in (window-list)
			  when (not (eq current-window w))
			  return w)
		     current-window))))
	     ((not (or
		    (not (window-live-p win))
		    (window-minibuffer-p win)
		    (window-dedicated-p win)
		    (and inhibit-same
			 (eq win current-window))))
	      win))))
   'reuse alist))

(add-to-list 'display-buffer-alist
	     (cons #'buffer-has-process-p
		   (cons #'display-buffer-rightmost-window
			 (cons
			  (cons 'accept-current-with-process t)
			  nil))))

(add-to-list 'display-buffer-alist
	     (cons #'buffer-derives-from-prog
		   (cons #'display-buffer-leftmost-window
			 (cons
			  (cons 'inhibit-same-window t)
			  nil))))

(add-to-list 'display-buffer-alist
	     (cons #'my/keep-right-and-inhibit-same
		   (cons #'display-buffer-rightmost-window
			 (cons
			  (cons 'inhibit-same-window t)
			  nil))))

(defun magit-auto-create-gitattributes&ignore (orig &rest args)
  "Create .gitattributes and .gitignore files if they do not
already exist and the content of the files can be predicted based
on the location of the new git directory."
  (let* ((dir-name (expand-file-name (first args)))
	 (attributes-file (concat dir-name ".gitattributes"))
	 (ignore-file (concat dir-name ".gitignore")))
    (unless (file-exists-p attributes-file)
      (cond
	((string-match "/quicklisp/local-projects/" attributes-file)
	 (with-temp-file attributes-file
	   (insert "*.lisp diff=lisp\n")))))
    (unless (file-exists-p ignore-file)
      (cond
	((string-match "/quicklisp/local-projects/" ignore-file)
	 (with-temp-file ignore-file
	   (insert "*.fasl\n"))))))
  (apply orig args))

(defvar magit-no-message '("Turning on magit-auto-revert-mode..."))

(autoload 'magit-ido-completing-read "magit-utils" "Ido-based `completing-read' almost-replacement.")
(with-eval-after-load 'magit
  (advice-add 'magit-init :around #'magit-auto-create-gitattributes&ignore)
  (defvar magit-completing-read-function)
  (setq magit-completing-read-function #'magit-ido-completing-read)
  (defvar magit-status-mode-map)
  (my/special-beginning-of-buffer magit-status
    (while (looking-at "\\(Head\\|Push\\):")
      (magit-section-forward)))
  (my/special-end-of-buffer magit-status
    (magit-section-backward))
  (with-eval-after-load 'magit-repos
    (defvar magit-repository-directories)
    (add-to-list 'magit-repository-directories (cons user-emacs-directory 0))
    (when (eval-when-compile (file-exists-p #1="~/quicklisp/local-projects/"))
      (add-to-list 'magit-repository-directories (cons #1# 1)))
    (when (eval-when-compile (file-exists-p #1="~/dotfiles/"))
      (add-to-list 'magit-repository-directories (cons #1# 0)))
    (when (eval-when-compile (file-exists-p #1="~/emacs/"))
      (add-to-list 'magit-repository-directories (cons #1# 0)))
    (when (eval-when-compile (file-exists-p #1="~/sbcl/"))
      (add-to-list 'magit-repository-directories (cons #1# 0)))))

(defun my/git-commit-setup ()
  (when (string-match
	 (eval-when-compile
	   (concat "^" (regexp-quote
			(expand-file-name "~/emacs/"))))
	 default-directory)
    (setq-local fill-column 77)))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'my/git-commit-setup))

(setq ioccur-buffer-completion-use-ido t)

(when (eval-when-compile (< emacs-major-version 26))
  (with-eval-after-load 'message
    (defvar message-use-idna)
    (setq message-use-idna 'ask)))

(with-eval-after-load 'tls
  (defvar tls-program)
  (setq tls-program
	(mapcan
	 (lambda (x)
	   (unless
	       (string-match "--\\(insecure\\|protocols ssl3\\)" x)
	     (list x)))
	 tls-program)))

(with-eval-after-load 'gnutls
  (when (< gnutls-min-prime-bits 4096)
    (setq gnutls-min-prime-bits 4096)))

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

(with-eval-after-load 'ioccur
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

(defkey "C-u" revert-this-buffer my/window)

(defun really-desktop-clear ()
  (interactive)
  (when (yes-or-no-p "Really clear the desktop? ")
    (desktop-clear)))

(defkey "k" really-desktop-clear my/window)

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (when (yes-or-no-p "Really kill all other buffers? ")
    (dolist (buffer (buffer-list))
      (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
	(kill-buffer buffer)))))

(defkey "C-k" kill-other-buffers my/window)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (user-error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (user-error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defkey "C-f" rename-current-buffer-file my/window)

(defun ediff-current-buffer-file-with-auto-save ()
  "Call `ediff' on the current buffer file with its auto save file."
  (interactive)
  (let ((auto-save-file (make-auto-save-file-name)))
    (if (file-exists-p auto-save-file)
	(ediff buffer-file-name auto-save-file)
      (user-error "Current buffer has no auto-save file"))))

(defun message-current-time ()
  "Print the current time in the mini-buffer."
  (interactive)
  (message (format-time-string "%T %A %B %e, %Y")))

(defkey "C-w" message-current-time my/window)

(with-eval-after-load 'alert
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
		     (alert (concat time " minutes timer has finished at "
				    (format-time-string "%T"))
			    :category 'egg-timer))))
    (message (concat "Timer set for " time " minutes starting at "
		     (format-time-string "%T")))))

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
	((user-error "Too many windows"))))))

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
	   '((user-error "Need two windows"))))))

(defun find-user-init-file ()
  "Edit the `user-init-file'"
  (interactive)
  (cl-loop for win in (window-list)
     when (string= user-init-file
		   (buffer-file-name (window-buffer win)))
     return (pop-to-buffer (window-buffer win))
     finally (find-file user-init-file)))

(defkey "C-i" find-user-init-file my/window)

(defun find-user-shell-rc ()
  "Edit the users shell rc file."
  (interactive)
  (let ((inhibit-message t))
    (find-file
     (concat
      "~/"
      (if (string-match "zsh\\'" (getenv "SHELL"))
	  ".zshrc"
	".bashrc")))))

(defkey "C-z" find-user-shell-rc my/window)

(defun find-stumpwm-file ()
  (interactive)
  (find-file "~/dotfiles/stumpwm/.config/stumpwm/config"))

(defkey "C-t" find-stumpwm-file my/window)

(defun pop-to-scratch ()
  "Pop to the scratch buffer."
  (interactive)
  (pop-to-buffer "*scratch*"))

(defkey "C-;" pop-to-scratch my/window)

(defun find-first-agenda-file (arg)
  "Edit the first file in `org-agenda-files'. With `prefix-arg'
open last agenda file."
  (interactive "P")
  (require 'org)
  (defvar org-agenda-files)
  (defun find-first-agenda-file (arg)
    "Edit the first file in `org-agenda-files'. With `prefix-arg'
open last agenda file."
    (interactive "P")
    (if (and org-agenda-files (listp org-agenda-files))
	(find-file
	 (first
	  (if (consp arg)
	      (last org-agenda-files)
	    org-agenda-files)))
      (user-error "No agenda files")))
  (find-first-agenda-file arg))

(defkey "C-a" find-first-agenda-file my/window)

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

(defun other-window-quit (arg)
  "Run `quit-window' in the window selected by `ace-window'.
With \\[universal-argument] switch to that window after quitting."
  (interactive "P")
  (let ((current (selected-window))
	(target (ace-window 1)))
    (quit-window nil target)
    (select-window
     (if (and arg (window-live-p target))
	 target
       current))))

(global-set-key (kbd "H-q") #'other-window-quit)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not 1, move forward ARG - 1 lines first. If point
reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (when (/= arg 1)
    (let (line-move-visual)
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defkey "C-a" smarter-move-beginning-of-line)

(defun back-to-end-of-sexp (arg)
  "Move backwards to the end of the sexp. Always stay on the
current line. Repeat ARG times."
  (dotimes (_ arg)
    (if (bolp)
	(move-end-of-line 1)
      (let ((eolp (eolp)))
	(backward-char 1)
	(while (my/looking-at "\\s-")
	  (backward-char 1))
	(if (and eolp (my/looking-back "\\s)" 1))
	    (progn
	      (while (my/looking-at "\\s)")
		(backward-char 1))
	      (forward-char 1))
	  (while (not (my/looking-at "\\s(\\|\\s)\\|\n"))
	    (backward-char 1)))
	(when (my/looking-at "\\s-\\|\n")
	  (forward-char 1))))))

(defun smarter-move-end-of-line (arg)
  "Like `move-end-of-line' except when repeated or at eol call
`back-to-end-of-sexp' instead."
  (interactive "^p")
  (if (or (eq last-command this-command)
	  (eolp))
      (back-to-end-of-sexp arg)
    (move-end-of-line arg)))

(defkey "C-e" smarter-move-end-of-line)

(defun count-lines-page-or-maybe-region ()
  "If region is active call `count-words-region'. Otherwise call
`count-lines-page'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'count-words-region)
    (count-lines-page)))

(defun my/kill-this-buffer (arg)
  "When the current buffer is visiting a file and the buffer has
been modified, call `save-buffer' unless ARG. Then call
`kill-this-buffer' or `bury-buffer' if in the scratch buffer."
  (interactive "P")
  (when (and (not arg)
	     (buffer-file-name)
	     (buffer-modified-p))
    (save-buffer))
  (if (string= (buffer-name) "*scratch*")
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (with-no-warnings
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
	  ((use-region-p)
	   (narrow-to-region (region-beginning)
			     (region-end)))
	  ((derived-mode-p 'org-mode)
	   ;; `org-edit-src-code' is not a real narrowing
	   ;; command. Remove this first conditional if
	   ;; you don't want it.
	   (cond ((ignore-errors (org-edit-src-code) t)
		  (delete-other-windows))
		 ((ignore-errors (org-narrow-to-block) t))
		 (t (org-narrow-to-subtree))))
	  ((derived-mode-p 'latex-mode)
	   (LaTeX-narrow-to-environment))
	  (t (narrow-to-defun)))))

(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(defun my/avy-action-mark (pt)
  "See `avy-action-mark'"
  (goto-char pt)
  (smarter-mark-sexp))

(defun my/avy-action-kill-move (pt)
  "See `avy-action-kill-move'"
  (my/avy-action-mark pt)
  (kill-region pt (mark) t))

(defun my/avy-action-kill-stay (pt)
  "See `avy-action-kill-stay'"
  (save-excursion
    (my/avy-action-kill-move pt)
    (when (and (bolp) (eolp))
      (delete-blank-lines))))

(with-eval-after-load 'avy
  (defvar avy-keys)
  (setq avy-keys (list ?h ?t ?n ?s ?a ?o ?e ?u))
  (defvar avy-style)
  (setq avy-style 'de-bruijn)
  (defvar avy-case-fold-search)
  (setq avy-case-fold-search nil)
  (defvar avy-dispatch-alist)
  (setq avy-dispatch-alist
	'((?k . my/avy-action-kill-stay)
	  (? . my/avy-action-kill-move)
	  (?m . my/avy-action-mark)
	  (?w . avy-action-copy)
	  (?c . avy-action-ispell)))
  (defvar aw-keys)
  (setq aw-keys avy-keys))

(define-prefix-command 'my/avy-passthrough-map)
(defkey "C-o" my/avy-passthrough-map)

(defun my/avy-goto-char ()
  (interactive)
  (let ((defining-kbd-macro nil)
	(executing-kbd-macro nil))
    (avy-goto-char
     (if (integerp last-command-event)
	 last-input-event
       (get last-input-event 'ascii-character))
     current-prefix-arg)))

(defkey [t] my/avy-goto-char my/avy-passthrough)

(defun smarter-open-line (arg)
  (interactive "*p")
  (open-line arg)
  (save-excursion
    (forward-line)
    (indent-according-to-mode))
  (when paredit-mode
    (paredit-reindent-defun))
  (last-key-repeating))

(defkey "C-o" smarter-open-line my/avy-passthrough)

(defmacro passthrough-move-key (key package &optional package-map no-defvar)
  "Create an `eval-after-load' for PACKAGE to move KEY in
PACKAGE-MAP to KEY on the prefix map at KEY in the
`current-global-map' then remove the former keybinding. If
PACKAGE-MAP is not specified assume it has the form
PACKAGE-mode-map. Insert a `defvar' for PACKAGE-MAP unless
NO-DEFVAR in order to pacify the byte compiler."
  (let ((package-map
	 (or package-map
	     (intern
	      (concat (symbol-name package)
		      "-mode-map"))))
	(key (kbd key)))
    `(with-eval-after-load ',package
       ,(unless no-defvar `(defvar ,package-map))
       (define-key ,package-map
	   (vector 'remap
		   (lookup-key
		    (lookup-key (current-global-map)
				,key)
		    ,key))
	 (lookup-key ,package-map ,key))
       (define-key ,package-map ,key nil))))

(defmacro passthrough-move-keys (key &rest args)
  "Call `passthrough-move-key' with KEY as the argument. ARGS is
either a symbol denoting the package or a list of the remaining
arguments for each call with the package listed first."
  (declare (indent 1))
  `(progn
     ,@(cl-loop
	  for a in args collect
	    `(passthrough-move-key ,key ,@(my/ensure-cons a)))))

(passthrough-move-keys "C-o"
  ibuffer
  dired
  wdired
  (rect rectangle-mark-mode-map)
  grep
  (xref xref--xref-buffer-mode-map)
  (compile compilation-mode-map))

(defun close-line (arg)
  (interactive "*p")
  (dotimes (_ arg)
    (forward-line)
    (join-line))
  (when paredit-mode
    (paredit-reindent-defun)))

(defun open-previous-line (arg)
  (interactive "*p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode)
  (last-key-repeating))

(defun open-next-line (arg)
  (interactive "*p")
  (end-of-line)
  (open-line arg)
  (forward-line)
  (indent-according-to-mode)
  (last-key-repeating))

(defun my/xor (a b)
  "Logical xor of A and B"
  (if a (if (not b) a) b))

(defvar non-native-C-c-C-z-first nil
  "Whether `C-c-C-z-or-smart-switch-to-output-buffer' should call
  the native C-c C-z function without \\[universal-argument].")

(defun C-c-C-z-or-smart-switch-to-output-buffer (arg)
  "With plain \\[universal-argument] call `smart-switch-to-output-buffer',
otherwise call whatever is bound to C-c C-z ARG times. Reverse if
`non-native-C-c-C-z-first'. Two \\[universal-argument]'s will
change the value of `non-native-C-c-C-z-first'."
  (interactive "P")
  (if (and (consp arg) (= 16 (car arg)))
      (progn
	(setq non-native-C-c-C-z-first (not non-native-C-c-C-z-first))
	(message
	 (format "Switching mode set to%snative first."
		 (if non-native-C-c-C-z-first
		     " non-"
		   " "))))
    (let ((non-native (my/xor (consp arg) non-native-C-c-C-z-first))
	  prefix-arg)
      (dotimes (_ (if non-native
		      (progn
			(setq this-command #'smart-switch-to-output-buffer)
			(log (or (car-safe arg) 4) 4))
		    (setq this-command (my/push-key "C-c C-z"))
		    (last-key-repeating)
		    (/
		     (prefix-numeric-value arg)
		     (if non-native-C-c-C-z-first
			 4
		       1))))
	(call-interactively
	 this-command)))))

(define-prefix-command 'my/run-map)
(defkeys my/run
  "C-l" slime
  "C-t" shell
  "C-e" eshell
  "C-p" run-python
  "C-s" sage-shell:run-sage
  "C-i" ielm)

(defkeys  my/avy-passthrough
  "C-c" close-line
  "C-p" open-previous-line
  "C-n" open-next-line

  "C-z" join-line
  "C-l" avy-goto-line

  "C-f" describe-function
  "C-v" describe-variable
  "C-k" describe-key
  "C-h" describe-prefix-bindings

  "C-r" my/run-map

  "C-a" align-regexp
  "C-x" projectile-direct-jack-in
  "C-s" C-c-C-z-or-smart-switch-to-output-buffer)

(define-prefix-command 'my/buffer-jump-map)
(defkey "C-e" my/buffer-jump-map my/avy-passthrough)

(let ((map (make-sparse-keymap)))
  (defun my/buffer-jump (key)
    "When KEY is not a control modified event, create an
association with the current buffer to the control modified
version of the event. When it is a control modified event, switch
to the associated buffer. KEY is `last-input-event' with
interactive use. If KEY is `help-char' display a buffer
describing the associations."
    (interactive (or (listify-key-sequence
		      (lookup-key input-decode-map (vector last-input-event) t))
		     (list last-input-event)))
    (if (eq key help-char)
	(let ((buf "*Buffer jump table*"))
	  (with-output-to-temp-buffer buf
	    (with-current-buffer buf
	      (insert (substitute-command-keys "\\{my/buffer-jump-map}"))
	      (cl-loop for (char . buf) in (cdr map)
		 when (buffer-live-p buf)
		 do
		   (insert (format "%s		%s\n"
				   (key-description (vector char))
				   (buffer-name buf)))))))
      (let ((value (lookup-key map (vector key))))
	(if (or (memq 'control (event-modifiers key))
		(eq key 127)) ;; Make C-h being read as DEL work right
	    (if (buffer-live-p value)
		(pop-to-buffer value)
	      (user-error "%s has no buffer associated with it"
			  (key-description (vector key))))
	  (let* ((key (vector (event-apply-modifier key 'control 26 "C-")))
		 (key (or (lookup-key input-decode-map key t) key))
		 (bind (lookup-key my/buffer-jump-map key)))
	    (if bind
		(user-error "%s is bound to %s"
			    (key-description
			     key
			     (cl-subseq (this-command-keys-vector) 0 -1))
			    bind)
	      (define-key map key (current-buffer))
	      (message "%s now jumps to %s" (key-description key) (buffer-name)))))))))

(defkeys my/buffer-jump
  "C-g" keyboard-quit
  [t] my/buffer-jump)

(with-eval-after-load 'avy-zap
  (defvar avy-zap-dwim-prefer-avy)
  (setq avy-zap-dwim-prefer-avy nil))

(defun smarter-delete-other-windows (arg)
  "With more than one window on frame, `delete-other-windows'.
With \\[universal-argument], first call `ace-window'. With only
one window, `split-window-right' and display next buffer in
`buffer-list'"
  (interactive "P")
  (if (cdr (window-list nil 'never))
      (progn
	(when arg
	  (ace-window nil))
	(delete-other-windows))
    (split-window-right)
    (let ((cur-buf (current-buffer)))
      (unless (get-buffer-process cur-buf)
	(other-window 1))
      (unwind-protect
	   (cl-dolist (win (buffer-list))
	     (unless (or (eq win cur-buf)
			 (minibufferp win))
	       (cl-return (switch-to-buffer win t t))))
	(other-window 1)))))

(defkey "C-x t" smarter-delete-other-windows)

(define-prefix-command 'my-other-window-map)
(make-last-key-repeating-function duplicate-other-window-buffer my-other-window-map t)
(defkeys my-other-window
  "C-c s"	 other-window-imenu
  "C-s"		 other-window-isearch-forward
  "C-r"		 other-window-isearch-backward
  "C-v"		 scroll-other-window
  "C-M-v"	 scroll-other-window
  "M-v"		 scroll-other-window-down)

(defkeys my/window
  "C-l" (eval-when-compile (if (>= emacs-major-version 26) #'display-line-numbers-mode #'linum-mode))
  "C-c" emacs-uptime
  "C-e" erase-buffer
  "C-s" delete-trailing-whitespace
  "C-n" column-number-mode
  "C-m" calc
  "C-p" package-list-packages
  "C-(" smartparens-strict-mode)

(defkey "C-w" my/window-map my/avy-passthrough)

(defkeys
  "C-c g"	 magit-status
  "C-c o"	 ioccur
  "C-c a"	 org-agenda
  "C-c l"	 slime-switch-to-output-buffer
  "C-c t"	 sh-show-shell
  "C-x C-b"	 ibuffer
  "C-x C-k"	 my/kill-this-buffer
  "C-c d"	 duplicate-other-window-buffer
  "C-c C-z"	 smart-switch-to-output-buffer
  "C-c z"	 smart-switch-to-output-buffer
  "C-c e"	 eval-and-replace
  "C-c '"	 (my/push-key "`")
  "C-c -"	 (my/push-key "~")
  "C-c ,"	 insert-splice
  "C-c b"	 previous-buffer
  "C-c f"	 next-buffer
  "C-c k"	 kmacro-keymap
  "C-c s"	 imenu
  "C-c y"	 browse-kill-ring
  "C-c c"	 endless/ispell-word-then-abbrev
  "C-c n"	 gnus
  "C-x w b"	 hi-lock-write-interactive-patterns
  "C-x w r"	 unhighlight-regexp
  "C-x w h"	 highlight-regexp
  "C-x w p"	 highlight-phrase
  "C-x w l"	 highlight-lines-matching-regexp
  "C-x w i"	 (with-no-warnings #'hi-lock-find-patterns)
  "C-x r S"	 activate-word-column-region
  "C-x l"	 count-lines-page-or-maybe-region
  "C-h x"	 x86-lookup
  "C-h r"	 re-builder
  "C-h s"	 string-edit-at-point
  "C-h C-f"	 find-function
  "C-h C-k"	 find-function-on-key
  "C-h C-v"	 find-variable
  "C-x o"	 ace-window
  "M-o"		 ace-window
  "M-n"		 smartscan-symbol-go-forward
  "M-p"		 smartscan-symbol-go-backward
  "M-z"		 avy-zap-up-to-char-dwim
  "M-SPC"	 cycle-spacing
  "C-z"		 repeat
  "C-x z"	 repeat-complex-command
  "<f5>"	 egg-timer
  "C-x c"	 server-edit
  "C-x #"	 nil
  "C-;"		 (if (fboundp 'comment-line)
		     'comment-line
		   (my/push-key "M-;"))
  "H-k"		 kill-whole-line
  "C-c h"	 man
  "H-,"		 (my/push-key "M-<")
  "H-."		 (my/push-key "M->")
  "C-j"		 (lookup-key (current-global-map) (kbd "RET")))

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

(autoload #'er/contract-region "expand-region")
(global-set-key (kbd "H-(") #'er/expand-region)
(global-set-key (kbd "H-)") #'er/contract-region)

(defun er/mark-indented-line ()
  (interactive)
  (end-of-line 1)
  (set-mark (point))
  (back-to-indentation))

(defun er/mark-whole-line ()
  (interactive)
  (end-of-thing 'line)
  (set-mark (point))
  (forward-line -1))

(with-eval-after-load 'expand-region
  (defvar er/try-expand-list)
  (setf (nthcdr (1- (length er/try-expand-list)) er/try-expand-list)
	(cons #'er/mark-indented-line
	      (cons #'er/mark-whole-line
		    (append (last er/try-expand-list)
			    (list #'mark-paragraph)))))
  (setf expand-region-contract-fast-key ")"
	expand-region-reset-fast-key "SPC"))

(global-set-key (kbd "<mouse-8>") #'previous-buffer)
(global-set-key (kbd "<mouse-9>") #'next-buffer)

(defun control-number->self-insert (arg)
  (interactive "p")
  (let* ((char
	  (if (integerp last-command-event)
	      last-command-event
	    (get last-command-event 'ascii-character)))
	 (digit (number-to-string (- (logand char ?\177) ?0))))
    (dotimes (_i arg)
      (insert digit))
    (run-hooks 'post-self-insert-hook)))

(dotimes (k 10)
  (let ((num-string (number-to-string (1- k)))
	(num-char (+ 47 k)))
    (put (intern (concat "C-kp-" num-string)) 'ascii-character num-char)
    (put (intern (concat "M-kp-" num-string)) 'ascii-character num-char)))

(when (eval-when-compile (or (file-exists-p "~/home-row-numbers")
			     (package-installed-p 'home-row-numbers)))

  (eval-when-compile (require 'home-row-numbers))
  (home-row-numbers :layout 'dvorak)

  (global-set-key [remap digit-argument] #'control-number->self-insert))

(defun help-go-to-definition--find-key (key)
  (let (fun)
    (if (and (eq (buffer-name) (help-buffer))
	     (save-excursion
	       (goto-char (point-min))
	       (cl-loop for k across key
		  do (forward-sexp)
		  unless (eq k (aref (kbd (thing-at-point 'symbol)) 0))
		  return nil
		  finally return
		    (progn
		      (re-search-forward
		       "runs[[:space:]\n]the[[:space:]\n]command[[:space:]\n]\\(\\(\\sw\\|\\s_\\)*\\)"
		       (point-at-eol 3) t)
		      (setq fun (intern (match-string 1)))
		      (fboundp fun)))))
	(find-function fun)
      (find-function-on-key key))))

(defun help-go-to-definition (start-buffer)
  "When called after a help describe function, go to the
definition of that thing instead."
  (interactive (list (current-buffer)))
  (cl-dolist (temp/command-history command-history
	      (user-error
	       "No applicable help command in recent history"))
    (let ((fun
	   (case (car temp/command-history)
	     (describe-function #'find-function)
	     (describe-variable #'find-variable)
	     (describe-key #'help-go-to-definition--find-key)
	     (t nil))))
      (when fun
	(unless (and
		 (eval-when-compile (< emacs-major-version 25))
		 ;; Prior to version 25 find-function-on-key used other-window
		 (eq (car temp/command-history)
		     #'describe-key))
	  (pop-to-buffer (help-buffer)))
	(cl-return
	  (prog1
	      (funcall fun
		       (let ((temp (cadr temp/command-history)))
			 (if (consp temp)
			     (cadr temp)
			   temp)))
	    (unless (eq (buffer-name start-buffer) (help-buffer))
	      (pop-to-buffer start-buffer))))))))

(defun my/open-alternative-source-file ()
  "Attempt to switch to the corresponding source file."
  (interactive)
  (let* ((pt (point))
	 (pt-bol (point-at-bol))
	 (line-offset 0)
	 recenter-num
	 line
	 inhibit-goto
	 inhibit-recenter
	 file
	 find-alternate)
    (cond
      ((string-match
	(concat "^\\("
		(expand-file-name "~/")
		".*[./]\\)zsh\\(.*/\\.\\)zsh\\(.*\\'\\)")
	buffer-file-name)
       (setq file (replace-match "\\1bash\\2bash\\3" t nil buffer-file-name)))
      ((string-match
	(concat "^\\("
		(expand-file-name "~/")
		".*[./]\\)bash\\(.*/\\.\\)bash\\(.*\\'\\)")
	buffer-file-name)
       (setq file (replace-match "\\1zsh\\2zsh\\3" t nil buffer-file-name)))
      ((and (eval-when-compile (file-exists-p #1="~/emacs/"))
	    (string-match "/usr/share/emacs/\\([[:digit:].]*/\\)" buffer-file-name))
       (let ((new-file (replace-match "~/emacs/" t t buffer-file-name)))
	 (when (string-match "\\.gz\\'" new-file)
	   (setq new-file (replace-match "" t t new-file)))
	 (setq file new-file
	       find-alternate t)))
      ((string-match (eval-when-compile (regexp-quote (expand-file-name #1#))) buffer-file-name)
       (user-error "Already in Emacs source repo"))
      ((and (eval-when-compile (file-exists-p #2="~/sbcl/"))
	    (string-match "/usr/share/sbcl-source/" buffer-file-name))
       (let ((new-file (replace-match "~/sbcl/" t t buffer-file-name)))
	 (setq file new-file
	       find-alternate t)))
      ((and (string-match "\\(\\.lisp\\)\\'" buffer-file-name)
	    (let ((dir (locate-dominating-file
			buffer-file-name
			(file-name-nondirectory
			 (replace-match ".asd" t t buffer-file-name 1)))))
	      (when dir
		(setq file (concat dir
				   (file-name-sans-extension
				    (file-name-nondirectory buffer-file-name))
				   ".asd")
		      inhibit-goto t
		      inhibit-recenter t)))))
      ((and (string-match "\\(\\.asd\\)\\'" buffer-file-name)
	    (let ((files (directory-files-recursively
			  default-directory
			  (regexp-quote
			   (concat
			    (file-name-sans-extension
			     (file-name-nondirectory buffer-file-name))
			    ".lisp")))))
	      (when files
		(setq file (car (last files))
		      inhibit-goto t
		      inhibit-recenter t)))))
      ((string-match (eval-when-compile (regexp-quote (expand-file-name #2#))) buffer-file-name)
       (user-error "Already in SBCL source repo"))
      ((and (string-match "\\(\\.el\\)\\'" buffer-file-name)
	    (let ((new-file (replace-match ".elc" t t buffer-file-name 1)))
	      (when (file-readable-p new-file)
		(setq line
		      (save-excursion
			(beginning-of-defun)
			(forward-char)
			(forward-sexp)
			(skip-chars-forward "[:blank:]\n")
			(regexp-quote (thing-at-point 'symbol)))
		      inhibit-recenter t
		      line-offset (1- line-offset)
		      pt-bol (point)
		      file new-file)))))
      (t
       (user-error "Could not find an alternative source file")))
    (unless file
      (error "No file specified"))
    (unless (or line inhibit-goto)
      (setq line
	    (let ((line (concat "^" (regexp-quote (thing-at-point 'line)))))
	      (while (and
		      (save-excursion
			(goto-char (point-min))
			(re-search-forward line nil t 2))
		      (not (eobp)))
		(incf line-offset)
		(forward-line)
		(setq line (concat "^" (regexp-quote (thing-at-point 'line)))))
	      line)))
    (unless (or recenter-num inhibit-recenter)
      (setq recenter-num
	    (+ (count-screen-lines (window-start) (point))
	       (if (bolp) 0 -1))))
    (if find-alternate
	(find-alternate-file file)
      (find-file file))
    (unless inhibit-goto
      (goto-char (point-min))
      (condition-case nil
	  (progn
	    (re-search-forward line nil)
	    (forward-line (- (1+ line-offset)))
	    (forward-char (- pt pt-bol)))
	(error (goto-char (max (min pt (point-max)) (point-min))))))
    (unless inhibit-recenter
      (recenter recenter-num))))

(defkey "DEL" help-go-to-definition my/avy-passthrough)
(defkeys
  "C-c v" my/open-alternative-source-file)

(eval-when-compile (require 'xref))
(defun my/help-goto-symbol-at-point ()
  (interactive)
  (let ((xref-backend-functions
	 (if (get-buffer "*scratch*")
	     (with-current-buffer "*scratch*"
	       xref-backend-functions)
	   xref-backend-functions))
	xref-prompt-for-identifier)
    (call-interactively #'xref-find-definitions)))

(with-eval-after-load 'help-mode
  (defkeys help-mode
    "<mouse-8>" help-go-back
    "<mouse-9>" help-go-forward
    "C-c b" help-go-back
    "C-c f" help-go-forward
    "n" next-line
    "p" previous-line
    "M-." my/help-goto-symbol-at-point)
  (defadvice describe-bindings (after describe-bindings-move-to-major-mode compile activate)
    "Pop to the help buffer and search forward for the Major mode bindings."
    (pop-to-buffer (help-buffer))
    (search-forward "Major Mode Bindings:" nil t)
    (recenter-top-bottom 0)))

(defkeys (emacs-lisp-mode lisp-interaction-mode (ielm t t))
  "C-c C-c" eval-defun
  "C-c M-e" macrostep-expand
  "C-c C-w C-c" xref-find-references)

(when (eval-when-compile (< emacs-major-version 25))
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'eldoc-mode))

(add-to-list 'default-frame-alist '(font . "Hack:pixelsize=13"))
(setq split-width-threshold 100)

(autoload #'auth-source-search "auth-source")

(defun sudo-askpass (arg)
  "Attempt to get the password for the prompt ARG from
auth-source. Otherwise use `read-passwd'. See the script
sudo_askpass.sh"
  (or (and
       (string-match "^\\[sudo\\] password for \\([a-z_][a-z0-9_]\\{0,30\\}\\):"
		     arg)
       (let ((pass
	      (plist-get
	       (first
		(auth-source-search :host "localhost" :port "sudo"
				    :user (match-string 1 arg)
				    :require '(:secret)))
	       :secret)))
	 (if (functionp pass)
	     (funcall pass)
	   pass)))
      (read-passwd arg)))

(defmacro defun-do-when-no-process (name no-process &rest else)
  "Define an interactive function called NAME with one anaphoric
argument ARG taking `interactive' parameter p. If the
`current-buffer' has no process call NO-PROCESS, otherwise call
ELSE."
  (declare (indent 1))
  `(defun ,name (arg)
     (interactive "p")
     (if (null (get-buffer-process (current-buffer)))
	 ,no-process
       ,@else)))

(defun-do-when-no-process
    comint-delchar-or-eof-or-kill-buffer
  (kill-buffer)
  (comint-delchar-or-maybe-eof arg))

(define-key comint-mode-map (kbd "C-d") #'comint-delchar-or-eof-or-kill-buffer)
(setq comint-history-isearch 'dwim)
(defkey "C-c C-z" nil comint-mode)

(defun imenu-man-flag-create-index-function ()
  "For use in Man buffers. Suitable for use in
`imenu-create-index-function'. Collects an alist of the flags
listed in the man page and their `point's."
  (goto-char (point-min))
  (cl-loop while (re-search-forward "^\\s-*[-−]+" nil t)
     collect #1=(cons (thing-at-point 'symbol t) (point))
     nconc
       (cl-loop while (re-search-forward ",\\s-+[-−]+" (1+ (point-at-eol)) 1)
	  collect #1#)))

(with-eval-after-load 'man
  (defvar Man-width)
  (setq Man-width 80)
  (require 'imenu)
  (add-hook 'Man-mode-hook
	    (lambda ()
	      (setq-local imenu-create-index-function
			  #'imenu-man-flag-create-index-function))))

(when (and (string-match "zsh\\'" (getenv "SHELL"))
	   (not (getenv "HISTFILE")))
  (setenv "HISTFILE" (expand-file-name "~/.histfile")))

(defun add-mode-line-dirtrack ()
  "Add the abbreviated directory to the end of the mode line."
  (add-to-list 'mode-line-format
	       '(:propertize
		 (" "
		  (:eval (abbreviate-file-name default-directory))
		  " ")
		 face dired-directory)
	       t))

(with-eval-after-load 'shell
  (defvar shell-mode-syntax-table)
  (when (string-match "zsh\\'" (getenv "SHELL"))
    (modify-syntax-entry ?\> " " shell-mode-syntax-table))
  (defvar shell-mode-map)
  (defkeys shell-mode
    "SPC" comint-magic-space)
  (add-hook 'shell-mode-hook #'add-mode-line-dirtrack)
  (add-hook 'shell-mode-hook #'truncate-lines->t))

(with-eval-after-load 'term
  (add-hook 'term-mode-hook #'add-mode-line-dirtrack))

(with-eval-after-load 'multi-term
  (defvar term-unbind-key-list)
  (setq term-unbind-key-list
	'("C-o" "C-g" "C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>"))
  (defvar term-bind-key-alist)
  (setq term-bind-key-alist
	'(("C-c C-c" . term-interrupt-subjob)
	  ("C-c C-e" . term-send-esc)
	  ("C-p" . previous-line)
	  ("C-n" . next-line)
	  ("C-s" . isearch-forward)
	  ("C-r" . term-send-reverse-search-history)
	  ("C-m" . term-send-return)
	  ("M-f" . term-send-forward-word)
	  ("M-b" . term-send-backward-word)
	  ("M-p" . term-send-up)
	  ("M-n" . term-send-down)
	  ("M-M" . term-send-forward-kill-word)
	  ("M-N" . term-send-backward-kill-word)
	  ("<C-backspace>" . term-send-backward-kill-word)
	  ("M-r" . term-send-reverse-search-history)
	  ("M-d" . term-send-delete-word)
	  ("M-," . term-send-raw)
	  ("M-." . comint-dynamic-complete)))
  (with-no-warnings
    (defun multi-term-switch-internal (direction offset)
      (if multi-term-buffer-list
	  (let ((buffer-list-len (length multi-term-buffer-list))
		(my-index (position (current-buffer) multi-term-buffer-list)))
	    (if my-index
		(let ((target-index (if (eq direction 'NEXT)
					(mod (+ my-index offset) buffer-list-len)
				      (mod (- my-index offset) buffer-list-len))))
		  (pop-to-buffer (nth target-index multi-term-buffer-list)))
	      (pop-to-buffer (car multi-term-buffer-list))))
	nil))))

(with-eval-after-load 'em-term
  (defvar eshell-visual-commands)
  (add-to-list 'eshell-visual-commands "htop"))

(defun endless/send-input (input &optional nl)
  "Send INPUT to the current process.
Interactively also sends a terminating newline."
  (interactive "MInput: \nd")
  (let ((string (concat input (if nl "\n"))))
    ;; This is just for visual feedback.
    (let ((inhibit-read-only t))
      (insert-before-markers string))
    ;; This is the important part.
    (process-send-string
     (get-buffer-process (current-buffer))
     string)))

(defun endless/send-self ()
  "Send the pressed key to the current process."
  (interactive)
  (endless/send-input
   (apply #'string
          (append (this-command-keys-vector) nil))))

(define-key compilation-mode-map (kbd "C-c i")
  #'endless/send-input)

(dolist (key '("\C-d" "\C-j" "y" "n"))
  (define-key compilation-mode-map key
    #'endless/send-self))

(with-eval-after-load 'latex
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'latex-preview-pane-mode)
  (with-no-warnings
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
    (define-key LaTeX-mode-map (kbd "C-x n") nil)))

(add-to-list 'auto-mode-alist '("README\\'" . text-mode))

(add-hook 'text-mode-hook #'writegood-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

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

(global-set-key (kbd "C-c (") #'fix-capitalization->paren)

(defun paredit-space-for-predicates-cl (endp delimiter)
  "Do not add a space when \" is preceded by #. reader macro or (
  is preceded by [ (],@ or #.=?"
  (or endp
      (cond ((eq (char-syntax delimiter) ?\")
	     (not (looking-back "#." 2)))
	    ((eq (char-syntax delimiter) ?\()
	     (not (looking-back "\\([\n\s-(],@\\)\\|\\(#.=?\\)" 3)))
	    (t t))))

(defun paredit-uncomment-or-comment-sexp ()
  "If there is an active region call `paredit-comment-dwim',
otherwise if within a comment then uncomment, else call
`smarter-mark-sexp' and comment."
  (interactive)
  (unless (use-region-p)
    (if (save-excursion
  	  (back-to-indentation)
  	  (my/looking-at "\\s<"))
  	(progn
  	  (beginning-of-line)
  	  (while (looking-at "[[:space:]]*\\s<")
  	    (forward-line))
  	  (forward-line -1)
  	  (end-of-line)
  	  (push-mark (point) t t)
  	  (beginning-of-line)
  	  (while (looking-at "[[:space:]]*\\s<")
  	    (forward-line -1))
  	  (forward-line)
  	  (back-to-indentation))
      (smarter-mark-sexp)))
  (with-no-warnings
    (paredit-comment-dwim)))

(defun my/save-excursion-advice (orig &rest args)
  (save-excursion
    (apply orig args)))

(with-eval-after-load 'paredit
  (defvar paredit-mode-map)
  (defun-smarter-movement paredit-wrap-round
    (paredit-backward) (paredit-forward) "M-(" nil nil paredit-mode-map)
  (defkeys paredit-mode
    "C-M-)" "C-)"
    "C-M-(" "C-("
    "C-;" paredit-uncomment-or-comment-sexp
    "C-d" `(menu-item "" ,(lookup-key paredit-mode-map (kbd "C-d"))
		      :filter
		      ,(lambda (cmd)
			 (unless (and (derived-mode-p 'comint-mode)
				      (or (eobp)
					  (not (get-buffer-process (current-buffer)))))
			   cmd))))
  (add-to-list 'paredit-space-for-delimiter-predicates
	       #'paredit-space-for-predicates-cl)
  (advice-add 'paredit-reindent-defun :around #'my/save-excursion-advice))

(defun my/wrap-earmuffs ()
  "Wrap the symbol at point in *'s or insert **"
  (interactive)
  (let ((forward-blank (my/looking-at "\\s-\\|$"))
	(backward-blank (my/looking-back "\\s-\\|^" 1))
	(not-region (not (use-region-p)))
	move-forward)
    (save-excursion
      (if (and forward-blank backward-blank)
	  (progn
	    (insert "**")
	    (setq move-forward t))
	(when (and (not forward-blank)
		   (not backward-blank)
		   not-region)
	  (backward-sexp))
	(when not-region
	  (smarter-mark-sexp))
	(exchange-point-and-mark)
	(insert "*")
	(exchange-point-and-mark)
	(insert "*")))
    (when move-forward
      (forward-char)))
  (setq mark-active nil))

(defkey "C-*" my/wrap-earmuffs)

(defun make-new-quicklisp-project (name description)
  "Make a new project in the quicklisp local-projects directory
entitled NAME with DESCRIPTION. Create an asd file for the
project."
  (interactive "sProject name: \nsDescription: ")
  (let ((project-dir
	 (concat (expand-file-name "~/quicklisp/local-projects/") name "/")))
    (make-directory project-dir)
    (with-temp-file (concat project-dir name ".asd")
      (insert (concat "(in-package :asdf-user)\n\n"
		      "(defsystem :" name "\n    "
		      ":description \"" description "\"\n    "
		      ":version \"0.0.1\"\n    "
		      ":author \"" user-full-name " <" user-mail-address ">\"\n    "
		      ":depends-on (\"iterate\" \"alexandria\")\n    "
		      ":components ((:file \"" name "\")))\n")))
    (with-temp-file (concat project-dir name ".lisp")
      (insert (concat ";;; Copyright " (format-time-string "%Y") " " user-full-name "\n"
		      "(in-package :cl-user)\n"
		      "(defpackage :" name "\n  "
		      "(:use :common-lisp :iterate :alexandria))\n"
		      "(in-package :" name ")\n\n")))
    (find-file (concat project-dir name ".lisp"))
    (goto-char (point-max))))

(global-set-key (kbd "C-c p n") #'make-new-quicklisp-project)

(autoload 'slime-eval-print-last-expression "slime")

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

(autoload 'slime-scratch "slime")

(with-eval-after-load 'lisp-mode
  (font-lock-add-keywords
   'lisp-mode
   '(("(\\(iter\\(ate\\)?\\|defmacro-\\(driver\\|clause\\)\\)[[:space:]\n]" 1 'font-lock-keyword-face)
     ("(\\(i\\(ter\\(ate\\)?\\|n\\)\\|block\\|return-from\\|loop\\s-+named\\)\\s-+\\([^()[:space:]]+\\)" 4 'font-lock-constant-face)
     ("(\\(f\\(or\\|in\\(ish\\|ally\\(-protected\\)?\\)\\)\\|generate\\|w\\(hile\\|ith\\)\\|until\\|repeat\\|leave\\|next-iteration\\|i\\(n\\(itially\\)?\\|f-first-time\\)\\|after-each\\|else\\)[[:space:]\n)]" 1 'font-lock-keyword-face)
     ("(define-constant\\s-+\\(\\(\\sw\\|\\s_\\)*\\)" 1 'font-lock-variable-name-face))
   t)
  (define-key lisp-mode-map (kbd "C-c e") #'slime-eval-and-replace)
  (define-key lisp-mode-map (kbd "C-c C-s") #'slime-scratch)
  (load (concat user-emacs-directory "lisp/cl-indentation") nil t))

(defun slime-return-to-lisp-file ()
  "Go backwards through the buffer list until one in lisp mode is found."
  (interactive)
  (dolist (buff (buffer-list) (current-buffer))
    (if (equal 'lisp-mode
	       (buffer-local-value 'major-mode buff))
	(progn
	  (pop-to-buffer buff)
	  (return buff)))))

(defun slime-repl-previous-if-eobp-else-up ()
  "If point is at end of the buffer call
`slime-repl-previous-input', else move up."
  (interactive)
  (if (eobp)
      (with-no-warnings
	(slime-repl-previous-input))
    (call-interactively #'previous-line)))

(defun slime-repl-next-if-eobp-else-down ()
  "If point is at end of the buffer call `slime-repl-next-input',
 else move down."
  (interactive)
  (if (eobp)
      (with-no-warnings
	(slime-repl-next-input))
    (call-interactively #'next-line)))

(defun add-paredit-to-list (orig &rest args)
  "For use with `advice-add', add `paredit-mode' to the beginning
  of the list returned by ORIG."
  (cons #'paredit-mode
	(apply orig args)))

(with-eval-after-load 'slime-repl
  (defvar slime-auto-start)
  (setq slime-auto-start 'always)
  (defvar slime-repl-history-size)
  (setq slime-repl-history-size 500)
  (advice-add 'slime-minibuffer-setup-hook :around #'add-paredit-to-list)
  (defvar slime-repl-mode-map)
  (define-key slime-repl-mode-map (kbd "C-c C-s") #'slime-scratch)
  (define-key slime-repl-mode-map (kbd "C-c e") #'slime-eval-and-replace)
  (define-key slime-repl-mode-map (kbd "<up>") #'slime-repl-previous-if-eobp-else-up)
  (define-key slime-repl-mode-map (kbd "<down>") #'slime-repl-next-if-eobp-else-down)
  (define-key slime-repl-mode-map (kbd "C-c C-z") #'slime-return-to-lisp-file)
  (define-key slime-repl-mode-map (kbd "DEL") nil)
  (define-key slime-repl-mode-map (kbd "M-r") nil)
  (with-no-warnings
    (define-key slime-repl-mode-map (kbd "C-M-r") #'slime-repl-previous-matching-input)
    (define-key slime-repl-mode-map (kbd "C-c q") #'slime-disconnect)
    (define-key slime-repl-mode-map (kbd "C-c C-q") #'slime-quit-lisp)))

(defadvice slime-scratch (after slime-scratch-add-top-line-comment compile activate)
  (when (and (bobp) (not (my/looking-at ".")))
    (insert ";; This buffer is for notes you don't want to save, and for Common Lisp evaluation.\n\n")))

(defun cider-repl-previous-if-eobp-else-up ()
  "If point is at end of the buffer call
`cider-repl-previous-input', else move up."
  (interactive)
  (if (eobp)
      (with-no-warnings
	(cider-repl-previous-input))
    (call-interactively #'previous-line)))

(defun cider-repl-next-if-eobp-else-down ()
  "If point is at end of the buffer call `cider-repl-next-input',
 else move down."
  (interactive)
  (if (eobp)
      (with-no-warnings
	(cider-repl-next-input))
    (call-interactively #'next-line)))

(with-eval-after-load 'cider
  (defvar cider-prompt-for-symbol)
  (setq cider-prompt-for-symbol nil))

(with-eval-after-load 'cider-repl
  (defvar cider-repl-history-size)
  (setq cider-repl-history-size 500)
  (defvar cider-repl-mode-map)
  (define-key cider-repl-mode-map (kbd "C-c C-s") #'cider-scratch)
  (define-key cider-repl-mode-map (kbd "<up>") #'cider-repl-previous-if-eobp-else-up)
  (define-key cider-repl-mode-map (kbd "<down>") #'cider-repl-next-if-eobp-else-down)
  (with-no-warnings
    (define-key cider-repl-mode-map (kbd "C-M-r") #'cider-repl-previous-matching-input)))

(defun paredit-or-smartparens ()
  "Enable either paredit or smartparens strict mode."
  (if (member major-mode '(emacs-lisp-mode
			   lisp-interaction-mode
			   lisp-mode
			   clojure-mode
			   scheme-mode))
      (enable-paredit-mode)
      (smartparens-strict-mode)))

(add-hook 'prog-mode-hook #'paredit-or-smartparens)

(add-hook 'eshell-mode-hook				#'enable-paredit-mode)
(add-hook 'ielm-mode-hook				#'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook	#'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook				#'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook				#'enable-paredit-mode)

(defun my/c-electric-brace-electric-pair (&rest args)
  (when (= last-command-event ?\{)
    (save-excursion
      (let ((last-command-event ?\})
  	    smartparens-mode)
  	(self-insert-command (prefix-numeric-value (car-safe args)))))
    (smarter-open-line 1)))

(with-eval-after-load 'cc-mode
  (add-hook 'c-mode-hook 'c-toggle-auto-newline)
  (add-hook 'c++-mode-hook 'c-toggle-auto-newline)
  (when (eval-when-compile
	  (file-exists-p "~/emacs"))
    (with-eval-after-load 'cmacexp
      (defvar c-macro-cppflags)
      (setq c-macro-cppflags
	    (concat c-macro-cppflags
		    (when c-macro-cppflags " ")
		    "-I" (expand-file-name "~/emacs/src")
		    " -I" (expand-file-name "~/emacs/lib")))))
  (defvar c-mode-map)
  (defvar c++-mode-map)
  (defkeys (c-mode c++-mode)
    "C-c M-e" macrostep-expand)
  (require 'smartparens)
  (with-no-warnings
    (sp-local-pair '(c-mode c++-mode) "{" "}" :actions '(:rem insert)))
  (advice-add 'c-electric-brace :after #'my/c-electric-brace-electric-pair))

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches \"[\]\)}]\" then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (my/looking-at "\]\\|\)\\|\}")
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defset-function electrify-return-if-match "RET")

(defun lazy-pdf-tools-install ()
  "Run `pdf-tools-install' and then call `pdf-view-mode'."
  (pdf-tools-install)
  (setf auto-mode-alist
	(delete '("\\.[pP][dD][fF]\\'" . lazy-pdf-tools-install) auto-mode-alist))
  (with-no-warnings
    (pdf-view-mode)))

(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . lazy-pdf-tools-install))

(with-eval-after-load 'pdf-tools
  (with-no-warnings
    (define-key pdf-view-mode-map (kbd "a") #'image-bol)
    (define-key pdf-view-mode-map (kbd "e") #'image-eol)))

(defadvice abort-if-file-too-large (around no-abort-if-file-is-pdf compile activate)
  (if (string-match-p "\\.[pP][dD][fF]\\'" filename)
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

(with-eval-after-load 'org
  (defvar org-modules)
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (defvar org-mode-map)
  (define-key org-mode-map (kbd "M-j") (kbd "M-<return>"))
  (define-key org-mode-map (kbd "C-c i") #'interleave-mode)
  (define-key org-mode-map (kbd "C-c '") nil)
  (with-no-warnings
    (define-key org-mode-map (kbd "M-n") #'outline-next-visible-heading)
    (define-key org-mode-map (kbd "M-p") #'outline-previous-visible-heading))
  (with-eval-after-load 'org-src
    (with-no-warnings
      (setq org-edit-src-persistent-message nil)
      (define-key org-src-mode-map (kbd "C-c '") nil)
      (define-key org-src-mode-map (kbd "C-x c") #'org-edit-src-exit)
      (define-key org-src-mode-map (kbd "C-x C-s") #'org-edit-src-exit)))
  (defvar org-agenda-sticky)
  (setq org-agenda-sticky t))

(defalias 'run-sage #'sage-shell:run-sage)

(defun-do-when-no-process
    sage-shell:delchar-or-maybe-eof-or-kill-buffer
  (kill-buffer)
  (with-no-warnings
    (sage-shell:delchar-or-maybe-eof arg)))

(with-eval-after-load 'sage-shell-mode
  (sage-shell:define-alias)
  (defvar sage-shell-mode-map)
  (defkey "C-d" sage-shell:delchar-or-maybe-eof-or-kill-buffer sage-shell-mode)
  (defvar sage-shell:input-history-cache-file)
  (setq sage-shell:input-history-cache-file "~/.emacs.d/.sage_shell_input_history"))

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

(defun python-setup-history ()
  (setq-local comint-input-ring-file-name (expand-file-name "~/.python_history"))
  (comint-read-input-ring t))

(with-eval-after-load 'python
  (elpy-enable)
  (defvar inferior-python-mode-map)
  (define-key inferior-python-mode-map (kbd "C-c M-o") #'python-repl-clear-buffer)
  (add-hook 'inferior-python-mode-hook #'python-setup-history)
  (add-to-list 'safe-local-variable-values '(python-shell-interpreter . "python2")))

(defun sp-paredit-like-close-round ()
  "If the next character is a closing delimiter skip it,
otherwise self-insert."
  (interactive)
  (if (and (not (eobp)) (my/looking-at "\\s)"))
      (forward-char 1)
    (let ((pt (point))
	  message-log-max)
      (with-temp-message (or (current-message) "")
	(call-interactively #'self-insert-command))
      (when (= pt (point))
	(delete-blank-lines)
	(re-search-forward "\\s)")))))

(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (defvar smartparens-mode-map)
  (defkey ")" sp-paredit-like-close-round smartparens-mode))

(defun maybe-enable-smartparens ()
  "Enable `smartparens-strict-mode' unless in `paredit-mode' or
`smartparens-mode' or in `ielm'."
  (unless (or paredit-mode smartparens-mode
	      (member major-mode
		      '(inferior-emacs-lisp-mode)))
    (smartparens-strict-mode)))

(add-hook 'comint-mode-hook #'maybe-enable-smartparens)

(defkeys ((python-mode t t)
	  (sage-shell-mode t sage-shell-mode))
  [remap sp-forward-slurp-sexp] (with-no-warnings #'sp-slurp-hybrid-sexp))

(with-eval-after-load 'elpy
  (add-hook 'elpy-mode-hook #'set-flymake-no-changes-timeout-to-one-hour)
  (defvar elpy-mode-map)
  (define-key elpy-mode-map (kbd "M-,") #'pop-tag-mark)
  (define-key elpy-mode-map (kbd "C-c C-c") 'python-shell-send-defun)
  (define-key elpy-mode-map (kbd "C-c C-k") 'elpy-shell-send-region-or-buffer))

(defvar flymake-no-changes-timeout)
(defun set-flymake-no-changes-timeout-to-one-hour ()
  (interactive)
  (set (make-local-variable 'flymake-no-changes-timeout) 3600))

(with-eval-after-load 'wttrin
  (advice-add 'wttrin :after #'truncate-lines->t))

;;; Fix security issue in older versions of Emacs
(when (eval-when-compile (version< emacs-version "25.3"))
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional _param)
      (list start end))))

(cl-macrolet
    ((my/undisable (&rest functions)
       `(progn
	  ,@(cl-loop for f in functions
	       collect `(put ',f 'disabled nil)))))

  (my/undisable
   set-goal-column
   erase-buffer
   downcase-region
   upcase-region
   narrow-to-region
   timer-list))

(defun maybe-delete-trailing-whitespace ()
  "Call `delete-trailing-whitespace' if the `major-mode' is
derived from prog-mode."
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'maybe-delete-trailing-whitespace)

(when (and (boundp 'user-full-name)
	   (not (string= user-full-name ""))
	   (require 'copyright)
	   (string= copyright-names-regexp ""))
  (setq copyright-names-regexp (regexp-quote user-full-name)))

(when (and (boundp 'copyright-names-regexp)
	   (not (string= copyright-names-regexp "")))
  (add-hook 'before-save-hook #'copyright-update))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
	     (or (not
		  (string-match
		   (eval-when-compile
		     (concat "^" (regexp-quote
				  (expand-file-name "~/emacs/"))))
		   default-directory))
		 (yes-or-no-p
		  (format "Byte compile file %s? "
			  (abbreviate-file-name buffer-file-name))))
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook #'byte-compile-current-buffer)
