;;; init.el --- haoxiangliew's Emacs configuration

;;; Commentary:
;; This is my personal Emacs configuration

;;; Dependencies:
;; git
;; ripgrep
;; fd
;; gitAndTools.delta

;;; Code:

;; bootstrap straight and use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'straight)
(eval-when-compile (straight-use-package 'use-package))
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)

;; define internet check
(defun internet-check (&optional host)
  "Check for internet connection with ping HOST."
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
		     (if host host "www.google.com"))))

;; straight management
(if (internet-check)
    (if (daemonp)
	(progn (straight-pull-all)
	       (straight-normalize-all)
	       (add-hook 'after-init-hook 'straight-prune-build)
	       (add-hook 'after-init-hook 'straight-remove-unused-repos))
      (message "In order to reduce startup delay, we are not going to update straight on startup. Run straight-maintain to do so."))
  (message "There is no internet connection, we are unable to update straight."))
(defun straight-maintain ()
  "Maintain straight packages and repositories."
  (interactive)
  (straight-fetch-all)
  (straight-merge-all)
  (straight-normalize-all)
  (straight-prune-build)
  (straight-remove-unused-repos)
  (eval-and-compile (straight-use-package 'use-package)))

;; emacs config
(use-package emacs
  :straight (:type built-in)
  :init
  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)
  ;; vertico config
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  ;; prioritize non-byte-compiled source files in non interactive session
  (setq load-prefer-newer noninteractive)
  ;; optimize process throughput
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  ;; define restart emacs
  (defun launch-separate-emacs-in-terminal ()
    (suspend-emacs "fg ; emacs -nw"))
  (defun launch-separate-emacs-under-x ()
    (call-process "sh" nil nil nil "-c" "emacs &"))
  (defun restart-emacs ()
    (interactive)
    (if (daemonp)
	(progn (restart-counter)
	       (save-buffers-kill-emacs))
      (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
							       #'launch-separate-emacs-under-x
							     #'launch-separate-emacs-in-terminal)))))
	(restart-counter)
	(save-buffers-kill-emacs))))
  (defun restart-counter ()
    (setq restart-count 5)
    (while (> restart-count 0)
      (if (daemonp)
	  (message "We are in a daemon, killing in %d seconds..." . (restart-count))
	(message "Restarting in %d seconds..." . (restart-count)))
      (sleep-for 1)
      (setq restart-count (- restart-count 1))))
  ;; load secrets
  (defun load-if-exists (f)
    (if (file-exists-p (expand-file-name f))
	(load-file (expand-file-name f))))
  (load-if-exists (concat (print user-emacs-directory) "secrets.el"))
  (setq auth-sources '("~/.authinfo"))
  :config
  ;; name and email for various things like git and email
  (setq user-full-name "Hao Xiang Liew"
	user-mail-address "haoxiangliew@gmail.com")
  ;; optimize terminal use
  (setq xterm-set-window-title t)
  (setq visible-cursor nil)
  (add-hook 'tty-setup-hook #'xterm-mouse-mode)
  ;; optimize stumpwm frame
  (setq frame-resize-pixelwise t)
  ;; disable bells
  (setq ring-bell-function 'ignore)
  ;; change yes/no to y/n
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; prevent emacs from buffering
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  (setq pgtk-wait-for-event-timeout 0.001)
  ;; disable bidirectional text scanning
  (setq-default bidi-display-reordering 'left-to-right
		bidi-paragraph-direction 'left-to-right)
  ;; set font
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-10.5"))
  (set-face-attribute 'default t :font "JetBrains Mono-10.5")
  ;; highlight matching parentheses
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  ;; raise undo limit to 80 mb
  (setq undo-limit 80000000)
  ;; match parentheses
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  ;; open config defun
  (defun open-config ()
    (interactive)
    (find-file (concat (print user-emacs-directory) "init.el")))
  (global-set-key (kbd "C-c C-s") 'open-config)
  ;; intelligent word-wrap
  (defvar +word-wrap-extra-indent 'double)
  (defvar +word-wrap-disabled-modes '(fundamental-mode so-long-mode))
  (defvar +word-wrap-text-modes '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode rst-mode latex-mode LaTeX-mode))
  (when (memq 'visual-line-mode text-mode-hook)
    (remove-hook 'text-mode-hook #'visual-line-mode)
    (add-hook 'text-mode-hook #'+word-wrap-mode))
  ;; use system clipboard
  (setq select-enable-clipboard t)
  ;; fix scrolling
  (setq hscroll-margin 1
	scroll-conservatively 101
	scroll-margin 0
	scroll-preserve-screen-position t
	auto-window-vscroll nil
	mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
	mouse-wheel-scroll-amount-horizontal 2)
  (setq fast-but-imprecise-scrolling t)
  ;; smooth scrolling
  (if (<= emacs-major-version 29)
      (progn (pixel-scroll-precision-mode)
	     (setq pixel-scroll-precision-large-scroll-height 40.0
		   pixel-scroll-precision-interpolation-factor 30))
    (pixel-scroll-mode 1))
  ;; autosave
  (setq auto-save-default t)
  ;; better backups
  (if (file-exists-p "~/.emacs-backups")
      (setq backup-directory-alist '(("." . "~/.emacs-backups/"))
	    backup-by-copying t
	    version-control t
	    delete-old-versions t
	    kept-new-versions 20
	    kept-old-versions 5)
    (make-directory "~/.emacs-backups"))
  ;; delete trailing whitespace
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; gcmh
(use-package gcmh
  :init
  (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 'auto
	gcmh-auto-idle-delay-factor 10
	gcmh-high-cons-threshold (* 16 1024 1024))) ; 16mb

;; dashboard
(use-package dashboard
  :after
  all-the-icons
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (defun dashboard-banner-startup()
    (if (display-graphic-p)
        (setq dashboard-startup-banner (concat (print user-emacs-directory) "home.png"))
      (setq dashboard-startup-banner (concat (print user-emacs-directory) "home.txt"))))
  (add-hook 'server-after-make-frame-hook 'dashboard-banner-startup)
  (dashboard-banner-startup)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title (concat "Hi " (user-full-name) "! Welcome to Chika Emacs!"))
  (setq dashboard-center-content t)
  (setq dashboard-week-agenda nil)
  (setq dashboard-items '((recents . 5)
			  (agenda . 5)))
  (setq dashboard-match-agenda-entry "TODO")
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
	`((
	   (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
	    "Browse Repository"
	    "Browse configuration repository"
	    (lambda (&rest _) (browse-url "https://github.com/haoxiangliew/.emacs.d")))
	   (,(all-the-icons-octicon "gear" :height 1.1 :v-adjust 0.0)
	    "Edit Config"
	    "Edit current configuration"
	    (lambda (&rest _) (open-config)))
	   (,(all-the-icons-octicon "cloud-download" :height 1.1 :v-adjust 0.0)
	    "Update"
	    "Updates and cleans all packages"
	    (lambda (&rest _) (straight-maintain)))
	   (,(all-the-icons-octicon "sync" :height 1.1 :v-adjust 0.0)
	    "Restart"
	    "Restart Emacs"
	    (lambda (&rest _) (restart-emacs)))
	   )
	  ()
	  (
	   (,(all-the-icons-octicon "rocket" :height 1.1 :v-adjust 0.0)
	    "Projects"
	    "Opens list of projects in treemacs"
	    (lambda (&rest _) (treemacs)))
	   (,(all-the-icons-octicon "sync" :height 1.1 :v-adjust 0.0)
	    "Sync"
	    "Sync projects to treemacs"
	    (lambda (&rest _) (treemacs-projectile)))
	   (,(all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.0)
	    "Agenda"
	    "Opens org-agenda"
	    (lambda (&rest _) (org-agenda-list)))
	   (,(all-the-icons-octicon "calendar" :height 1.1 :v-adjust 0.0)
	    "Calendar"
	    "Opens my-calendar"
	    (lambda (&rest _) (open-my-calendar)))
	   (,(all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0)
	    "Email"
	    "Opens notmuch for emails"
	    (lambda (&rest _) (notmuch-hello)))
	   )))
  :config
  (dashboard-setup-startup-hook)
  (if (< (length command-line-args) 2)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))))

;; dracula-theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'(lambda () (load-theme 'doom-dracula t)))
    (load-theme 'doom-dracula t))
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :config
  (add-to-list 'solaire-mode-themes-to-face-swap "^doom-")
  (add-hook 'dashboard-mode-hook 'solaire-mode)
  (solaire-global-mode +1))

;; all-the-icons
(use-package all-the-icons
  :if
  (display-graphic-p)
  :init
  (setq inhibit-compacting-font-caches t)
  :config
  (if (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
      (message "all-the-icons is installed!")
    (and (all-the-icons-install-fonts) (restart-emacs))))

(use-package all-the-icons-completion
  :after
  all-the-icons
  :config
  (all-the-icons-completion-mode))

;; emojify
(use-package emojify
  :init
  (add-hook 'after-init-hook #'global-emojify-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; rainbow-mode
(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode)
  (add-hook 'rainbow-mode-hook (hl-line-mode (if rainbow-mode -1 +1))))

;; vertico
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
		     :includes ())
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  :config
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; which-key
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-allow-multiple-replacements t))

;; doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (column-number-mode)
  (size-indication-mode))

;; company
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'global-company-mode-hook #'company-tng-mode)
  (add-hook 'eshell-mode-hook (lambda ()
				(company-mode -1)))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-box
  :hook
  (company-mode . company-box-mode)
  :config
  (setq company-frontends '(company-tng-frontend company-box-frontend)))

;; projectile
(use-package projectile
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1))

;; lsp-mode
(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil))
(use-package lsp-ui)

;; flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

;; flyspell
(use-package flyspell
  :init
  (add-hook 'yaml-mode-hook #'flyspell-prog-mode)
  (add-hook 'conf-mode-hook #'flyspell-prog-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flyspell-correct
  :after
  flyspell
  :bind
  (("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-popup
  :after
  flyspell-correct)

(use-package flyspell-lazy
  :init
  (flyspell-lazy-mode)
  :config
  (flyspell-mode 1))

;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-triggers-in-field t))

;; doom-snippets
(use-package doom-snippets
  :straight
  (doom-snippets
   :type git
   :host github
   :repo "hlissner/doom-snippets")
  :after
  yasnippet)

;; dap-mode
(use-package dap-mode
  :hook
  (dap-mode . dap-ui-mode)
  :hook
  (dap-ui-mode . dap-ui-controls-mode)
  :after
  lsp
  :init
  (setq dap-breakpoints-file "~/.emacs-backups/dap-breakpoints"
        dap-utils-extension-path "~/.emacs-backups/dap-extension/")
  :config
  (dap-mode 1))

;; dumb-jump
(use-package dumb-jump
  :config
  (setq dumb-jump-default-project user-emacs-directory
        dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil
        dumb-jump-selector 'popup)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

;; dired
(use-package dired
  :straight (:type built-in)
  :init
  (setq dired-auto-revert-buffer t
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask
        image-dired-dir (concat "~/.emacs-backups/" "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150))
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; ranger
(use-package ranger
  :after
  dired
  :config
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir))
  (setq ranger-cleanup-on-disable t
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details t
        ranger-max-preview-size 10
        ranger-show-literal nil
        ranger-hide-cursor nil))

;; eshell
(use-package eshell
  :bind
  ("C-x C-e" . eshell)
  :straight (:type built-in)
  :init
  (defun eshell-add-aliases ()
    (dolist (var   '(("q"  "exit")
		     ("ff" "find-file $1")
		     ("d"  "dired $1")
		     ("rg" "rg --color=always $*")
		     ("l"  "ls -lh $*")
		     ("ll" "ls -lah $*")
		     ("git" "git --no-pager $*")
		     ("gg" "magit-status")
		     ("cdp" "cd-to-project")
		     ("clear" "clear-scrollback")))
      (add-to-list 'eshell-command-aliases-list var)))
  (add-hook 'eshell-post-command-hook 'eshell-add-aliases)
  :config
  (setq eshell-prompt-regexp "^.* λ "
	eshell-prompt-function #'+eshell/prompt)
  (defun +eshell/prompt ()
    (let ((base/dir (shrink-path-prompt default-directory)))
      (concat (propertize (car base/dir)
                          'face 'font-lock-comment-face)
	      (propertize (cdr base/dir)
                          'face 'font-lock-constant-face)
	      (propertize " λ" 'face 'eshell-prompt-face)
	      (propertize " " 'face 'default)))))
(use-package shrink-path)
(use-package fish-completion
  :hook
  (eshell-mode . fish-completion-mode)
  :init
  (setq fish-completion-fallback-on-bash-p t))

;; vterm
(use-package vterm
  :bind
  ("C-x C-t" . vterm)
  ;; if vterm is installed through nix
  :straight f
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

;; ibuffer
(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold)))
  (define-ibuffer-column icon (:name "  ")
    (let ((icon (if (and (buffer-file-name)
                         (all-the-icons-auto-mode-match?))
                    (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                  (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
      (if (symbolp icon)
          (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
        icon)))
  (define-ibuffer-column size
    (:name "Size"
	   :inline t
	   :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

;; undo-tree
(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; treemacs
(use-package treemacs
  :bind
  ("C-<tab>" . treemacs)
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file "~/.emacs-backups/treemacs-persist"
        treemacs-last-error-persist-file "~/.emacs-backups/treemacs-last-error-persist"))

(use-package treemacs-magit
  :after
  (treemacs magit)
  :config
  (setq treemacs-git-mode 'simple))
(use-package treemacs-projectile
  :after
  (treemacs projectile))
(use-package lsp-treemacs
  :after
  (treemacs lsp))

;; magit
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :init
  (require 'git-commit)
  (setq transient-default-level 5))
(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))
(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?"))
(use-package magit-delta
  :after magit
  :init
  (magit-delta-mode +1))

(use-package git-gutter
  :config
  (global-git-gutter-mode 't))

;; highlight-indent-guides
(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

;; multiple-cursors
(use-package multiple-cursors
  :bind
  ("C-c c" . mc/edit-lines))

;; ligatures
(use-package ligature
  :straight
  (ligature :type git :host github :repo "mickeynp/ligature.el")
  :init
  (global-prettify-symbols-mode)
  :config
  ;; Enable all JetBrains Mono ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
				       "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
				       "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
				       "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
				       "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
				       "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
				       ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
				       "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
				       "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
				       "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
				       "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; org-mode
(use-package org
  :straight (:type built-in)
  :bind
  ("C-x C-a" . org-agenda)
  :config
  (setq org-directory "~/haoxiangliew/org")
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-include-deadlines t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-tags-column 100))

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups '((:name "Today"
					 :time-grid t
					 :date today
					 :deadline today
					 :scheduled today
					 :order 1)
                                  (:name "Important"
					 :priority "A"
					 :order 2)
                                  (:name "Overdue"
					 :deadline past
					 :scheduled past
					 :face error
					 :order 3)
                                  (:name "Homework"
					 :tag "homework"
					 :order 5)
                                  (:name "Tasks"
					 :tag "tasks"
					 :order 6)
                                  (:name "Classes"
					 :tag "classes"
					 :order 7)))
  (org-super-agenda-mode))

(use-package org-appear
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))
(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
(use-package org-fancy-priorities
  :config
  (add-hook 'org-mode-hook 'org-fancy-priorities-mode))

(use-package alert
  :config
  (setq alert-default-style 'libnotify))
(use-package org-wild-notifier
  :config
  (org-wild-notifier-mode))

;; calfw (calendar)
(use-package calfw
  :bind
  ("C-c C-c" . open-my-calendar)
  :config
  (setq cfw:face-item-separator-color nil
        cfw:render-line-breaker 'cfw:render-line-breaker-none)
  (defun open-my-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "#50fa7b")
      (cfw:ical-create-source "Canvas" "https://canvas.vt.edu/feeds/calendars/user_B7azceel162srPg4Nw9Ax13hcF0aPcJ57bcriQbK.ics" "#ff5555")
      ))))

(use-package calfw-org
  :after
  calfw)
(use-package calfw-ical
  :after
  calfw)

;; notmuch
(use-package notmuch
  :bind
  ("C-x C-m" . notmuch-hello)
  :init
  (setq +notmuch-mail-folder "~/mail/")
  (setq +notmuch-sync-backend "notmuch new")
  (setq-default notmuch-search-oldest-first nil)
  :config
  (setq notmuch-fcc-dirs nil
        message-kill-buffer-on-exit t
        message-send-mail-function 'message-send-mail-with-sendmail
        send-mail-function 'sendmail-send-it
        ;; sendmail-program "/usr/local/bin/msmtp"
        notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-30s ")
          ("subject" . "%-72s ")
          ("tags" . "(%s)"))
        notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread)))
        notmuch-saved-searches
        '((:name "inbox"   :query "tag:inbox not tag:trash" :key "i")
          (:name "flagged" :query "tag:flagged"             :key "f")
          (:name "sent"    :query "tag:sent"                :key "s")
          (:name "drafts"  :query "tag:draft"               :key "d"))
        notmuch-archive-tags '("-inbox" "-unread"))
  (setq notmuch-show-log nil
        notmuch-hello-sections `(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        notmuch-message-headers-visible nil))

;; hl-todo
(use-package hl-todo
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode)
  (add-hook 'yaml-mode-hook 'hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

;; language configuration

;; format-all
;; check https://github.com/lassik/emacs-format-all-the-code#supported-languages
(use-package format-all
  :config
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter)
  (add-hook 'prog-mode-hook 'format-all-mode))

;; c/c++
;; requires clangd v9+
(use-package lsp-mode
  :init
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  :config
  (setq lsp-clients-clangd-args '("-j=3"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0")))

;; nix
;; requires nixfmt and rnix-lsp
(use-package lsp-mode
  :init
  (add-hook 'nix-mode-hook 'lsp))
(use-package nix-mode
  :interpreter
  ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode
  "\\.nix\\'")
(use-package nix-update)
(use-package company-nixos-options)

;;; init.el ends here
