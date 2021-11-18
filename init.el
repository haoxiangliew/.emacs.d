;;; package --- Summary
;; haoxiangliew's Emacs configuration

;;; Commentary:
;; This is my personal Emacs configuration

;;; Dependencies:
;; git
;; ripgrep
;; fd-find
;; git-delta

;;; LSP dependencies:
;; clangd v9+
;; pip3 install hdl-checker

;;; Code:

;; define internet check
(defun internet-check (&optional host)
  "Check for internet connection with ping HOST."
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
		     (if host host "www.google.com"))))

;; bootstrap straight and use-package
(setq package-enable-at-startup nil)
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

;; get rid of compiler warnings
(setq native-comp-async-report-warnings-errors nil)

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
  (straight-remove-unused-repos))

;; emacs config
(use-package emacs
  :straight (:type built-in)
  :init
  ;; vertico config
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  ;; minimize garbage collection on startup
  (setq gc-cons-threshold most-positive-fixnum)
  ;; lower threshold back to 8 MiB (default value is 800 kB)
  (add-hook 'emacs-startup-hook (lambda() (setq gc-cons-threshold (expt 2 23))))
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
	(kill-daemon)
      (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                               #'launch-separate-emacs-under-x
                                                             #'launch-separate-emacs-in-terminal)))))
	(save-buffers-kill-emacs))))
  (defun kill-daemon ()
    (interactive)
    (message "We are in a daemon, killing in 5 seconds...")
    (sit-for 1)
    (message "We are in a daemon, killing in 4 seconds...")
    (sit-for 1)
    (message "We are in a daemon, killing in 3 seconds...")
    (sit-for 1)
    (message "We are in a daemon, killing in 2 seconds...")
    (sit-for 1)
    (message "We are in a daemon, killing in 1 seconds...")
    (sit-for 1)
    (save-buffers-kill-emacs))
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
  ;; disable default UI elements
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  ;; highlight current line
  ;; (global-hl-line-mode 1)
  ;; use bar cursor
  (setq-default cursor-type 'bar)
  ;; optimize terminal use
  (setq xterm-set-window-title t)
  (setq visible-cursor nil)
  (add-hook 'tty-setup-hook #'xterm-mouse-mode)
  ;; disable bells
  (setq ring-bell-function 'ignore)
  ;; add padding to compensate for rounded corners
  (setq-default left-margin-width 1 right-margin-width 1)
  (set-window-buffer nil (current-buffer))
  ;; prevent emacs from buffering
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  ;; set font
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))
  (set-face-attribute 'default t :font "JetBrains Mono-11")
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
  ;; autosave
  (setq auto-save-default t)
  ;; better backups
  (setq backup-directory-alist '(("." . "~/.emacs-backups/"))
	backup-by-copying t
	version-control t
	delete-old-versions t
	kept-new-versions 20
	kept-old-versions 5)
  ;; delete trailing whitespace
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; dashboard
(use-package dashboard
  :after
  all-the-icons
  page-break-lines
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
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-week-agenda nil)
  (setq dashboard-items '((recents . 5)
			  (agenda . 10)))
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
  (solaire-global-mode +1))

;; all-the-icons
(use-package all-the-icons
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

;; page-break-lines
(use-package page-break-lines)

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
        completion-category-overrides '((file (styles partial-completion)))))

;; which-key
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-allow-multiple-replacements t))

;; spaceline
(use-package spaceline
  :init
  (setq powerline-default-separator 'bar)
  (spaceline-emacs-theme))

;; company
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'global-company-mode-hook #'company-tng-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; lsp-mode
(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil))
(use-package lsp-ui)

;; tree-sitter
(use-package tree-sitter
  :after
  tree-sitter-langs
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

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
  (doom-snippets :type git :host github :repo "hlissner/doom-snippets")
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
  ;; :straight f
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
  (global-undo-tree-mode 1))

;; treemacs
(use-package treemacs
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

(use-package org-wild-notifier
  :config
  (org-wild-notifier-mode))

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

;; pdf-tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install-noverify)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

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

;; format-all
;; check https://github.com/lassik/emacs-format-all-the-code#supported-languages
(use-package format-all
  :config
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter)
  (add-hook 'prog-mode-hook 'format-all-mode))

;; language configuration

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
;; requires nixfmt, rnix-lsp
(use-package lsp-mode
  :after
  nix-mode
  :init
  (add-hook 'nix-mode-hook 'lsp))
(use-package nix-mode
  :interpreter
  ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode
  "\\.nix\\'")
(use-package nix-update)
(use-package company-nixos-options)

;; verilog
;; requires pip3 install hdl-checker
(use-package lsp-mode
  :init
  (add-hook 'verilog-mode-hook 'lsp)
  (add-hook 'vhdl-mode 'lsp))

;;; init.el ends here
