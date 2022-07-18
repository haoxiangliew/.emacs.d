;;; init.el --- haoxiangliew's Emacs configuration

;;; Commentary:
;; This is my personal Emacs configuration

;;; Dependencies:
;; git
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
(straight-use-package 'use-package)
(use-package straight
  :config
  (setq straight-use-package-by-default t
	straight-vc-git-default-clone-depth 1
	straight-recipes-gnu-elpa-use-mirror t))

;; no littering
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(use-package no-littering)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; emacs config
(use-package emacs
  :straight (:type built-in)
  :init
  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
        native-comp-async-report-warnings-errors nil
        load-prefer-newer t)
  ;; load secrets
  (setq auth-sources '("~/.authinfo"))
  ;; configure scratch
  (setq initial-scratch-message (concat ";; Welcome " user-login-name " to Emacs " (format "%s" emacs-major-version) "." (format "%s" emacs-minor-version) "\n" (format ";; *** Emacs loaded in %s with %d garbage collections." (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done) "\n\n"))
  :config
  ;; username and email
  (setq user-full-name "Hao Xiang Liew"
        user-mail-address "haoxiangliew@gmail.com")
  ;; font
  (add-to-list 'default-frame-alist '(font . "Cascadia Code-10.5:weight=normal"))
  (set-face-attribute 'default t :font "Cascadia Code-10.5:weight=normal")
  ;; highlight and match parentheses
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  ;; autosave
  (setq auto-save-default t)
  ;; delete trailing whitespace
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; use system clipboard
  (setq select-enable-clipboard t)
  ;; raise undo limit
  (setq undo-limit 80000000) ; 80mb
  ;; intelligent word-wrap
  (defvar +word-wrap-extra-indent 'double)
  (defvar +word-wrap-disabled-modes '(fundamental-mode so-long-mode))
  (defvar +word-wrap-text-modes '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode rst-mode latex-mode LaTeX-mode))
  (when (memq 'visual-line-mode text-mode-hook)
    (remove-hook 'text-mode-hook #'visual-line-mode)
    (add-hook 'text-mode-hook #'+word-wrap-mode))
  ;; fix scrolling
  (setq hscroll-margin 1
        scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 2)
  (setq fast-but-imprecise-scrolling t)
  ;; disable bells
  (setq ring-bell-function 'ignore)
  ;; change yes/no to y/n
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; optimize terminal use
  (setq xterm-set-window-title t
        visible-cursor nil)
  ;; optimize frames
  (setq frame-resize-pixelwise t
        pgtk-wait-for-event-timeout 0.001)
  ;; prevent emacs from buffering
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  (setq pgtk-wait-for-event-timeout 0.001)
  ;; disable flashing cursor
  (blink-cursor-mode 0)
  ;; disable bidirectional text scanning
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right))

;; gcmh
(use-package gcmh
  :init
  (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 'auto
	gcmh-idle-delay-factor 10
	gcmh-high-cons-threshold (* 16 1024 1024))) ; 16mb

;; tramp
(use-package tramp
  :config
  (add-to-list 'tramp-connection-properties (list (regexp-quote "/ssh:haoxiangliew:")
						  "direct-async-processs" t))
  (setq tramp-verbose 0
	tramp-auto-save-directory (expand-file-name "tramp/autosave" user-emacs-directory)
	tramp-chunksize 2000
	tramp-use-ssh-controlmaster-options nil))

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

;; doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (column-number-mode)
  (size-indication-mode))

;; restart-emacs
(use-package restart-emacs)

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

;; vertico
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-mouse))
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (vertico-mode)
  (vertico-mouse-mode))
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
  (setq which-key-idle-delay 0.5
        which-key-allow-multiple-replacements t))

;; rainbow-mode
(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode)
  (add-hook 'rainbow-mode-hook (hl-line-mode (if rainbow-mode -1 +1))))

;; corfu
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
		   :includes (corfu-info))
  :custom
  (corfu-cycle t)
  (corfu-preselect-first nil)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)
  (setq tab-always-indent 'complete)
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 0
        completion-styles '(basic)))
(use-package kind-icon
  :after
  corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package corfu-doc
  :init
  (add-hook 'corfu-mode-hook #'corfu-doc-mode))
(use-package pcmpl-args)

;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-triggers-in-field t))
(use-package doom-snippets
  :straight
  (doom-snippets
   :type git
   :host github
   :repo "hlissner/doom-snippets")
  :after
  yasnippet)

;; dirvish
(use-package dirvish
  :straight (dirvish :files (:defaults "extensions/*")
		     :includes (dirvish-menu
				dirvish-yank
				dirvish-peek
				dirvish-vc
				dirvish-extras
				dirvish-icons
				dirvish-side))
  :bind
  (("C-<tab>" . dirvish-side)
   :map dired-mode-map
   ("h" . dired-up-directory)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("l" . dired-find-file)
   ("i" . wdired-change-to-wdired-mode)
   ("." . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("s"   . dirvish-quicksort)
   ("?"   . dirvish-dispatch)
   ("TAB" . dirvish-subtree-toggle)
   ("SPC" . dirvish-history-jump)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  :config
  (dirvish-peek-mode))

;; pdf-tools
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install-noverify)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

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

;; vterm
(use-package vterm
  :bind
  ("C-x C-t" . vterm)
  ;; if vterm is installed through nix
  :straight f
  :config
  (add-to-list 'vterm-eval-cmds '("magit-status" magit-status))
  (add-to-list 'vterm-eval-cmds '("magit-clone" magit-clone))
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000))

;; ibuffer
(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold))))
(use-package all-the-icons-ibuffer
  :hook
  (ibuffer-mode . all-the-icons-ibuffer-mode))

;; undo-fu
(use-package undo-fu)
(use-package undo-fu-session
  :init
  (global-undo-fu-session-mode))
(use-package vundo
  :bind
  ("C-x u" . vundo))

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
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-responsive 'top
	highlight-indent-guides-delay 0))

;; ligatures
(use-package ligature
  :straight
  (ligature :type git :host github :repo "mickeynp/ligature.el")
  :init
  (global-prettify-symbols-mode +1)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
				       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
				       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
				       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
				       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
				       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
				       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
				       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
				       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
				       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
				       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
				       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
				       "\\\\" "://"))
  (global-ligature-mode t))

;; multiple-cursors
(use-package multiple-cursors
  :bind
  ("C-c c" . mc/edit-lines)
  ("C-c <mouse-1>" . mc/add-cursor-on-click))

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

;; elcord
(use-package elcord
  :init
  (elcord-mode)
  :config
  (setq elcord-use-major-mode-as-main-icon t))

;; notmuch
(use-package notmuch
  :bind
  ("C-x C-m" . notmuch-hello)
  :init
  (setq +notmuch-mail-folder "~/.mail/")
  (setq +notmuch-sync-backend "notmuch new")
  (setq-default notmuch-search-oldest-first nil)
  (defun set-smtp-server-message-send-and-exit ()
    "Set SMTP server from list of multiple ones and send mail."
    (interactive)
    (message-remove-header "X-Message-SMTP-Method") ;; Remove. We always determine it by the From field
    (let ((sender
           (message-fetch-field "From")))
      (loop for (addr server port usr) in smtp-accounts
            when (string-match addr sender)
            do (message-add-header (format "X-Message-SMTP-Method: smtp %s %d %s" server port usr)))
      (let ((xmess
             (message-fetch-field "X-Message-SMTP-Method")))
        (if xmess
            (progn
              (message (format "Sending message using '%s' with config '%s'" sender xmess))
              (message-send-and-exit))
          (error "Could not find SMTP Server for this Sender address: %s. You might want to correct it or add it to the SMTP Server list 'smtp-accounts'" sender)))))
  (defun local-gnus-compose-mode()
    "Keys for gnus compose mode."
    (local-set-key (kbd "C-c C-c") 'set-smtp-server-message-send-and-exit))
  (add-hook 'gnus-message-setup-hook 'local-gnus-compose-mode)
  :config
  (setq smtp-accounts '(("haoxiangliew@gmail.com" "smtp.gmail.com" 465 "haoxiangliew@gmail.com")
			("haoxiangliew@vt.edu" "smtp.gmail.com" 465 "haoxiangliew@vt.edu")))
  (setq notmuch-fcc-dirs nil
        message-kill-buffer-on-exit t
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
  (setq message-send-mail-function 'message-smtpmail-send-it
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465)
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
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (add-hook 'prog-mode-hook 'format-all-mode))

;; tree-sitter
(use-package tree-sitter
  :init
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

;; github copilot
(use-package copilot
  :straight
  (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :init
  (add-hook 'prog-mode-hook 'copilot-mode)
  :config
  (defun copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
	(indent-for-tab-command)))
  (with-eval-after-load 'copilot
    (define-key copilot-mode-map (kbd "M-<tab>") #'copilot-tab)))

;; eglot
;; check https://github.com/joaotavora/eglot#connecting-to-a-server
(use-package eglot
  :init
  (add-hook 'prog-mode-hook 'eglot-ensure)
  :config
  (setq eglot-sync-connect nil))

;; flymake
;; check https://www.emacswiki.org/emacs/FlyMake#h5o-2
(use-package flymake
  :bind
  ("C-c ! c" . flymake-start)
  ("C-c ! l" . flymake-show-buffer-diagnostics)
  ("C-c ! n" . flymake-goto-next-error)
  ("C-c ! p" . flymake-goto-prev-error)
  :init
  (add-hook 'prog-mode-hook 'flymake-mode))

;; nix-mode
(use-package nix-mode
  :interpreter
  ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode
  "\\.nix\\'")

;; platformio-mode
(use-package platformio-mode
  :mode
  "\\.pio\\'"
  :init
  (add-hook 'c++-mode-hook (lambda () (platformio-conditionally-enable))))

;; udev-mode
(use-package udev-mode
  :mode
  "\\.rules\\'")

;;; init.el ends here
