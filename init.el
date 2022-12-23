;; init.el --- haoxiangliew's Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my personal Emacs configuration

;;; Dependencies:
;; git

;;; Code:

;; bootstrap straight and use-package
(setq warning-minimum-level :emergency)
(setq straight-check-for-modifications 'live-with-find
      straight--native-comp-available t
      straight-cache-autoloads t
      straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1
      straight-recipes-gnu-elpa-use-mirror t)
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

;; gcmh
(use-package gcmh
  :init
  (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 'auto
	gcmh-idle-delay-factor 10
	gcmh-high-cons-threshold (* 16 1024 1024))) ; 16mb

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
  ;; speed up load-file
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))
  (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
    (advice-remove #'load-file #'load-file@silence))
  ;; much safer networking
  (setq gnutls-verify-error noninteractive
	gnutls-algorithm-priority
	(when (boundp 'libgnutls-version)
	  (concat "SECURE128:+SECURE192:-VERS-ALL"
		  (if (>= libgnutls-version 30605)
		      ":+VERS-TLS1.3")
		  ":+VERS-TLS1.2"))
	gnutls-min-prime-bits 3072
	tls-checktrust gnutls-verify-error
	tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
		      "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
		      "gnutls-cli -p %p %h"))
  (setq ffap-machine-p-known 'reject)
  ;; load secrets
  (defun load-if-exists (f)
    (if (file-exists-p (expand-file-name f))
	(load-file (expand-file-name f))))
  (load-if-exists "~/.emacs.d/secrets.el")
  (setq auth-sources '("~/.authinfo"))
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 10)
     (internal-border-width . 10)))
  (dolist (face '(window-divider
		  window-divider-first-pixel
		  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))
  ;; configure scratch
  (setq initial-scratch-message (concat
				 ";; Welcome " user-login-name " to Emacs " emacs-version "\n"
				 ";; [INFO] Emacs loaded " (format "%s" (hash-table-count straight--profile-cache)) " packages in " (emacs-init-time "%s seconds") " with " (format "%s" gcs-done) " garbage collections." "\n\n"))
  :config
  ;; username and email
  (setq user-full-name "Hao Xiang Liew"
	user-mail-address "haoxiangliew@gmail.com")
  ;; font
  (add-to-list 'default-frame-alist '(font . "Monospace-10.5"))
  (set-face-attribute 'default nil :font "Monospace-10.5")
  (set-face-attribute 'fixed-pitch nil :font "Monospace-10.5")
  (set-face-attribute 'variable-pitch nil :font "Sans-10.5")
  (setq inhibit-compacting-font-caches t)
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
  (defvar +word-wrap-visual-modes '(org-mode))
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
  (pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-interpolate-page t
	pixel-scroll-precision-use-momentum t
	pixel-scroll-precision-momentum-seconds 0.1)
  (setq fast-but-imprecise-scrolling t)
  (setq redisplay-skip-fontification-on-input t)
  ;; disable bells
  (setq ring-bell-function 'ignore)
  ;; change yes/no to y/n
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; optimize terminal use
  (setq xterm-set-window-title t
	visible-cursor nil)
  ;; increase process throughput
  (setq read-process-output-max (* 1024 1024))
  ;; optimize frames
  (setq frame-resize-pixelwise t
	pgtk-wait-for-event-timeout 0.001)
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  ;; disable flashing cursor
  (blink-cursor-mode 0)
  ;; disable bidirectional text scanning
  (setq-default bidi-display-reordering 'left-to-right
		bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t))

;; esup
(use-package esup
  :config
  (setq esup-depth 0))

;; tramp
(use-package tramp
  :straight (:type built-in)
  :config
  (setq tramp-verbose 0
	tramp-auto-save-directory (expand-file-name "tramp/autosave" user-emacs-directory)
	tramp-chunksize 2000
	tramp-use-ssh-controlmaster-options nil))

;; doom-themes
(use-package doom-themes
  :config
  ;; (defun current-doom-theme ()
  ;;   "Return the currently used doom theme"
  ;;   (car
  ;;    (seq-filter
  ;;     (lambda (theme)
  ;; 	(string-match-p "^doom" (symbol-name theme)))
  ;;     custom-enabled-themes)))
  (defun load-dark-theme ()
    "Load dark theme and disable light theme"
    (interactive)
    ;; (disable-theme 'doom-solarized-light)
    (load-theme 'doom-dracula t))
  ;; (defun load-light-theme ()
  ;;   "Load light theme and disable dark theme"
  ;;   (interactive)
  ;;   (disable-theme 'doom-solarized-dark)
  ;;   (load-theme 'doom-solarized-light t))
  ;; (defun doom-themes-load-prompt ()
  ;;   "Helper for toggle-themes"
  ;;   (let ((theme
  ;; 	   (intern
  ;; 	    (completing-read "Load Doom theme (will disable all others): "
  ;; 			     '(doom-solarized-light doom-solarized-dark) nil t))))
  ;;     (mapc #'disable-theme custom-enabled-themes)
  ;;     (pcase theme
  ;; 	('doom-solarized-dark (load-dark-theme))
  ;; 	('doom-solarized-light (load-light-theme)))))
  ;; (defun toggle-themes ()
  ;;   "Toggle between solarized dark and light"
  ;;   (interactive)
  ;;   (pcase (current-doom-theme)
  ;;     ('doom-solarized-dark (load-light-theme))
  ;;     ('doom-solarized-light (load-dark-theme))
  ;;     (_ (doom-themes-load-prompt))))
  ;; (define-key global-map (kbd "<f5>") #'toggle-themes)
  ;; (defun auto-theme ()
  ;;   (run-at-time "07:00" (* 60 60 24) (lambda () (load-light-theme)))
  ;;   (run-at-time "19:00" (* 60 60 24) (lambda () (load-dark-theme))))
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t
	doom-themes-padded-modeline t)
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'(lambda () (load-dark-theme)))
    (load-dark-theme))
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; modus-themes
;; (use-package modus-themes
;;   :bind
;;   ("<f5>" . modus-themes-toggle)
;;   :init
;;   (setq modus-themes-italic-constructs t
;; 	modus-themes-variable-pitch-headings t
;; 	modus-themes-variable-pitch-ui nil
;; 	modus-themes-org-agenda '((header-block . (variable-pitch scale-title))
;; 				  (header-date . (grayscale bold-all)))
;; 	modus-themes-org-blocks '(grayscale)
;; 	modus-themes-mode-line '(borderless)
;; 	modus-themes-region '(bg-only no-extend))
;;   (modus-themes-load-themes)
;;   :config
;;   (defun auto-theme ()
;;     (modus-themes-load-operandi)
;;     (run-at-time "07:00" (* 60 60 24) (lambda () (modus-themes-load-operandi)))
;;     (modus-themes-load-vivendi)
;;     (run-at-time "19:00" (* 60 60 24) (lambda () (modus-themes-load-vivendi))))
;;   (if (daemonp)
;;       (add-hook 'after-init-hook #'(lambda () (auto-theme)))
;;     (auto-theme)))

;; solaire-mode
(use-package solaire-mode
  :config
  (add-to-list 'solaire-mode-themes-to-face-swap "^doom-")
  ;; (add-to-list 'solaire-mode-themes-to-face-swap "^modus-")
  (solaire-global-mode +1))

;; doom-modeline
(use-package doom-modeline
  :init
  (defun doom-modeline-conditional-buffer-encoding ()
    "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
    (setq-local doom-modeline-buffer-encoding
		(unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
				   '(coding-category-undecided coding-category-utf-8))
			     (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
		  t)))
  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
  (doom-modeline-mode 1)
  :config
  ;; (setq doom-modeline-height 30)
  (column-number-mode)
  (size-indication-mode))

;; all-the-icons
(use-package all-the-icons
  :if
  (display-graphic-p)
  :config
  (if ((find-font (font-spec :name "all-the-icons")))
      (message "all-the-icons is installed!")
    (and (all-the-icons-install-fonts) (restart-emacs))))
(use-package all-the-icons-completion
  :after
  all-the-icons
  :init
  (all-the-icons-completion-mode))

;; consult
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format))

;; vertico
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
		     :includes (vertico-mouse))
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
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
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

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
  (add-hook 'rainbow-mode-hook (hl-line-mode (if rainbow-mode -1 +1))))

;; corfu
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
		   :includes (corfu-info
			      corfu-popupinfo))
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :init
  (setq corfu-cycle t)
  (setq corfu-preselect-first nil)
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
  (setq corfu-popupinfo-delay 0.5)
  (corfu-popupinfo-mode)
  :config
  (defun basic-limited-all-completions (string table pred point)
    (when (length< string 4)
      (completion-emacs21-all-completions string table pred point)))
  (defun basic-limited-try-completion (string table pred point)
    (when (length< string 4)
      (completion-emacs21-try-completion string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-limited
		 basic-limited-try-completion
		 basic-limited-all-completions
		 "Limited basic completion."))
  (setq corfu-auto t
	corfu-auto-delay 0
	corfu-auto-prefix 0))
(use-package corfu-terminal
  :straight (corfu-terminal :type git
			    :repo "https://codeberg.org/akib/emacs-corfu-terminal")
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))
(use-package kind-icon
  :after
  corfu
  :init
  (setq kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package pcmpl-args)

;; cape
(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
	 ("C-c p t" . complete-tag)        ;; etags
	 ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
	 ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-symbol)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p i" . cape-ispell)
	 ("C-c p l" . cape-line)
	 ("C-c p w" . cape-dict)
	 ("C-c p \\" . cape-tex)
	 ("C-c p _" . cape-tex)
	 ("C-c p ^" . cape-tex)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-triggers-in-field t))
(use-package doom-snippets
  :straight (doom-snippets :type git
			   :host github
			   :repo "hlissner/doom-snippets"
			   :files ("*.el" "*"))
  :after
  yasnippet)

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
	image-dired-dir (expand-file-name "image-dired/" user-emacs-directory)
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

;; ansi-color
(use-package ansi-color
  :straight (:type built-in)
  :config
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook
	    #'colorize-compilation-buffer)
  (defun regexp-alternatives (regexps)
    "Return the alternation of a list of regexps."
    (mapconcat (lambda (regexp)
		 (concat "\\(?:" regexp "\\)"))
	       regexps "\\|"))
  (defvar non-sgr-control-sequence-regexp nil
    "Regexp that matches non-SGR control sequences.")
  (setq non-sgr-control-sequence-regexp
	(regexp-alternatives
	 '(;; icon name escape sequences
	   "\033\\][0-2];.*?\007"
	   ;; non-SGR CSI escape sequences
	   "\033\\[\\??[0-9;]*[^0-9;m]"
	   ;; noop
	   "\012\033\\[2K\033\\[1F"
	   )))
  (defun filter-non-sgr-control-sequences-in-region (begin end)
    (save-excursion
      (goto-char begin)
      (while (re-search-forward
	      non-sgr-control-sequence-regexp end t)
	(replace-match ""))))
  (defun filter-non-sgr-control-sequences-in-output (ignored)
    (let ((start-marker
	   (or comint-last-output-start
	       (point-min-marker)))
	  (end-marker
	   (process-mark
	    (get-buffer-process (current-buffer)))))
      (filter-non-sgr-control-sequences-in-region
       start-marker
       end-marker)))
  (add-hook 'comint-output-filter-functions
	    'filter-non-sgr-control-sequences-in-output))

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

;; project-x
(use-package project-x
  :straight
  (project-x
   :type git
   :host github
   :repo "karthink/project-x")
  :after project
  :config
  (project-x-mode 1))

;; deadgrep
(use-package deadgrep)

;; treemacs
(use-package treemacs
  :bind
  ("C-<tab>" . treemacs)
  :init
  (setq treemacs-follow-after-init t
	treemacs-is-never-other-window t
	treemacs-sorting 'alphabetic-case-insensitive-asc
	treemacs-persist-file (expand-file-name "treemacs-persist" user-emacs-directory)
	treemacs-last-error-persist-file (expand-file-name "treemacs-last-error-persist" user-emacs-directory)))
(use-package treemacs-magit
  :after
  (treemacs magit)
  :config
  (setq treemacs-git-mode 'simple))

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
(use-package diff-hl
  :hook (find-file    . diff-hl-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :config
  (add-hook 'diff-hl-mode-on-hook
            (lambda ()
              (unless (window-system)
		(diff-hl-margin-local-mode))))
  (setq diff-hl-disable-on-remote t)
  (setq vc-git-diff-switches '("--histogram"))
  (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; highlight-indent-guides
(use-package highlight-indent-guides
  :hook
  ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-responsive 'top
	highlight-indent-guides-auto-character-face-perc 50
	highlight-indent-guides-auto-top-character-face-perc 300)
  :config
  (defun disable-indent-guides ()
    (and highlight-indent-guides-mode
	 (bound-and-true-p org-indent-mode)
	 (highlight-indent-guides-mode -1)))
  (add-hook 'org-mode-hook #'disable-indent-guides))

;; ligatures
(use-package ligature
  :straight (ligature :type git
		      :host github
		      :repo "mickeynp/ligature.el")
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

;; hl-todo
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces '(("TODO" warning bold)
				("FIXME" error bold)
				("REVIEW" font-lock-keyword-face bold)
				("HACK" font-lock-constant-face bold)
				("DEPRECATED" font-lock-doc-face bold)
				("NOTE" success bold)
				("BUG" error bold)
				("XXX" font-lock-constant-face bold))))

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
  (add-to-list 'org-export-backends 'md)
  (setq org-startup-indented t)
  (setq org-directory "~/haoxiangliew/org")
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-include-deadlines t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-tags-column 100))
(use-package ox-gfm)
(use-package ox-pandoc
  :config
  (add-to-list 'org-export-backends 'pandoc))
(use-package org-super-agenda
  :init
  (org-super-agenda-mode)
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
					 :order 7))))
(use-package org-download
  :config
  (setq org-download-method 'directory
	org-download-image-dir "images"))
(use-package org-modern
  :init
  (global-org-modern-mode)
  :config
  (setq org-modern-label-border nil))

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
;; (use-package elcord
;;   :init
;;   (elcord-mode)
;;   :config
;;   (setq elcord-use-major-mode-as-main-icon t))

;; notmuch
(use-package notmuch
  :bind
  ("C-x C-m" . notmuch-hello)
  :init
  (setq +notmuch-mail-folder "~/.mail/")
  (setq +notmuch-sync-backend "notmuch new")
  (setq-default notmuch-search-oldest-first nil)
  :config
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
  (setq smtp-accounts '(("haoxiangliew@gmail.com" "smtp.gmail.com" 465 "haoxiangliew@gmail.com")
			("haoxiangliew@vt.edu" "smtp.gmail.com" 465 "haoxiangliew@vt.edu")))
  (defun set-smtp-server-message-send-and-exit ()
    "Set SMTP server from list of multiple ones and send mail."
    (interactive)
    (message-remove-header "X-Message-SMTP-Method") ;; Remove. We always determine it by the From field
    (let ((sender
	   (message-fetch-field "From")))
      (cl-loop for (addr server port usr) in smtp-accounts
	       when (string-match addr sender)
	       do (message-add-header (format "X-Message-SMTP-Method: smtp %s %d %s" server port usr)))
      (let ((xmess
	     (message-fetch-field "X-Message-SMTP-Method")))
	(if xmess
	    (progn
	      (message (format "Sending message using '%s' with config '%s'" sender xmess))
	      (message-send-and-exit))
	  (error "Could not find SMTP Server for this Sender address: %s. You might want to correct it or add it to the SMTP Server list 'smtp-accounts'" sender)))))
  (defun local-compose-mode()
    "Keys for compose mode."
    (local-set-key (kbd "C-c C-c") 'set-smtp-server-message-send-and-exit))
  (add-hook 'message-setup-hook 'local-compose-mode)
  (setq notmuch-show-log nil
	notmuch-hello-sections `(notmuch-hello-insert-saved-searches
				 notmuch-hello-insert-alltags)
	notmuch-message-headers-visible nil))

;; language configuration

;; tree-sitter
(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;; apheleia
;; check (describe-variable (apheleia-formatters))
(use-package apheleia
  :init
  (apheleia-global-mode)
  :config
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (setq apheleia-remote-algorithm 'remote))

;; eglot
;; check https://github.com/joaotavora/eglot#connecting-to-a-server
(use-package eglot
  :straight (:type built-in)
  :init
  (add-hook 'prog-mode-hook (lambda ()
			      (cond ((derived-mode-p 'emacs-lisp-mode) (ignore))
				    (t (eglot-ensure)))))
  (add-hook 'markdown-mode 'eglot-ensure)
  :config
  (setq eglot-sync-connect 1
	eglot-connect-timeout 10
	eglot-autoshutdown t
	eglot-send-changes-idle-time 0.5
	eglot-extend-to-xref t))

;; copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :init
  (setq copilot-node-executable (replace-regexp-in-string "[()]" "" (format "%s" (file-expand-wildcards "/nix/store/*-nodejs-16*/bin/node"))))
  :config
  (setq copilot-idle-delay 2)
  (defun copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
	(indent-for-tab-command)))
  (with-eval-after-load 'copilot
    (define-key copilot-mode-map (kbd "<tab>") #'copilot-tab)))

;; flymake
;; check https://www.emacswiki.org/emacs/FlyMake#h5o-2
(use-package flymake
  :bind
  ("C-c ! c" . flymake-start)
  ("C-c ! l" . flymake-show-buffer-diagnostics)
  ("C-c ! n" . flymake-goto-next-error)
  ("C-c ! p" . flymake-goto-prev-error)
  :init
  (add-hook 'prog-mode-hook 'flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

;; arduino-mode
(use-package arduino-mode
  :mode
  "\\.ino\\'"
  :config
  (setq arduino-tab-width 4))

;; cc-mode
(use-package cc-mode
  :straight (:type built-in)
  :mode
  ("\\.tpp\\'" . c++-mode)
  ("\\.txx\\'" . c++-mode)
  :config
  (add-to-list 'eglot-server-programs
	       '((c-mode c++-mode cc-mode)
		 . ("clangd"
		    "-j=8"
		    "--malloc-trim"
		    "--background-index"
		    "--clang-tidy"
		    "--completion-style=detailed"
		    "--pch-storage=memory"))))
(use-package cmake-mode)

;; go-mode
(use-package go-mode
  :mode
  "\\.go\\'")

;; lua-mode
(use-package lua-mode
  :mode
  "\\.lua\\'")

;; markdown-mode
(use-package markdown-mode
  :mode
  ("README\\.md\\'" . gfm-mode)
  "\\.md\\'")

;; matlab-mode
(use-package matlab-mode
  :mode
  "\\.m\\'"
  :config
  (setq matlab-indent-function t))

;; nix-mode
(use-package nix-mode
  :mode
  "\\.nix\\'")

;; rust-mode
(use-package rust-mode
  :mode
  "\\.rs\\'")

;; udev-mode
(use-package udev-mode
  :mode
  "\\.rules\\'")

;; verilog-mode
(use-package verilog-mode
  :mode
  "\\.v\\'"
  :init
  (add-to-list 'eglot-server-programs '(verilog-mode "verible-verilog-ls"))
  (push '(verible-verilog-format . ("verible-verilog-format"
				    filepath))
	apheleia-formatters)
  (add-to-list 'apheleia-mode-alist '(verilog-mode . verible-verilog-format))
  :config
  (defun verilog-config-hook ()
    (setq indent-tabs-mode nil
	  tab-width 2))
  (add-hook 'verilog-mode-hook 'verilog-config-hook))

;; yaml-mode
(use-package yaml-mode
  :mode
  "\\.yaml\\'")

;;; init.el ends here
