;; init.el --- haoxiangliew's Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my personal Emacs configuration

;;; Dependencies:
;; git

;;; Code:

;; enable AOT native compilation
(defun elpaca--native-compile (e)
  "Native compile E's package."
  ;; Assumes all dependencies are 'built
  (let ((default-directory (elpaca<-build-dir e)))
    (elpaca--signal e (concat "Native compiling " default-directory) 'native-compilation)
    (elpaca--make-process e
      :name "native-compile"
      :command `(,(elpaca--emacs-path) "-Q" "-L" "."
                 ,@(cl-loop for dep in (elpaca-dependencies (elpaca<-id e) '(emacs))
                            for item = (elpaca-get dep)
                            for build-dir = (and item (elpaca<-build-dir item))
                            when build-dir append (list "-L" build-dir))
                 ;; Inherit eln load-path in child process. Otherwise, default assumed.
                 "--eval" ,(format "%S" `(setq native-comp-eln-load-path ',native-comp-eln-load-path))
                 "--batch" "-f" "batch-native-compile"
                 ,@(directory-files-recursively default-directory "\\.el$"))
      :sentinel (apply-partially #'elpaca--process-sentinel "Native compilation complete" nil))))

(if (fboundp 'native-comp-available-p)
    (setq elpaca-build-steps
	  '(elpaca--clone
	    elpaca--configure-remotes
	    elpaca--checkout-ref
	    elpaca--run-pre-build-commands
	    elpaca--queue-dependencies
	    elpaca--check-version
	    elpaca--link-build-files
	    elpaca--generate-autoloads-async
	    elpaca--byte-compile
	    elpaca--native-compile
	    elpaca--compile-info
	    elpaca--install-info
	    elpaca--add-info-path
	    elpaca--run-post-build-commands
	    elpaca--activate-package)))

;; set elpaca-core-date
(setq elpaca-core-date '(format-time-string "%Y%m%d" (current-time) t))

;; bootstrap elpaca and use-package
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; install use-package
(elpaca use-package)

;; install use-package support
(elpaca elpaca-use-package
  ;; enable :ensure use-package keyword.
  (elpaca-use-package-mode)
  ;; assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; process queues
(elpaca-wait)

;; no-littering
(use-package no-littering
  :config
  (no-littering-theme-backups))
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; load-path
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "CC" "CXX" "LANG" "LC_CTYPE" "LDFLAGS" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

;; emacs config
(use-package emacs
  :hook (prog-mode . electric-pair-mode)
  :ensure nil
  :init
  ;; safer networking
  (setq gnutls-verify-error noninteractive
	gnutls-algorithm-priority (when (boundp 'libgnutls-version)
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
    "Load file if it exists"
    (if (file-exists-p (expand-file-name f))
	(if (fboundp 'native-compile-async)
	    (native-compile-async (expand-file-name f))
	  (load-file expand-file-name f))))
  (load-if-exists "~/.emacs.d/secrets.el")
  (setq auth-sources '("~/.authinfo"))
  (setq auth-source-save-behavior nil)
  ;; configure scratch
  (setq initial-major-mode 'org-mode
	initial-scratch-message (concat
				 "#+TITLE: Welcome " user-login-name " to Emacs " emacs-version "\n"
				 "#+SUBTITLE: Emacs loaded in " (emacs-init-time "%s seconds") " with " (format "%s" gcs-done) " garbage collection(s)." "\n\n"))
  ;; macOS pseudo-daemon
  (when (and (eq system-type 'darwin)
	     (display-graphic-p))
    (defun pseudo-exit (event)
      "Hide Emacs on macOS"
      (call-process
       "osascript" nil nil nil
       "-e" "tell application \"Finder\""
       "-e" "set visible of process \"Emacs\" to false"
       "-e" "end tell"))
    (defun handle-delete-frame-wrapper (orig-fun &rest args)
      "Only pseudo-exit when there is only one frame"
      (if (= 1 (length (frame-list)))
	  (pseudo-exit nil)
	(apply orig-fun args)))
    (advice-add 'handle-delete-frame :around #'handle-delete-frame-wrapper)
    (advice-add 'save-buffers-kill-terminal :override #'pseudo-exit))
  ;; macOS
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'super))
  :config
  ;; username and email
  (setq user-full-name "Hao Xiang Liew"
	user-mail-address "haoxiangliew@gmail.com")
  ;; font
  (if (eq system-type 'darwin)
      (progn (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-12"))
	     (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-12")
	     (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font-12")
	     (set-face-attribute 'variable-pitch nil :font "SF Pro-12"))
    (progn (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-10"))
	   (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-10")
	   (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font-10")
	   (set-face-attribute 'variable-pitch nil :font "SF Pro-10")))
  (setq inhibit-compacting-font-caches t)
  ;; highlight and match parentheses
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  ;; autosave
  (setq auto-save-default t)
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
  (setq mouse-wheel-progressive-speed t)
  (setq fast-but-imprecise-scrolling t)
  (setq redisplay-skip-fontification-on-input t)
  (pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-interpolate-page t
        pixel-scroll-precision-use-momentum t
        pixel-scroll-precision-momentum-seconds 0.1)
  (defun filter-mwheel-always-coalesce (orig &rest args)
    "A filter function suitable for :around advices that ensures only
   coalesced scroll events reach the advised function."
    (if mwheel-coalesce-scroll-events
	(apply orig args)
      (setq mwheel-coalesce-scroll-events t)))
  (defun filter-mwheel-never-coalesce (orig &rest args)
    "A filter function suitable for :around advices that ensures only
   non-coalesced scroll events reach the advised function."
    (if mwheel-coalesce-scroll-events
	(setq mwheel-coalesce-scroll-events nil)
      (apply orig args)))
  ;; Don't coalesce for high precision scrolling
  (advice-add 'pixel-scroll-precision :around #'filter-mwheel-never-coalesce)
  ;; Coalesce for default scrolling (which is still used for horizontal scrolling)
  ;; and text scaling (bound to ctrl + mouse wheel by default).
  (advice-add 'mwheel-scroll          :around #'filter-mwheel-always-coalesce)
  (advice-add 'mouse-wheel-text-scale :around #'filter-mwheel-always-coalesce)
  ;; yes/no -> y/n
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; optimize terminal use
  (setq xterm-set-window-title t
	visible-cursor nil)
  ;; increase process throughput
  (setq read-process-output-max (* 1024 1024)
	process-adaptive-read-buffering nil)
  ;; optimize frames
  (setq frame-resize-pixelwise t
	cursor-in-non-selected-windows nil
	highlight-nonselected-windows nil)
  (if (boundp 'pgtk-wait-for-event-timeout)
      (setq pgtk-wait-for-event-timeout 0.001))
  ;; show tab-bar
  (setq tab-bar-show 1)
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
  :ensure nil)

;; modus-themes
(use-package auto-dark
  :config
  (setq auto-dark-dark-theme nil
	auto-dark-light-theme nil)
  (add-hook 'auto-dark-dark-mode-hook (lambda () (enable-theme 'modus-vivendi)))
  (add-hook 'auto-dark-light-mode-hook (lambda () (enable-theme 'modus-operandi))))
(use-package modus-themes
  :after auto-dark
  :bind
  ("C-c t" . my-modus-themes-toggle)
  :init
  (defun my-modus-themes-toggle ()
    "Toggle between `modus-operandi' and `modus-vivendi' themes.
This uses `enable-theme' instead of the standard method of
`load-theme'.  The technicalities are covered in the Modus themes
manual."
    (interactive)
    (pcase (modus-themes--current-theme)
      ('modus-operandi (progn (enable-theme 'modus-vivendi)
                              (disable-theme 'modus-operandi)))
      ('modus-vivendi (progn (enable-theme 'modus-operandi)
                             (disable-theme 'modus-vivendi)))
      (_ (error "No Modus theme is loaded; evaluate `modus-themes-load-themes' first"))))
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t)
  (load-theme 'modus-operandi t t)
  (load-theme 'modus-vivendi t t)
  :config
  (if (daemonp)
      (add-hook 'after-init-hook #'(lambda () (auto-dark-mode t)))
    (auto-dark-mode t)))

;; doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :config
  (column-number-mode)
  (size-indication-mode))

;; spacious-padding
(use-package spacious-padding
  :config
  (setq spacious-padding-widths '(:internal-border-width 10 :right-divider-width 10 :scroll-bar-width 0)
	spacious-padding-subtle-mode-line t)
  (spacious-padding-mode))

;; nerd-icons
(use-package nerd-icons)
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; vertico
(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*")
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
  (marginalia-mode))

;; which-key
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
	which-key-allow-multiple-replacements t))

;; corfu
(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*")
		 :includes (corfu-info
			    corfu-popupinfo))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  (setq corfu-popupinfo-delay 0.5)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-auto t
	corfu-auto-delay 0
	corfu-auto-prefix 1
	corfu-quit-no-match t
	corfu-cycle t
	corfu-preselect 'prompt)
  ;; minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input)
		(eq (current-local-map) read-passwd-map))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  ;; eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
	      (setq-local corfu-auto nil)
	      (corfu-mode)))
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))
  (advice-add #'corfu-insert :after #'corfu-send-shell))
(use-package corfu-terminal
  :ensure (corfu-terminal :repo "https://codeberg.org/akib/emacs-corfu-terminal")
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))
(use-package pcmpl-args)

;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-triggers-in-field t))
(use-package yasnippet-snippets
  :after yasnippet)

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
  :ensure nil
  :bind
  ("C-x C-e" . eshell)
  :init
  (defun eshell-add-aliases ()
    "Alias for eshell"
    (dolist (var   '(("q"  "exit")
		     ("ff" "find-file $1")
		     ("d"  "dired $1")
		     ("rg" "rg --color=always $*")
		     ("l"  "ls -lh $*")
		     ("ll" "ls -lah $*")
		     ("git" "git --no-pager $*")
		     ("gg" "magit-status")
		     ("cdp" "project-find-file")
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

;; eat
(use-package eat
  :hook ((eshell-load . eat-eshell-mode)
	 (eshell-load . eat-eshell-visual-command-mode))
  :ensure (eat :repo "https://codeberg.org/akib/emacs-eat"
	       :files ("*.el" ("term" "term/*.el") "*.texi"
		       "*.ti" ("terminfo/e" "terminfo/e/*")
		       ("terminfo/65" "terminfo/65/*")
		       ("integration" "integration/*")
		       (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (advice-add #'compilation-start :override #'eat-compilation-start)
  (defun eat-compilation-start (command &optional mode name-function highlight-regexp continue)
    (let ((name-of-mode "compilation")
          (dir default-directory)
          outbuf)
      (if (or (not mode) (eq mode t))
          (setq mode #'compilation-minor-mode)
        (setq name-of-mode (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
      (with-current-buffer
          (setq outbuf
                (get-buffer-create
                 (compilation-buffer-name name-of-mode mode name-function)))
        (setq default-directory dir)
        (setq buffer-read-only nil)
        (erase-buffer)
        (compilation-insert-annotation
         "-*- mode: " name-of-mode
         "; default-directory: "
         (prin1-to-string (abbreviate-file-name default-directory))
         " -*-\n")
        (compilation-insert-annotation
         (format "%s started at %s\n\n"
                 mode-name
	         (substring (current-time-string) 0 19))
         command "\n")
        (eat-mode)
        (eat-exec outbuf "*compile*" shell-file-name nil (list "-lc" command))
        (run-hook-with-args 'compilation-start-hook (get-buffer-process outbuf))
        (eat-emacs-mode)
        (funcall mode)
        (setq next-error-last-buffer outbuf)
        (display-buffer outbuf '(nil (allow-no-window . t)))
        (when-let (w (get-buffer-window outbuf))
          (set-window-start w (point-min)))))))

;; vterm
(use-package vterm
  ;; if vterm is installed via nix
  ;; :ensure nil
  :bind
  ("C-x C-t" . vterm)
  :config
  (defun vterm-add-aliases ()
    "Alias for vterm"
    (dolist (var   '(("gg" magit-status)
		     ("ff" find-file)
		     ("magit-status" magit-status)
		     ("magit-clone" magit-clone)
		     ("cdp" project-find-file)
		     ("project-find-file" project-find-file)))
      (add-to-list 'vterm-eval-cmds var)))
  (add-hook 'vterm-mode-hook 'vterm-add-aliases)
  (setq vterm-tramp-shells '(("docker" "sh")
                             ("scp" "'zsh'")
			     ("scp" "'bash'")
                             ("ssh" "'zsh'")
			     ("ssh" "'bash'")))
  (setq vterm-kill-buffer-on-exit t
	vterm-max-scrollback 5000))

;; dired
(use-package dired
  :ensure nil
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

;; ranger
(use-package ranger
  :after dired
  :config
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir))
  (setq ranger-cleanup-on-disable t
	ranger-excluded-extensions '("mkv" "iso" "mp4")
	ranger-deer-show-details t
	ranger-max-preview-size 10
	ranger-show-literal nil
	ranger-hide-cursor nil))

;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
	ibuffer-filter-group-name-face '(:inherit (success bold))))

;; undo-fu
(use-package undo-fu)
(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode))
(use-package vundo
  :bind
  ("C-x u" . vundo)
  :config
  (defun vundo-diff ()
    (interactive)
    (let* ((orig vundo--orig-buffer)
	   (source (vundo--current-node vundo--prev-mod-list))
	   (dest (vundo-m-parent source)))
      (if (or (not dest) (eq source dest))
          (message "vundo diff not available")
	(let ((buf (make-temp-name (concat (buffer-name orig) "-vundo-diff"))))
          (vundo--move-to-node source dest orig vundo--prev-mod-list)
          (with-current-buffer (get-buffer-create buf)
	    (insert-buffer-substring orig))
          (vundo--refresh-buffer orig (current-buffer) 'incremental)
          (vundo--move-to-node dest source orig vundo--prev-mod-list)
          (vundo--refresh-buffer orig (current-buffer) 'incremental)
          (diff-buffers buf orig)
          (kill-buffer buf)))))
  (define-key vundo-mode-map "d" #'vundo-diff)
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; project.el
(use-package project-x
  :ensure (project-x :repo "https://github.com/karthink/project-x")
  :after project
  :config
  (project-x-mode 1))

;; hl-todo
(use-package hl-todo
  :ensure (hl-todo :depth nil
		   :version (lambda (_) "2.0.0"))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

;; rainbow-mode
(use-package rainbow-mode
  :hook ((css-mode html-mode sass-mode scss-mode web-mode) . rainbow-mode))

;; magit
(use-package transient)
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :init
  (require 'git-commit)
  (setq transient-default-level 5))
(use-package magit-section)
(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))
(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?"))
(use-package git-gutter
  :init
  (if (fboundp 'fringe-mode) (fringe-mode '8))
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode org-mode))
  :config
  (setq git-gutter:handled-backends
	(cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
				     :key #'symbol-name)))
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)
  (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
  (advice-remove #'quit-window #'git-gutter:quit-window)
  (advice-remove #'switch-to-buffer #'git-gutter:switch-to-buffer)
  (defvar git-gutter-last-buffer-and-window nil
    "Cons of current buffer and selected window before last command.
This is used to detect when the current buffer or selected window
changes, which means that `git-gutter' needs to be re-run.")
  (defun git-gutter--on-buffer-or-window-change ()
    "Update `git-gutter' when current buffer or selected window changes."
    (let ((new (cons (current-buffer) (selected-window))))
      (unless (equal new git-gutter-last-buffer-and-window)
        (setq git-gutter-last-buffer-and-window new)
        ;; Sometimes the current buffer has not gotten updated yet
        ;; after switching window, for example after `quit-window'.
        (with-current-buffer (window-buffer)
          (when git-gutter-mode
            (when buffer-file-name
              (unless (file-remote-p buffer-file-name)
                (git-gutter))))))))
  (defun git-gutter--init-maybe ()
    (when (and (buffer-file-name (buffer-base-buffer))
               (file-remote-p buffer-file-name)
               (bound-and-true-p git-gutter-mode))
      (git-gutter-mode)))
  (add-hook 'post-command-hook #'git-gutter--on-buffer-or-window-change)
  (add-hook 'apheleia-post-format-hook #'git-gutter--on-buffer-or-window-change)
  (global-git-gutter-mode +1))

;; indent-bars
(use-package indent-bars
  :ensure (indent-bars :repo "https://github.com/jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-treesit-support t
	indent-bars-prefer-character t
	indent-bars-starting-column 0
	indent-bars-display-on-blank-lines nil))

;; ligatures
(use-package ligature
  :ensure (ligature :repo "https://github.com/mickeynp/ligature.el"
		    :inherit nil)
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode)
  :config
  ;; Enable all JetBrains Mono ligatures
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

;; multiple-cursors
(use-package multiple-cursors
  :bind
  ("C-c c" . mc/edit-lines)
  ("C-c <mouse-1>" . mc/add-cursor-on-click))

;; org-mode
(use-package org
  :ensure nil
  :bind
  ("C-x C-a" . org-agenda)
  :config
  (setq org-startup-indented t)
  (setq org-directory "~/haoxiangliew/org")
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-include-deadlines t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-tags-column 100))
(use-package ox-moderncv
  :ensure (ox-moderncv :repo "https://github.com/haoxiangliew/org-cv")
  :requires ox-moderncv)
(use-package ox-gfm
  :config
  (add-to-list 'org-export-backends 'md))
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
  :after org
  :init
  (global-org-modern-mode)
  :config
  (setq org-modern-label-border nil))

;; elcord
(use-package elcord
  :init
  (elcord-mode)
  :config
  (setq elcord-use-major-mode-as-main-icon t
	elcord--editor-name (concat "Emacs " emacs-version)))

;; notmuch
;; (use-package notmuch
;;   :disabled
;;   :bind
;;   ("C-x C-m" . notmuch-hello)
;;   :init
;;   (setq +notmuch-mail-folder "~/.mail/")
;;   (setq +notmuch-sync-backend "notmuch new")
;;   (setq-default notmuch-search-oldest-first nil)
;;   (setq notmuch-saved-searches
;; 	'((:name "inbox"   :query "tag:inbox not tag:trash" :key "i")
;; 	  (:name "flagged" :query "tag:flagged"             :key "f")
;; 	  (:name "sent"    :query "tag:sent"                :key "s")
;; 	  (:name "drafts"  :query "tag:draft"               :key "d")))
;;   :config
;;   (setq notmuch-fcc-dirs nil
;; 	message-kill-buffer-on-exit t
;; 	notmuch-search-result-format
;; 	'(("date" . "%12s ")
;; 	  ("count" . "%-7s ")
;; 	  ("authors" . "%-30s ")
;; 	  ("subject" . "%-72s ")
;; 	  ("tags" . "(%s)"))
;; 	notmuch-tag-formats
;; 	'(("unread" (propertize tag 'face 'notmuch-tag-unread)))
;; 	notmuch-archive-tags '("-inbox" "-unread"))
;;   (setq message-send-mail-function 'message-smtpmail-send-it
;; 	smtpmail-smtp-server "smtp.gmail.com"
;; 	smtpmail-stream-type 'ssl
;; 	smtpmail-smtp-service 465)
;;   (setq smtp-accounts '(("haoxiangliew@gmail.com" "smtp.gmail.com" 465 "haoxiangliew@gmail.com")
;; 			("haoxiangliew@vt.edu" "smtp.gmail.com" 465 "haoxiangliew@vt.edu")))
;;   (defun set-smtp-server-message-send-and-exit ()
;;     "Set SMTP server from list of multiple ones and send mail."
;;     (interactive)
;;     (message-remove-header "X-Message-SMTP-Method") ;; Remove. We always determine it by the From field
;;     (let ((sender
;; 	   (message-fetch-field "From")))
;;       (cl-loop for (addr server port usr) in smtp-accounts
;; 	       when (string-match addr sender)
;; 	       do (message-add-header (format "X-Message-SMTP-Method: smtp %s %d %s" server port usr)))
;;       (let ((xmess
;; 	     (message-fetch-field "X-Message-SMTP-Method")))
;; 	(if xmess
;; 	    (progn
;; 	      (message (format "Sending message using '%s' with config '%s'" sender xmess))
;; 	      (message-send-and-exit))
;; 	  (error "Could not find SMTP Server for this Sender address: %s. You might want to correct it or add it to the SMTP Server list 'smtp-accounts'" sender)))))
;;   (defun local-compose-mode ()
;;     "Keys for compose mode."
;;     (local-set-key (kbd "C-c C-c") 'set-smtp-server-message-send-and-exit))
;;   (add-hook 'message-setup-hook 'local-compose-mode)
;;   (setq notmuch-show-log nil
;; 	notmuch-hello-sections `(notmuch-hello-insert-saved-searches
;; 				 notmuch-hello-insert-alltags)
;; 	notmuch-message-headers-visible nil)
;;   (defun notmuch-update ()
;;     "Sync notmuch emails with server."
;;     (interactive)
;;     (let ((compilation-buffer-name-function (lambda (_) (format "*notmuch update*"))))
;;       (with-current-buffer (compile (format "%s" +notmuch-sync-backend))
;; 	(add-hook
;; 	 'compilation-finish-functions
;; 	 (lambda (buf status)
;; 	   (if (equal status "finished\n")
;; 	       (progn
;; 		 (delete-windows-on buf)
;; 		 (bury-buffer buf)
;; 		 (notmuch-refresh-all-buffers)
;; 		 (message "Notmuch sync successful"))
;; 	     (user-error "Failed to sync notmuch data")))
;; 	 nil
;; 	 'local))))
;;   (define-key notmuch-search-mode-map "G" 'notmuch-update))

;;; language configuration

;; tree-sitter
(use-package treesit-auto
  :config
  (setq treesit-auto-install 't)
  (treesit-auto-install-all)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; apheleia (formatter)
;; check (describe-variable (apheleia-formatters))
(use-package apheleia
  :init
  (setq require-final-newline t
	show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (apheleia-global-mode)
  :config
  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
    (with-current-buffer scratch
      (setq-local eglot--cached-server
                  (with-current-buffer buffer
                    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
        (eglot-format-buffer))
      (funcall callback)))
  (add-to-list 'apheleia-formatters
	       '(eglot-managed . apheleia-indent-eglot-managed-buffer))
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (setq apheleia-remote-algorithm 'remote))

;; eglot
;; check https://github.com/joaotavora/eglot#connecting-to-a-server
(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure)))))
  :init
  (fset 'jsonrpc--log-event 'ignore)
  :config
  (setf (plist-get eglot-events-buffer-config :size) 0)
  (setq eglot-sync-connect 0
	eglot-send-changes-idle-time 3
	eglot-autoshutdown t
	eglot-extend-to-xref t))
(use-package eglot-booster
  :ensure (eglot-booster :repo "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

;; flymake
;; check https://www.emacswiki.org/emacs/FlyMake#h5o-2
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind
  ("C-c ! c" . flymake-start)
  ("C-c ! l" . flymake-show-buffer-diagnostics)
  ("C-c ! n" . flymake-goto-next-error)
  ("C-c ! p" . flymake-goto-prev-error)
  :config
  (setq flymake-no-changes-timeout 3))

;; flyspell
(use-package flyspell
  :ensure nil
  :hook (((org-mode markdown-mode TeX-mode rst-mode mu4e-compose-mode message-mode git-commit-mode) . flyspell-mode)
	 ((yaml-mode conf-mode prog-mode) . flyspell-prog-mode))
  :config
  (setq flyspell-issue-welcome-flag nil
	flyspell-issue-message-flag nil))
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
(use-package flyspell-lazy
  :after flyspell
  :config
  (setq flyspell-lazy-idle-seconds 1
	flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

;; copilot
(use-package copilot
  :ensure (copilot :repo "https://github.com/copilot-emacs/copilot.el"
		   :files ("dist" "*.el"))
  :hook ((prog-mode . copilot-turn-on-unless-buffer-read-only)
	 (emacs-lisp-mode . (lambda ()
			      (setq-local copilot--indent-warning-printed-p t))))
  :bind (("C-c h" . copilot-mode)
	 (:map copilot-completion-map
	       ("C-g" . 'copilot-clear-overlay)
	       ("<tab>" . 'copilot-tab)
	       ("TAB" . 'copilot-tab)))
  :init
  (defun copilot-turn-on-unless-buffer-read-only ()
    "Turn on `copilot-mode' if the buffer is writable."
    (unless (or buffer-read-only (not (buffer-file-name (current-buffer))))
      (copilot-mode 1)))
  :config
  (setq copilot-indent-offset-warning-disable t)
  (defun copilot-tab ()
    "Copilot completion for tab"
    (interactive)
    (if (copilot--overlay-visible)
	(progn
	  (copilot-accept-completion))
      (copilot-complete))))

;; envrc
(use-package envrc
  :config
  (envrc-global-mode))

;; applescript-mode
(use-package applescript-mode)

;; arduino-mode
(use-package arduino-mode
  :mode
  "\\.ino\\'"
  :config
  (setq arduino-tab-width 4))

;; cc-mode
(use-package cc-mode
  :after eglot apheleia
  :ensure nil
  :mode
  ("\\.tpp\\'" . c++-mode)
  ("\\.txx\\'" . c++-mode)
  :config
  (add-to-list 'eglot-server-programs
	       '((c-mode c++-mode cc-mode)
		 . ("clangd"
		    "-j=20"
		    "--background-index"
		    "--clang-tidy"
		    "--completion-style=detailed"
		    "--pch-storage=memory")))
  (add-to-list 'apheleia-mode-alist '(c-mode . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(c++-mode . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(cc-mode . eglot-managed)))
(use-package cmake-mode
  :ensure (cmake-mode :main "Auxiliary/cmake-mode.el")
  :after eglot apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(cmake-mode . eglot-managed)))

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
;; (use-package matlab-mode
;;   :mode
;;   "\\.m\\'"
;;   :config
;;   (setq matlab-indent-function t))

;; nix-mode
(use-package nix-mode
  :after eglot apheleia
  :mode
  "\\.nix\\'"
  :init
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd"))))

;; rust-mode
(use-package rust-mode
  :mode
  "\\.rs\\'"
  :config
  (add-to-list 'apheleia-mode-alist '(rust-mode . eglot-managed)))

;; udev-mode
(use-package udev-mode
  :mode
  "\\.rules\\'")

;; verilog-mode
(use-package verilog-mode
  :after eglot apheleia
  :mode
  ("\\.v\\'"
   "\\.sv\\'"
   "\\.vh\\'"
   "\\.svh\\'")
  :init
  (add-to-list 'eglot-server-programs '(verilog-mode . ("verible-verilog-ls")))
  (add-to-list 'apheleia-mode-alist '(verilog-mode . eglot-managed))
  (add-hook 'verilog-mode-hook (lambda () (setq indent-tabs-mode nil)))
  :config
  (setq verilog-indent-level 2
	verilog-indent-level-module 2
	verilog-indent-level-directive 2
	verilog-indent-level-behavioral 2
	verilog-indent-level-declaration 2))

;; yaml-mode
(use-package yaml-mode
  :mode
  "\\.yaml\\'")

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; init.el ends here
