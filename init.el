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
    (setq elpaca-build-steps '(elpaca--clone
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

;; bootstrap elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--dxepth=%d" depth) "--no-single-branch"))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; install use-package
(elpaca elpaca-use-package
  ;; enable :ensure use-package keyword
  (elpaca-use-package-mode)
  ;; assume :ensure t unless otherwise specified
  (setq elpaca-use-package-by-default t))

;; no-littering
(use-package no-littering
  :config
  (no-littering-theme-backups))

;; emacs config
(use-package emacs
  :hook (prog-mode . electric-pair-mode)
  :ensure nil
  :config
  ;; macOS
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'super))
  ;; font
  (progn (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-12"))
	 (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-12")
	 (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font-12")
	 (set-face-attribute 'variable-pitch nil :font "SF Pro-12"))
  (setq inhibit-compacting-font-caches t)
  ;; highlight and match parentheses
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  ;; intelligent word-wrap
  (defvar +word-wrap-extra-indent 'double)
  (defvar +word-wrap-disabled-modes '(fundamental-mode so-long-mode))
  (defvar +word-wrap-visual-modes '(org-mode))
  (defvar +word-wrap-text-modes '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode rst-mode latex-mode LaTeX-mode))
  (when (memq 'visual-line-mode text-mode-hook)
    (remove-hook 'text-mode-hook #'visual-line-mode)
    (add-hook 'text-mode-hook #'+word-wrap-mode))
  ;; yes/no -> y/n
  (defalias 'yes-or-no-p 'y-or-n-p)
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
  ;; disable flashing cursor
  (blink-cursor-mode 0)
  ;; disable bidirectional text scanning
  (setq-default bidi-display-reordering 'left-to-right
		bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  ;; autosave
  (setq auto-save-default t)
  ;; use system clipboard
  (setq select-enable-clipboard t)
  ;; raise undo limit
  (setq undo-limit 80000000)
  ;; show tab-bar
  (setq tab-bar-show 1)
  ;; username and email
  (setq user-full-name "Hao Xiang Liew"
	user-mail-address "haoxiangliew@gmail.com")
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
      (setq pgtk-wait-for-event-timeout 0.001)))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config(use-package nerd-icons)
  ;; override doom-dracula
  (advice-add 'def-doom-theme :around
              (lambda (orig-fun name docstring colors &rest body)
                (if (eq name 'doom-dracula)
                    (let ((new-colors
                           (mapcar (lambda (color-def)
                                     (let ((color-name (car color-def)))
                                       (cond
                                        ;;                            name         default   256       16
                                        ((eq color-name 'bg)        '(bg        '("#22212C" "#201F2E" "black")))         ;; bg
                                        ((eq color-name 'bg-alt)    '(bg-alt    '("#201F2E" "#2B293D" "black")))         ;; bg2

                                        ((eq color-name 'base0)     '(base0     '("#22212C" "#201F2E" "black")))         ;; bg
                                        ((eq color-name 'base1)     '(base1     '("#201F2E" "#2B293D" "brightblack")))   ;; bg2
                                        ((eq color-name 'base2)     '(base2     '("#2B293D" "#35334D" "brightblack")))   ;; bg3
                                        ((eq color-name 'base3)     '(base3     '("#36334C" "#3F3D5C" "brightblack")))   ;; bg4
                                        ((eq color-name 'base4)     '(base4     '("#454158" "#433D5C" "brightblack")))   ;; current
                                        ((eq color-name 'base5)     '(base5     '("#7970A9" "#756AAF" "brightblack")))   ;; comment

                                        ((eq color-name 'base6)     '(base6     '("#BABAAB" "#B3B3B3" "brightblack")))   ;; fg4
                                        ((eq color-name 'base7)     '(base7     '("#D6D6C2" "#D1D1C7" "brightblack")))   ;; fg3
                                        ((eq color-name 'base8)     '(base8     '("#F8F8F2" "#F9F9F1" "white")))         ;; fg

                                        ((eq color-name 'fg)        '(fg        '("#F8F8F2" "#F9F9F1" "white")))         ;; fg
                                        ((eq color-name 'fg-alt)    '(fg-alt    '("#EDEDDE" "#EBEBE0" "brightwhite")))   ;; fg2

                                        ((eq color-name 'grey)      '(grey      '("#454158" "#433D5C" "brightblack")))   ;; current
                                        ((eq color-name 'red)       '(red       '("#FF9580" "#F99986" "red")))           ;; red
                                        ((eq color-name 'orange)    '(orange    '("#FFCA80" "#F9C986" "brightred")))     ;; orange
                                        ((eq color-name 'green)     '(green     '("#8AFF80" "#8FF986" "green")))         ;; green
                                        ((eq color-name 'teal)      '(teal      '("#7766CC" "#875FD7" "brightgreen")))   ;; #7766CC, #875FD7
                                        ((eq color-name 'yellow)    '(yellow    '("#FFFF80" "#F9F986" "yellow")))        ;; yellow
                                        ((eq color-name 'blue)      '(blue      '("#8A75F0" "#846EF7" "brightblue")))    ;; alt-blue
                                        ((eq color-name 'dark-blue) '(dark-blue '("#7970A9" "#756AAF" "blue")))          ;; comment
                                        ((eq color-name 'magenta)   '(magenta   '("#FF80BF" "#F986BF" "magenta")))       ;; pink
                                        ((eq color-name 'violet)    '(violet    '("#9580FF" "#9986F9" "brightmagenta"))) ;; purple
                                        ((eq color-name 'cyan)      '(cyan      '("#80FFEA" "#86F9E6" "brightcyan")))    ;; cyan
                                        ((eq color-name 'dark-cyan) '(dark-cyan '("#80FFEA" "#86F9E6" "cyan")))          ;; cyan
                                        (t color-def))))
                                   colors)))
		      (apply orig-fun name docstring new-colors body))
                  (apply orig-fun name docstring colors body))))
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))

;; tramp
(use-package tramp
  :ensure nil)

;; which-key
(use-package which-key
  :ensure nil
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5
	which-key-allow-multiple-replacements t))

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
  (add-hook 'eshell-post-command-hook 'eshell-add-aliases))
(use-package eshell-prompt-extras
  :after eshell
  :config
  (setq eshell-prompt-function #'epe-theme-lambda))

(use-package exec-path-from-shell
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "CC" "CXX" "LANG" "LC_CTYPE" "LDFLAGS" "NIX_SSL_CERT_FILE" "NIX_PATH" "LIBRARY_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
	ibuffer-filter-group-name-face '(:inherit (success bold))))

;; esup
(use-package esup
  :config
  (setq esup-depth 0))

;; doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :config
  (column-number-mode)
  (size-indication-mode))

;; solaire-mode
(use-package solaire-mode
  :config
  (add-to-list 'solaire-mode-themes-to-face-swap "^doom-")
  (solaire-global-mode))

;; spacious-padding
(use-package spacious-padding
  :config
  (setq spacious-padding-widths '(:internal-border-width 10 :right-divider-width 10 :scroll-bar-width 10))
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
  :after ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-dired
  :after dired
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
  (setq context-menu-mode t
	enable-recursive-minibuffers t
	read-extended-command-predicate #'command-completion-default-include-p
	minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (vertico-mode)
  (vertico-mouse-mode))

;; marginalia
(use-package marginalia
  :init
  (marginalia-mode))

;; corfu
(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*")
		 :includes (corfu-info corfu-popupinfo))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (setq tab-always-indent 'complete
	text-mode-ispell-word-completion nil)
  (global-corfu-mode)
  (setq corfu-popupinfo-delay 0.5)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-auto t
	corfu-cycle t
	corfu-quit-no-match t
	corfu-preselect 'prompt)
  ;; eshell
  (add-hook 'eshell-mode-hook (lambda ()
				(setq-local corfu-auto nil)
				(corfu-mode))))
(use-package corfu-terminal
  :ensure (corfu-terminal :repo "https://codeberg.org/akib/emacs-corfu-terminal")
  :unless (display-graphic-p)
  :init
  (corfu-terminal-mode))

;; cape
(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword))

;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode)
  :config
  (setq yas-triggers-in-field t))
(use-package yasnippet-snippets
  :after yasnippet)

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
  (setq vterm-kill-buffer-on-exit t
	vterm-max-scrollback 5000))

;; undo-fu
(use-package undo-fu)
(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode))

;; vundo
(use-package vundo
  :bind
  ("C-x u" . vundo)
  :hook (vundo-mode . vundo-live-diff-mode)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (defun vundo-live-diff-post-command ()
    "Post command hook function for live diffing."
    (when (not (memq this-command '(vundo-quit vundo-confirm)))
      (progn
	(vundo-diff-mark (vundo-m-parent (vundo--current-node vundo--prev-mod-list)))
        (vundo-diff))))
  (define-minor-mode vundo-live-diff-mode
    "Shows live diff between the current node and its parent."
    :lighter nil
    (if vundo-live-diff-mode
	(add-hook 'post-command-hook #'vundo-live-diff-post-command 0 t)
      (remove-hook 'post-command-hook #'vundo-live-diff-post-command t)))
  (define-key vundo-mode-map "d" #'vundo-live-diff-mode))

;; project-x
(use-package project-x
  :ensure (project-x :repo "https://github.com/karthink/project-x")
  :after project
  :config
  (project-x-mode 1))

;; hl-todo
(use-package hl-todo
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
(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1)
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?"))

;; diff-hl
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :config
  (setq diff-hl-global-modes '(not image-mode pdf-view-mode)
	vc-git-diff-switches '("--histogram")
	diff-hl-update-async t
	diff-hl-show-staged-changes nil
	diff-hl-draw-borders nil)
  (defun diff-hl-define-thin-bitmaps ()
    "Define sleek, thin bitmaps for diff-hl that only use half the fringe width."
    (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                           (numberp text-scale-mode-amount))
                      (expt text-scale-mode-step text-scale-mode-amount)
                    1))
           (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
           (h (+ (ceiling (* (frame-char-height) scale))
		 (if (floatp spacing)
                     (truncate (* (frame-char-height) spacing))
                   spacing)))
           (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
                   (bound-and-true-p diff-hl-bmp-max-width)))
           (_ (when (or (not w) (zerop w))
		(setq w (or (bound-and-true-p diff-hl-bmp-max-width) 8)))))
      (define-fringe-bitmap 'sleek-diff-hl-bmp-middle
	(make-vector
	 h (string-to-number
            (let ((half-w (1- (/ w 2))))
              (concat (make-string half-w ?1)
                      (make-string (- w half-w) ?0)))
            2))
	nil nil 'center)))
  (defun diff-hl-fringe-thin-bmp-function (type pos)
    "Return appropriate bitmap for diff-hl"
    'sleek-diff-hl-bmp-middle)
  (defun diff-hl-make-faces-transparent ()
    "Make diff-hl faces have transparent backgrounds."
    (dolist (face '(diff-hl-insert diff-hl-delete diff-hl-change))
      (when (facep face)
	(set-face-background face nil))))
  (diff-hl-define-thin-bitmaps)
  (setq diff-hl-fringe-bmp-function #'diff-hl-fringe-thin-bmp-function)
  (advice-add 'diff-hl-define-bitmaps :after #'diff-hl-define-thin-bitmaps)
  (add-hook 'diff-hl-mode-hook #'diff-hl-make-faces-transparent)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; indent-bars
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :config
  (when (or (eq system-type 'darwin) (display-graphic-p))
    (setq indent-bars-prefer-character t))
  (setq indent-bars-treesit-support t
	indent-bars-starting-column 0
	indent-bars-color-by-depth nil
	indent-bars-highlight-current-depth '(:face default :blend 0.4)
	indent-bars-no-descend-lists t
	indent-bars-display-on-blank-lines nil))

(use-package ligature
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (global-prettify-symbols-mode)
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
				       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
				       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
				       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
				       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
				       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
				       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
				       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
				       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
				       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
				       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
				       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
				       "<:<" ";;;"))
  (global-ligature-mode t))

;; elcord
(use-package elcord
  :init
  (elcord-mode)
  :config
  (setq elcord-use-major-mode-as-main-icon t
	elcord--editor-name (concat "Emacs " emacs-version)))

;; language configuration

;; tree-sitter
(use-package treesit-auto
  :config
  (delete 'janet treesit-auto-langs)
  (delete 'latex treesit-auto-langs)
  (delete 'markdown treesit-auto-langs)
  (setq treesit-auto-install 't)
  (treesit-auto-install-all)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; eglot (LSP)
(use-package eglot
  :hook ((prog-mode . (lambda ()
			(unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
			  (eglot-ensure)))))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0
	eglot-sync-connect 0
	eglot-send-changes-idle-time 3
	eglot-autoshutdown t
	eglot-extend-to-xref t))

;; flymake
;; check https://www.emacswiki.org/emacs/FlyMake#h5o-2
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind
  ("C-c ! c" . flymake-start)
  ("C-c ! l" . flymake-show-diagnostics)
  ("C-c ! n" . flymake-goto-next-error)
  ("C-c ! p" . flymake-goto-prev-error)
  :init
  (defun flymake-show-diagnostics ()
    "If in a project, flymake-show-project-diagnostics,
     else flymake-show-buffer-diagnostics."
    (interactive)
    (if (project-current)
	(flymake-show-project-diagnostics)
      (flymake-show-buffer-diagnostics)))
  :config
  (setq flymake-no-changes-timeout 3
	flymake-fringe-indicator-position 'right-fringe))

;; apheleia (formatter)
;; check (describe-variable (apheleia-formatters))
(use-package apheleia
  :init
  (setq require-final-newline t
	show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (apheleia-global-mode)
  :config
  (setq apheleia-remote-algorithm 'remote)
  (defun apheleia-eglot-format (&rest _)
    "Format buffer using eglot, ignoring apheleia arguments."
    (when (and (eglot-managed-p)
               (eglot-server-capable :documentFormattingProvider))
      (eglot-format-buffer)))
  (add-to-list 'apheleia-formatters '(eglot . apheleia-eglot-format))
  (defun apheleia-prefer-eglot ()
    "Hook to prefer formatting from eglot when available."
    (when (and (null apheleia-formatter)
	       (eglot-managed-p)
	       (eglot-server-capable :documentFormattingProvider))
      (setq-local apheleia-formatter 'eglot)))
  (add-hook 'eglot-managed-mode-hook 'apheleia-prefer-eglot))

;; copilot
(use-package copilot
  :ensure (copilot :repo "https://github.com/copilot-emacs/copilot.el"
		   :files ("*.el"))
  :hook (prog-mode . copilot-turn-on-unless-buffer-read-only)
  :bind (("C-c h" . copilot-mode)
	 (:map copilot-completion-map
	       ("C-g" . 'copilot-clear-overlay)
	       ("<tab>" . 'copilot-tab)
	       ("TAB" . 'copilot-tab)))
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

;; cc-mode
(use-package cc-mode
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
		    "--pch-storage=memory"))))

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
  "\\.rs\\'")  :ensure nil

;; typescript-mode
(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'")
  :config
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("vtsls" "--stdio"))))

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
