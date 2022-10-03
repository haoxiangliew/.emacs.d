;;; editor-init.el --- haoxiangliew's default editor configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my personal default editor configuration
;; Under my nixos configuration this is aliased to emcs
;; Since my full emacs configuration is not fit to be $EDITOR
;; we can launch this with emacs -Q -nw -l ~/.emacs.d/editor-init.el

;;; Dependencies:
;; none

;;; Code:

;; optimize gc without gcmh
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; disable deferred compilation
(setq comp-deferred-compilation nil)

;; disable package management
(setq package-enable-at-startup nil
      package-quickstart nil)

;; inhibit resize
(setq frame-inhibit-implied-resize t)

;; disable default UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; use bar cursor
(setq-default cursor-type 'bar)

;; load leuven-dark
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'(lambda () (load-theme 'leuven-dark)))
  (load-theme 'leuven-dark t))

;; less noise when compiling elisp
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      native-comp-async-report-warnings-errors nil
      load-prefer-newer t)
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
			       ";; [INFO] Emacs loaded 0 packages in " (emacs-init-time "%s seconds") " with " (format "%s" gcs-done) " garbage collections." "\n\n"))
;; font
(add-to-list 'default-frame-alist '(font . "Monospace-10.5:weight=normal"))
(set-face-attribute 'default nil :font "Monospace-10.5:weight=normal")
(set-face-attribute 'fixed-pitch nil :font "Monospace-10.5:weight=normal")
(set-face-attribute 'variable-pitch nil :font "Ubuntu-10.5:weight=normal")
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
(setq pixel-scroll-precision-mode t)
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
              bidi-paragraph-direction 'left-to-right)
;; disable backups
(setq make-backup-files nil)

;; configure eshell
(global-set-key (kbd "C-x C-e") 'eshell)
(defun eshell-add-aliases ()
  (dolist (var   '(("q"  "exit")
		   ("ff" "find-file $1")
		   ("d"  "dired $1")
		   ("rg" "rg --color=always $*")
		   ("l"  "ls -lh $*")
		   ("ll" "ls -lah $*")
		   ("git" "git --no-pager $*")
		   ("clear" "clear-scrollback")))
    (add-to-list 'eshell-command-aliases-list var)))
(add-hook 'eshell-post-command-hook 'eshell-add-aliases)
