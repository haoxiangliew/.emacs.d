;;; early-init.el --- haoxiangliew's Emacs early-init configuration

;;; Commentary:
;; This is part of my personal Emacs configuration

;;; Code:

;; disable deferred compilation and fix nix native-comp
(setq comp-deferred-compilation nil)

;; bootstrap straight and use-package
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

;; add padding to compensate for rounded corners
(add-to-list 'default-frame-alist '(internal-border-width . 7))
;; (setq-default left-margin-width 1 right-margin-width 1)
;; (set-window-buffer nil (current-buffer))

;;; early-init.el ends here
