;;; early-init.el --- haoxiangliew's Emacs early-init configuration

;;; Commentary:
;; This is part of my personal Emacs configuration

;;; Code:

;; disable deferred compilation
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

;;; early-init.el ends here
