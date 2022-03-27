;;; early-init.el --- haoxiangliew's Emacs early-init configuration

;;; Commentary:
;; This is part of my personal Emacs configuration

;;; Code:

;; adjust gc until gcmh-mode
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; bootstrap straight and use-package
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; inhibit resize
(setq frame-inhibit-implied-resize t)

;; disable default UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; use bar cursor
(setq-default cursor-type 'bar)

;; add padding to compensate for rounded corners
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;;; early-init.el ends here
