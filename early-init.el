;;; early-init.el --- haoxiangliew's Emacs early-init configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is part of my personal Emacs configuration

;;; Code:

;; elpaca bootstrap
(setq package-enable-at-startup nil
      package-quickstart nil
      inhibit-default-init nil)

;; inhibit messages
(setq inhibit-message t)
(add-hook 'elpaca-after-init-hook (lambda () (setq inhibit-message nil)))

;; disable emacsclient instructions
(setq server-client-instructions nil)

;; disable regex searches of file-name-handler-alist
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; disable native-comp errors
(setq native-comp-async-report-warnings-errors nil)

;; disable deferred compilation
(setq comp-deferred-compilation nil)

;; defer gc until loaded
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

;; inhibit resize
(setq frame-inhibit-implied-resize t)

;; disable default UI elements
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; load a better modeline later
(put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
(setq-default mode-line-format nil)
(dolist (buf (buffer-list))
  (with-current-buffer buf (setq mode-line-format nil)))

;; use bar cursor
(setq-default cursor-type 'bar)

;;; early-init.el ends here
