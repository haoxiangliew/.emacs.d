;;; early-init.el --- haoxiangliew's Emacs early-init configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is part of my personal Emacs configuration

;;; Code:

;; no littering
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "~/.cache/emacs/eln-cache/"))))

;; elpaca bootstrap
(setq package-enable-at-startup nil
      package-quickstart nil
      inhibit-default-init nil)
(setq elpaca-aot-native-compilation t)

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

;; defer gc until loaded
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

;; inhibit resize
(setq frame-inhibit-implied-resize t)

;; disable default UI elements
;; (push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (setq menu-bar-mode nil
(setq tool-bar-mode nil
      scroll-bar-mode nil)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; use bar cursor
(setq-default cursor-type 'bar)

;;; early-init.el ends here
