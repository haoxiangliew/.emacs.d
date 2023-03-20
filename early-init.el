;;; early-init.el --- haoxiangliew's Emacs early-init configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is part of my personal Emacs configuration

;;; Code:

;; elpaca bootstrap
(setq package-enable-at-startup nil
      inhibit-default-init nil)

;; defer gc until loaded
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  "Reset GC."
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;; disable native-comp errors
(setq native-comp-async-report-warnings-errors nil)

;; inhibit resize
(setq frame-inhibit-implied-resize t)

;; disable regex searches of file-name-handler-alist
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; disable default UI elements
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      server-client-instructions nil)
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
