;;; early-init.el --- haoxiangliew's Emacs early-init configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is part of my personal Emacs configuration

;;; Code:

;; set gc-cons-threshold to max
(setq gc-cons-threshold most-positive-fixnum)
;; after init, reset gc-cons-threshold
(add-hook 'emacs-startup-hook (lambda ()
				(setq gc-cons-threshold (eval (car (get 'gc-cons-threshold 'standard-value))))))

;; disable package.el
(setq package-enable-at-startup nil)

;; optimize auto-mode-alist
(setq auto-mode-case-fold nil)

;; less noise when compiling elisp
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      native-comp-async-report-warnings-errors nil
      load-prefer-newer t)

;; no-littering
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "~/.cache/emacs/eln-cache/"))))
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      auto-save-list-file-prefix nil)

;; disable emacsclient instructions
(setq server-client-instructions nil)

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

;; use bar cursor
(setq-default cursor-type 'bar)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; early-init.el ends here
