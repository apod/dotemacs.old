;;; init.el

;;; Interface

;; Turn off menu-bar, tool-bar, scroll-bar, blink-cursor modes
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))

;;; Core

;; File to store customization information
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Make sure the coding system is utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(require 'cl) ; Common Lisp functions and macros

;;; Packages

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

;;; Theme

;; Use monokai if installed for default theme
(if (package-installed-p 'monokai-theme)
    (load-theme 'monokai t))
