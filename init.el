;;; init.el

;;; Core

;; File to store customization information
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Store all backup files on backups/
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Make sure the coding system is utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Frame title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Answer questions with y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'cl) ; Common Lisp functions and macros

;;; Interface

;; Turn off menu-bar, tool-bar, scroll-bar, blink-cursor modes
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))

;; Inhibit startup screen
(setq inhibit-startup-screen t)

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

;;; Whitespace

;; Don't use tab character for indentation
(setq-default indent-tabs-mode nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)
