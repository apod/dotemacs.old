;;; init.el

;;; Core

(defvar ap-cache-dir (expand-file-name ".cache" user-emacs-directory)
  "This directory stores cached content: auto-generated files, save files, history-files etc.")

;; Create cache dir
(unless (file-exists-p ap-cache-dir)
  (make-directory ap-cache-dir))

;; File to store customization information
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Store all backup files on backups directory
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

;; Meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; ignore special buffers
      uniquify-after-kill-buffer-p t)

;;; Interface

;; Turn off menu-bar, tool-bar, scroll-bar, blink-cursor modes
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Highlight current line
(global-hl-line-mode t)

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;;; Packages

(require 'package)

;; Add melpa package archive
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Activate installed packages now
(setq package-enable-at-startup nil)
(package-initialize)

;; Helper function to ensure a package is installed
(defun ap-ensure-package (package)
  "Ensure that a package is installed, if not install it."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;; Theme

;; Monokai
(ap-ensure-package 'monokai-theme)
(load-theme 'monokai t)

;;; Whitespace

;; Don't use tab character for indentation
(setq-default indent-tabs-mode nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;;; ido-mode
(require 'ido)
(ap-ensure-package 'flx-ido)

(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      ido-save-directory-list-file (expand-file-name "ido.last" ap-cache-dir))

(ido-mode t)
(ido-everywhere t)

(flx-ido-mode t)

;; Disable ido faces to see flx highlights
(setq ido-use-faces nil)
