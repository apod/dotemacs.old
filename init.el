;;; init.el

;;; Early configuration

;; Turn off interface early to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Reduce the frequency of garbage collection
(setq gc-cons-threshold 50000000)

;;; Directories configuration

(defvar ap-cache-directory (expand-file-name ".cache" user-emacs-directory)
  "This directory stores cached content: auto-generated files, save files,
 history-files etc.")
(unless (file-exists-p ap-cache-directory)
  (make-directory ap-cache-directory))

(defvar ap-backup-directory (expand-file-name "backup" ap-cache-directory)
  "This directory stores file backups and auto saves.")

(defvar ap-config-directory (expand-file-name "config" user-emacs-directory)
  "This directory contains all the module configurations.")

;; Add config directory to load-path
(add-to-list 'load-path ap-config-directory)

;;; Modules
(require 'ap-packages)
(require 'ap-mode-line)
(require 'ap-whitespace)
(require 'ap-global-keybindings)
(require 'ap-ido)
(require 'ap-smex)

;;; Core

;; File to store customization information
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Store backup files on .cache directory
(setq backup-directory-alist
      `((".*" . ,ap-backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "\\1" ap-backup-directory) t)))
(setq auto-save-list-file-prefix
      (expand-file-name ".saves-"
                        (expand-file-name "auto-save-list" ap-cache-directory)))

;; Revert buffer if the file was changed
(global-auto-revert-mode t)

;; Make sure the coding system is utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Frame title, format: "buffer_name (file_path) - invocation_name"
(setq-default frame-title-format
              '(:eval (format "%s %s %s"
                              (buffer-name)
                              (cond
                               (buffer-file-truename
                                (concat "(" buffer-file-truename ")"))
                               (dired-directory
                                (concat "{" dired-directory "}"))
                               (t
                                "[no file]"))
                              (concat "- " invocation-name))))

;; Answer questions with y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; ignore special buffers
      uniquify-after-kill-buffer-p t)

;; Show matching parens
(show-paren-mode t)

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Delete selection on key press
(delete-selection-mode t)

;; Lower the delay for displaying keystrokes in minibuffer
(setq echo-keystrokes 0.1)

;; Scrolling
(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; Save last cursor position
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" ap-cache-directory))
(setq-default save-place t)

;; Save minibuffer history
(require 'savehist)
(setq savehist-file (expand-file-name "savehist" ap-cache-directory)
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60)
(setq-default history-length 1000)
(savehist-mode t)

;;; Interface

;; Highlight current line
(global-hl-line-mode t)

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;;; Line numbers

(ap-ensure-package 'linum-relative)
(require 'linum-relative)

;; Line number format
(setq linum-relative-format "%4s ")

;; Show current line number on current line
(setq linum-relative-current-symbol "")

;; Enable linum mode globally
(global-linum-mode t)

;;; Theme

;; Monokai
(ap-ensure-package 'monokai-theme)
(load-theme 'monokai t)
