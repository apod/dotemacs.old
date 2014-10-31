;;; ap-core.el
;;; Core

;; Make sure the coding system is utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Store backup files
(setq backup-directory-alist
      `((".*" . ,ap-backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "\\1" ap-backup-directory) t)))
(setq auto-save-list-file-prefix
      (expand-file-name ".saves-"
                        (expand-file-name "auto-save-list" ap-cache-directory)))

;; Revert buffer if the file was changed
(global-auto-revert-mode t)

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; ignore special buffers
      uniquify-after-kill-buffer-p t)

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

;; Scrolling
(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; Lower the delay for displaying keystrokes in minibuffer
(setq echo-keystrokes 0.1)

;; Delete selection on key press
(delete-selection-mode t)

;; Don't set point position when you focus the frame
(setq x-mouse-click-focus-ignore-position t)

;; Answer questions with y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable automatic indentation
(electric-indent-mode t)

;; Make C-x 1 toggleable
(ap-ensure-package 'zygospore)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(provide 'ap-core)
