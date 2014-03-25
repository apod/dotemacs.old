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
(require 'ap-core)
(require 'ap-appearance)
(require 'ap-mode-line)
(require 'ap-whitespace)
(require 'ap-global-keybindings)
(require 'ap-ido)
(require 'ap-smex)

;; File to store customization information
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
