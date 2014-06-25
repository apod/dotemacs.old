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

(defvar ap-local-directory (expand-file-name "local" user-emacs-directory)
  "This directory contains all the local machine configurations.")
(unless (file-exists-p ap-local-directory)
  (make-directory ap-local-directory))

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
(require 'ap-evil)
(require 'ap-smartparens)

(require 'ap-haskell)
;; File to store the config changes made through customize ui
(setq custom-file (expand-file-name "custom.el" ap-local-directory))

;; Load local machine configurations
(when (file-exists-p ap-local-directory)
  (mapc 'load (directory-files ap-local-directory 't "^[^#].*el$")))
