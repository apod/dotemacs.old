;;; ap-packages.el
;;; Package system configuration

(require 'package)

;; Set package archive repositories
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; Activate installed packages now
(setq package-enable-at-startup nil)
(package-initialize)

;;; Functions

(defun ap-ensure-package (package)
  "Ensure that a package is installed."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(provide 'ap-packages)
