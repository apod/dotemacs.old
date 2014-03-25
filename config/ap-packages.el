;;; ap-packages.el
;;; Package system configuration

(require 'package)

;; Add melpa package archive
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
