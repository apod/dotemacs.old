;;; ap-evil.el
;;; Evil configuration

(ap-ensure-package 'evil)
(ap-ensure-package 'evil-surround)
(ap-ensure-package 'evil-visualstar)

(require 'evil)
(require 'evil-surround)
(require 'evil-visualstar)

(evil-mode t)
(global-evil-surround-mode t)

(provide 'ap-evil)
