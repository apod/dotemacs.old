;;; ap-evil.el
;;; Evil configuration

(ap-ensure-package 'evil)
(ap-ensure-package 'evil-surround)

(require 'evil)
(require 'evil-surround)

(evil-mode t)
(global-evil-surround-mode t)

(provide 'ap-evil)
