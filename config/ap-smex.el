;;; ap-smex.el
;;; smex, M-x enchancement

(ap-ensure-package 'smex)
(require 'smex)

;; Initialize smex
(setq smex-save-file (expand-file-name ".smex-items" ap-cache-directory))
(smex-initialize)

;; Replace M-x with smex
(global-set-key (kbd "M-x") 'smex)

;; M-X show commands relevant to the active major mode
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'ap-smex)
