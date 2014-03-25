;;; ap-global-keybindings.el
;;; Global keybindings

;; Replace buffer list with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Swap isearch with the regexp equivalents
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(provide 'ap-global-keybindings)
