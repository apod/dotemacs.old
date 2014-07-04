;;; ap-emacs-lisp.el
;;; Emacs lisp configuration

(ap-ensure-package 'rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                               (rainbow-delimiters-mode t)
                               (smartparens-strict-mode t)
                               (turn-on-eldoc-mode)))


(provide 'ap-emacs-lisp)
