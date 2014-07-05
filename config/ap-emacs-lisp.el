;;; ap-emacs-lisp.el
;;; Emacs lisp configuration

(ap-ensure-package 'rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                               (rainbow-delimiters-mode t)
                               (smartparens-strict-mode t)
                               (turn-on-eldoc-mode)))

(ap-advice-commands "evil-preceding-point"
                    (eval-print-last-sexp)
                    around
                    (if (evil-normal-state-p)
                        (save-excursion
                          (unless (or (eobp) (eolp)) (forward-char))
                          ad-do-it)
                      ad-do-it))

(provide 'ap-emacs-lisp)
