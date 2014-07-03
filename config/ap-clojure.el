;;; ap-clojure.el
;;; Clojure configuration

(ap-ensure-package 'rainbow-delimiters)
(ap-ensure-package 'clojure-mode)
(ap-ensure-package 'cider)

(setq nrepl-hide-special-buffers t)

(add-hook 'clojure-mode-hook (lambda ()
                               (rainbow-delimiters-mode t)
                               (smartparens-strict-mode t)))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(add-hook 'cider-repl-mode-hook (lambda ()
                                  (rainbow-delimiters-mode t)
                                  (smartparens-strict-mode t)))

(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)

;; Make cider preceding point commands work on point when using evil's normal mode
(ap-advice-commands "evil-preceding-point"
                    (cider-eval-last-sexp
                     cider-eval-last-sexp-and-replace
                     cider-eval-last-sexp-to-repl
                     cider-insert-last-sexp-in-repl
                     cider-pprint-eval-last-sexp)
                    around
                    (if (evil-normal-state-p)
                        (save-excursion
                          (unless (or (eobp) (eolp)) (forward-char))
                          ad-do-it)
                      ad-do-it))

(provide 'ap-clojure)
