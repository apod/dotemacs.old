;;; ap-scheme.el
;;; Scheme configuration

(ap-ensure-package 'geiser)

(add-hook 'scheme-mode-hook (lambda ()
                              (rainbow-delimiters-mode t)
                              (smartparens-strict-mode t)))

;; Store geiser REPL history
(setq geiser-repl-history-filename (expand-file-name "geiser-history" ap-cache-directory))

(add-hook 'geiser-repl-mode-hook (lambda ()
                                   (rainbow-delimiters-mode t)
                                   (smartparens-strict-mode t)))

(ap-advice-commands "evil-preceding-point"
                    (geiser-eval-last-sexp
                     geiser-expand-last-sexp)
                    around
                    (if (evil-normal-state-p)
                        (save-excursion
                          (unless (or (eobp) (eolp)) (forward-char))
                          ad-do-it)
                      ad-do-it))

(provide 'ap-scheme)
