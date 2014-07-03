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

(provide 'ap-clojure)
