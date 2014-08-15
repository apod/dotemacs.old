;;; ap-common-lisp.el
;;; Common Lisp configuration

(ap-ensure-package 'rainbow-delimiters)
(ap-ensure-package 'slime)

(setq slime-lisp-implementations
      '((clisp ("clisp" "-q"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

(setq slime-default-lisp 'clisp)

(add-to-list 'slime-contribs 'slime-repl)

(add-hook 'lisp-repl-mode-hook (lambda ()
                                 (rainbow-delimiters-mode t)
                                 (smartparens-strict-mode t)))

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (rainbow-delimiters-mode t)
                                  (smartparens-strict-mode t)))

(provide 'ap-common-lisp)
