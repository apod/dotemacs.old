;;; ap-haskell.el
;;; Haskell configuration

(ap-ensure-package 'haskell-mode)

;; https://github.com/bbatsov/prelude/blob/master/modules/prelude-haskell.el
(eval-after-load 'haskell-mode
  '(progn
     (defun ap-haskell-mode-defaults ()
       (turn-on-haskell-doc-mode)
       (turn-on-haskell-indentation)
       (interactive-haskell-mode t))

     (setq ap-haskell-mode-hook 'ap-haskell-mode-defaults)
     (setq haskell-process-suggest-remove-import-lines t)
     (setq haskell-process-auto-import-loaded-modules t)
     (setq haskell-process-log t)
     (setq haskell-process-type 'ghci)

     (add-hook 'haskell-mode-hook (lambda ()
                                    (run-hooks 'ap-haskell-mode-hook)))))

(provide 'ap-haskell)
