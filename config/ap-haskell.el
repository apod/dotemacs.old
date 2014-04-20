;;; ap-haskell.el
;;; Haskell configuration

(ap-ensure-package 'haskell-mode)

;; https://github.com/bbatsov/prelude/blob/master/modules/prelude-haskell.el
(eval-after-load 'haskell-mode
  '(progn
     (defun ap-haskell-mode-defaults ()
       (subword-mode +1)
       (turn-on-haskell-doc-mode)
       (turn-on-haskell-indentation))

     (setq ap-haskell-mode-hook 'ap-haskell-mode-defaults)

     (add-hook 'haskell-mode-hook (lambda ()
                                    (run-hooks 'ap-haskell-mode-hook)))))

;; http://comments.gmane.org/gmane.comp.lang.haskell.cafe/85859
(defadvice inferior-haskell-load-file (after change-focus-after-load)
  "Switch focus to inferior haskell process after loading buffer (C-c C-l)"
  (other-window 1))

(ad-activate 'inferior-haskell-load-file)

(provide 'ap-haskell)
