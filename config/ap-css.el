;;; ap-css.el
;;; CSS configuration

(ap-ensure-package 'rainbow-mode)
(ap-ensure-package 'scss-mode)

(setq css-indent-offset 2)

(add-hook 'css-mode-hook (lambda ()
                           (rainbow-mode t)))

;; Turn off auto-compile on save
(setq scss-compile-at-save nil)

(provide 'ap-css)
