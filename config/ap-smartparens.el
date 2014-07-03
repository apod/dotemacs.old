;;; ap-smartparens.el
;;; Smartparens configuration

(ap-ensure-package 'smartparens)

(require 'smartparens-config)
(smartparens-global-mode t)

(sp-pair "(" ")"   :wrap "M-(")
(sp-pair "{" "}"   :wrap "M-{")
(sp-pair "[" "]"   :wrap "M-[")
(sp-pair "\"" "\"" :wrap "M-\"")

(provide 'ap-smartparens)
