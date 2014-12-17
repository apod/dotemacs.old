;;; ap-lua.el
;;; Lua configuration

(ap-ensure-package 'lua-mode)

(eval-after-load 'lua-mode
  '(progn
     (define-key lua-mode-map (kbd "C-c C-r") 'lua-send-region)
     (define-key lua-mode-map (kbd "C-c C-e") 'lua-send-current-line)
     (define-key lua-mode-map (kbd "C-M-x")   'lua-send-defun)

     (setq lua-indent-level 2)))

(provide 'ap-lua)
