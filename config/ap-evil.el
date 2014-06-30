;;; ap-evil.el
;;; Evil configuration

(ap-ensure-package 'evil)
(ap-ensure-package 'evil-surround)
(ap-ensure-package 'evil-visualstar)

(require 'evil)
(require 'evil-surround)
(require 'evil-visualstar)

(evil-mode t)
(global-evil-surround-mode t)


;;; Keybindings

;; Partial functionality from vim-unimpaired
(defun ap-blank-line-above ()
  (interactive)
  (save-excursion 
    (beginning-of-line)
    (newline)))

(defun ap-blank-line-below ()
  (interactive)
  (save-excursion 
    (end-of-line)
    (newline)))

(define-key evil-normal-state-map "] " 'ap-blank-line-below) ; ]<space>
(define-key evil-normal-state-map "[ " 'ap-blank-line-above) ; [<space>

(provide 'ap-evil)
