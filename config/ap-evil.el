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

;; Remap j/k to work like gj/gk and vice versa
(define-key evil-normal-state-map "gj" 'evil-next-line)
(define-key evil-normal-state-map "gk" 'evil-previous-line)
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)

;; Partial functionality from vim-unimpaired
(defun ap-newline-above ()
  (interactive)
  (save-excursion 
    (beginning-of-line)
    (newline)))

(defun ap-newline-below ()
  (interactive)
  (save-excursion 
    (end-of-line)
    (newline)))

(define-key evil-normal-state-map "] " 'ap-newline-below) ; ]<space>
(define-key evil-normal-state-map "[ " 'ap-newline-above) ; [<space>

(provide 'ap-evil)
