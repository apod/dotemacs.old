;;; ap-evil.el
;;; Evil configuration

(ap-ensure-package 'evil)
(ap-ensure-package 'evil-surround)
(ap-ensure-package 'evil-visualstar)
(ap-ensure-package 'evil-numbers)

(require 'evil)
(require 'evil-surround)
(require 'evil-visualstar)

(evil-mode t)

;; evil-surround
(global-evil-surround-mode t)

;; evil-numbers
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-+") 'evil-numbers/inc-at-pt)

;;; Keybindings

;; Remap j/k to work on visual lines and gj/gk on actual lines
(define-key evil-motion-state-map (kbd "j")  'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "k")  'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "gj") 'evil-next-line)
(define-key evil-motion-state-map (kbd "gk") 'evil-previous-line)

;; Remap Y to y$
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

;; Partial functionality from vim-unimpaired
(defun ap-newline-above ()
  (interactive)
  (let ((column (current-column)))
    (beginning-of-line)
    (newline)
    (forward-char column)))

(defun ap-newline-below ()
  (interactive)
  (save-excursion 
    (end-of-line)
    (newline)))

(define-key evil-normal-state-map (kbd "] SPC") 'ap-newline-below) ; ]<space>
(define-key evil-normal-state-map (kbd "[ SPC") 'ap-newline-above) ; [<space>

(defun ap-move-line-above ()
  (interactive)
  (let ((column (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (forward-char column)))

(defun ap-move-line-below ()
  (interactive)
  (let ((column (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (forward-char column)))

(define-key evil-normal-state-map (kbd "] e") 'ap-move-line-below)
(define-key evil-normal-state-map (kbd "[ e") 'ap-move-line-above)

(provide 'ap-evil)
