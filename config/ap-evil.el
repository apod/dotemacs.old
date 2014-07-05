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

;; Unbind evil's normal mode M-. binding
(define-key evil-normal-state-map (kbd "M-.") nil)

;; Remap j/k to work on visual lines and gj/gk on actual lines
(define-key evil-motion-state-map (kbd "j")  'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "k")  'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "gj") 'evil-next-line)
(define-key evil-motion-state-map (kbd "gk") 'evil-previous-line)

;; Remap H, L to ^, g_
(define-key evil-motion-state-map (kbd "H")  'evil-first-non-blank)
(define-key evil-motion-state-map (kbd "L")  'evil-last-non-blank)

;; Remap Y to y$
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

;; Partial functionality from vim-unimpaired
(defun ap-newline-above (times)
  (interactive "p")
  (let ((column (current-column)))
    (beginning-of-line)
    (newline times)
    (forward-char column)))

(defun ap-newline-below (times)
  (interactive "p")
  (save-excursion 
    (end-of-line)
    (newline times)))

(define-key evil-normal-state-map (kbd "] SPC") 'ap-newline-below)
(define-key evil-normal-state-map (kbd "[ SPC") 'ap-newline-above)

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

;; Smartparens navigation
(define-key evil-normal-state-map (kbd "M-l") 'sp-forward-sexp)
(define-key evil-normal-state-map (kbd "M-h") 'sp-backward-sexp)
(define-key evil-normal-state-map (kbd "M-j") 'sp-down-sexp)
(define-key evil-normal-state-map (kbd "M-k") 'sp-up-sexp)

;; Use s for smartparens manipulations
(define-key evil-normal-state-map (kbd "s") nil)

(define-key evil-normal-state-map (kbd "s y") 'sp-copy-sexp)

(define-key evil-normal-state-map (kbd "s k") 'sp-kill-sexp)

(define-key evil-normal-state-map (kbd "s t") 'sp-transpose-sexp)

(define-key evil-normal-state-map (kbd "s j") 'sp-join-sexp)
(define-key evil-normal-state-map (kbd "s s") 'sp-split-sexp)

(define-key evil-normal-state-map (kbd "s h") 'sp-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "s l") 'sp-forward-slurp-sexp)
(define-key evil-normal-state-map (kbd "s H") 'sp-backward-slurp-sexp)
(define-key evil-normal-state-map (kbd "s L") 'sp-backward-barf-sexp)

(provide 'ap-evil)
