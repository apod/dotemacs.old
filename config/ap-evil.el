;;; ap-evil.el
;;; Evil configuration

(ap-ensure-package 'evil)
(ap-ensure-package 'evil-surround)
(ap-ensure-package 'evil-visualstar)
(ap-ensure-package 'evil-numbers)
(ap-ensure-package 'evil-nerd-commenter)
(ap-ensure-package 'evil-exchange)
(ap-ensure-package 'evil-leader)

(setq evilnc-hotkey-comment-operator "gc")

(require 'evil)
(require 'evil-nerd-commenter)
(require 'evil-surround)
(require 'evil-visualstar)
(require 'evil-exchange)

;; evil-leader
(global-evil-leader-mode t)

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(evil-leader/set-leader "\\")
(evil-leader/set-key "ol" 'linum-mode
                     "oh" 'whitespace-mode
                     "\\" 'evil-buffer
                     "n"  'narrow-or-widen-dwim
                     "g"  'magit-status)

;; * and # work on symbols instead of words
(setq-default evil-symbol-word-search t)

(evil-mode t)

;; evil-surround
(global-evil-surround-mode t)

;; evil-numbers
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-+") 'evil-numbers/inc-at-pt)

;; evil-exchange
(evil-exchange-install)

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

;; vim-vinegar like keybindings
(require 'dired)
(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(define-key dired-mode-map (kbd "-") 'dired-up-directory)

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

;; magit
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging)

;; C-g works like esc
(define-key evil-normal-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Esc works consistently
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

(provide 'ap-evil)
