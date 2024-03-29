;;; ap-clojure.el
;;; Clojure configuration

(ap-ensure-package 'rainbow-delimiters)
(ap-ensure-package 'clojure-mode)
(ap-ensure-package 'cider)

;; Hide cider REPL buffers
(setq nrepl-hide-special-buffers t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Store cider REPL history
(setq cider-repl-history-file (expand-file-name "cider-history" ap-cache-directory))

;; Run cider some cider modes on emacs state
(add-to-list 'evil-emacs-state-modes 'cider-docview-mode)
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)

(add-hook 'clojure-mode-hook (lambda ()
                               (rainbow-delimiters-mode t)
                               (smartparens-strict-mode t)))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(add-hook 'cider-repl-mode-hook (lambda ()
                                  (rainbow-delimiters-mode t)
                                  (smartparens-strict-mode t)))


;; Make cider preceding point commands work on point when using evil's normal mode
(ap-advice-commands "evil-preceding-point"
                    (cider-eval-last-sexp
                     cider-eval-last-sexp-and-replace
                     cider-eval-last-sexp-to-repl
                     cider-insert-last-sexp-in-repl
                     cider-pprint-eval-last-sexp)
                    around
                    (if (evil-normal-state-p)
                        (save-excursion
                          (unless (or (eobp) (eolp)) (forward-char))
                          ad-do-it)
                      ad-do-it))

(defun ap-cider-eval-defun-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-current-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(provide 'ap-clojure)
