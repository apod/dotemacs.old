;;; ap-ruby.el
;;; Ruby configuration

(ap-ensure-package 'inf-ruby)
(ap-ensure-package 'slim-mode)

;; Treat as ruby files
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))

(defadvice ruby-send-last-sexp (around ruby-send-last-sexp-evil-preceding-point activate)
  (if (evil-normal-state-p)
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        ad-do-it)
    ad-do-it))

(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook (lambda () (inf-ruby-minor-mode t))))


(provide 'ap-ruby)
