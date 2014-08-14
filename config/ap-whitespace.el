;;; ap-whitespace.el
;;; Whitespace configuration

;; Don't use tab character for indentation
(setq-default indent-tabs-mode nil)

;; Whitespace mode setup
(setq whitespace-display-mappings '((newline-mark 10 [172 10])
                                    (space-mark 32 [183] [46])
                                    (tab-mark 9 [187 9] [92 9])))

;; Show trailing whitespace
(define-minor-mode apod-show-trailing-whitespace-mode
  "Enables `show-trailing-whitespace'."
  :lighter nil
  (progn (setq show-trailing-whitespace apod-show-trailing-whitespace-mode)))

(add-hook 'prog-mode-hook 'apod-show-trailing-whitespace-mode)

(provide 'ap-whitespace)
