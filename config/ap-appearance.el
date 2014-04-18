;;; ap-appearance.el
;;; Appearance configuration

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Show matching parens
(show-paren-mode t)

;; Highlight current line
;; (global-hl-line-mode t)

;; Frame title, format: "buffer_name (file_path) - invocation_name"
(setq-default frame-title-format
              '(:eval (format "%s %s %s"
                              (buffer-name)
                              (cond
                               (buffer-file-truename
                                (concat "(" buffer-file-truename ")"))
                               (dired-directory
                                (concat "{" dired-directory "}"))
                               (t
                                "[no file]"))
                              (concat "- " invocation-name))))

;;; Line numbers

(ap-ensure-package 'linum-relative)
(require 'linum-relative)

;; Line number format
(setq linum-relative-format "%4s ")

;; Show current line number on current line
(setq linum-relative-current-symbol "")

;; Enable linum mode globally
(global-linum-mode t)

;;; Themes

(ap-ensure-package 'monokai-theme)

;; Load theme
(load-theme 'monokai t)

(provide 'ap-appearance)

