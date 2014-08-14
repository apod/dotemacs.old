;;; ap-appearance.el
;;; Appearance configuration

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Show matching parens
(show-paren-mode t)

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

;; Enable linum mode on programming modes
(add-hook 'prog-mode-hook 'linum-mode)

;;; Fonts

;; Use M+ if available
(when (member "M+ 1mn" (font-family-list))
  (set-face-attribute 'default nil :font "M+ 1mn-11")
  (set-face-attribute 'variable-pitch nil :font "M+ 1mn-11"))

;;; Themes
(ap-ensure-package 'monokai-theme)

;; Load theme
(load-theme 'monokai t)

;; Reset the height scale to 1.0 for org levels 1-4 (set by monokai theme)
(apply 'custom-set-faces 
       (mapcar (lambda (level) 
                 `(,(intern (concat "org-level-" (number-to-string level))) ((t (:height 1.0))))) 
               (number-sequence 1 4)))

;; Change the link color
(custom-set-faces '(org-link ((t (:foreground "deep sky blue" :underline "deep sky blue")))))

(provide 'ap-appearance)

