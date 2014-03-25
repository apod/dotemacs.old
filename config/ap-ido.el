;;; ap-ido.el
;;; ido-mode configuration

(require 'ido)

(setq ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-default-file-method 'selected-window
      ido-save-directory-list-file (expand-file-name "ido.last" ap-cache-directory))

(ido-mode t)

;; Use Ido for all buffer/file reading
(ido-everywhere t)

;; flx-ido, fuzzy ido
(ap-ensure-package 'flx-ido)

(defun ap/setup-ido ()
  ;; Toggle flx-ido-mode
  (define-key ido-completion-map (kbd "C-q")
    (lambda ()
      (interactive)
      (if flx-ido-mode
          (progn
            (setq ido-enable-flex-matching nil)
            (setq ido-use-faces t)
            (flx-ido-mode -1))
        (progn
          (setq ido-enable-flex-matching t)
          (setq ido-use-faces nil)
          (flx-ido-mode t))))))

(add-hook 'ido-setup-hook 'ap/setup-ido)

;; ido-ubiquitous, use ido everywhere
(ap-ensure-package 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(provide 'ap-ido)
