;;; ap-sml.el
;;; SML configuration

(ap-ensure-package 'sml-mode)

(defun ap-sml-send-region-or-line ()
  (interactive)
  (if (use-region-p)
      (sml-prog-proc-send-region (region-beginning)
                                 (region-end))
    (sml-prog-proc-send-region (line-beginning-position)
                               (line-beginning-position 2))))

(eval-after-load 'sml-mode
  '(define-key sml-mode-map (kbd "C-c C-r") 'ap-sml-send-region-or-line))

(provide 'ap-sml)
