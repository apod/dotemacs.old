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

(defun ap-sml-kill-and-load-file (file)
  (interactive
   (list (or buffer-file-name
             (read-file-name "File to load: " nil nil t))))
  (when (processp (get-buffer-process "*sml*"))
    (delete-process "*sml*"))
  (sml-prog-proc-load-file file)
  (other-window 1))

(eval-after-load 'sml-mode
  '(progn
     (define-key sml-mode-map (kbd "C-c C-r") 'ap-sml-send-region-or-line)
     (define-key sml-mode-map (kbd "C-c C-k") 'ap-sml-kill-and-load-file)))

(provide 'ap-sml)
