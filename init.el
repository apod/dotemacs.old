;;; init.el

;;; Interface

;; Turn off menu-bar, tool-bar, scroll-bar, blink-cursor modes
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
