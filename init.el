;;; init.el

;;; Interface

;; Turn off menu-bar, tool-bar, scroll-bar modes
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
