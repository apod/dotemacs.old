;;; ap-projectile.el
;;; Projectile configuration

(ap-ensure-package 'projectile)

(setq projectile-cache-file (expand-file-name "projectile.cache" ap-cache-directory))
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" ap-cache-directory))

(projectile-global-mode t)

(provide 'ap-projectile)
