;;; package -- develop
;;; Commentary:

;; 配置editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; 配置vue支持
(use-package vue-mode
  :mode "\\.vue\\'"
  )

;; 配置emmet-mode
;; 默认为C-j展开
(use-package emmet-mode
  :hook html-mode
  :hook css-mode
  :hook vue-mode
  )

;; 配置php支持
(use-package php-mode
  :mode "\\.php\\'"
  )

(use-package pug-mode
  :mode "\\.pug\\'"
  )

(provide 'develop)
