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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; (setq lsp--tcp-server-port 5000)

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js-mode . lsp)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration))
         )
  :commands lsp)

;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


;; plantuml
(use-package plantuml-mode
  :config
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))

  (setq plantuml-executable-path "~/.nix-profile/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  )

(provide 'develop)
