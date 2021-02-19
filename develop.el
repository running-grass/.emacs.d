;;; package -- develop
;;; Commentary:

;; 配置editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; 配置vue支持
(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (setq js-indent-level 2)
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

  ;; :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (js-mode . lsp)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration))
         ;; )
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
  (setq plantuml-executable-path "~/.nix-profile/bin/plantuml")
  (setq plantuml-jar-path "~/.nix-profile/lib/plantuml.jar")
  (setq plantuml-default-exec-mode 'executable)
  (setq org-plantuml-exec-mode 'executable)
  (setq org-plantuml-jar-path "~/.nix-profile/lib/plantuml.jar")
  (setq plantuml-executable-args '(
                                   "-headless"
                                   "-charset"
                                   "UTF-8"
                                   ))
  )


(use-package tramp
  :straight nil
  :config
  (setq tramp-persistency-file-name "~/.emacs.d/.cache/tramp"))


;; 快速选择工具
(use-package expand-region
  :bind
	("C-c e" . er/expand-region)
  )


;; ivy智能提示后端
(use-package ivy
  :config
  ;; 可以使switch-buffer集成recentf
  (setq ivy-use-virtual-buffers t)
  :hook (after-init . ivy-mode)
)

;; 自动补全
(use-package company
  :config
  (global-company-mode 1)
  )


;; 括号的多色彩
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )


;; 高亮显示配对的大括号
(use-package paren
  :straight nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


;; 注释/反注释
(use-package newcomment
  :straight nil
  :bind
  ( "C-c /" . comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (call-interactively 'comment-dwim)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  (comment-auto-fill-only-comments t))

;; 当某个文件的某一行特别长的时候，自动优化性能
(use-package so-long
  :straight nil
  :config (global-so-long-mode 1))

;; 显示/隐藏结构化的数据
(use-package hideshow
  :straight nil
  :hook (prog-mode . hs-minor-mode)
  :bind
  ("C-c TAB" . hs-toggle-hiding)
  )

;; project config
(use-package projectile
  :init
  (setq
   projectile-known-projects-file "~/.emacs.d/.cache/projectile-bookmarks.eld"
   projectile-project-search-path '("~/mugeda/" "~/workspace/" "~/")
   )
  :hook
  (after-init . projectile-mode)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))


(provide 'develop)
