;;; package -- sss
;;; Commentary:

;;; Code:

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;;关闭启动画面
(setq inhibit-startup-message t)

;; 初始化straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 使用straight安装use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; use-package初始化
(setq use-package-compute-statistics t)

;; 初始化一些全局变量
;; 设置2个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default default-tab-width 2)

;; 默认查找目录为home目录
(setq command-line-default-directory "~")

(setq bookmark-default-file "~/.emacs.d/.cache/bookmarks")
;; 定义全局的leader-map

(set-frame-font "Source Code Pro 20" nil t)


;; 快捷键提示
(use-package which-key
  :config
  ;; 为define-key增加注释
  (setq which-key-enable-extended-define-key t)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)

  (which-key-mode)
  )

;; 内置模块的一些功能

;; 保存了上一次打开文件时的光标位置
(use-package saveplace
  :straight nil
  :init
  (setq save-place-file "~/.emacs.d/.local/places")
  :hook (after-init . save-place-mode))

;; 高亮当前行
(use-package hl-line
  :straight nil
  :hook (after-init . global-hl-line-mode))

;; 显示/隐藏结构化的数据
(use-package hideshow
  :straight nil
  :hook (prog-mode . hs-minor-mode)
  :bind
  ("C-c TAB" . hs-toggle-hiding)
  )

;; 简单文件指示
(use-package simple
  :straight nil
  :hook (after-init . (lambda ()
                        (line-number-mode)
                        (column-number-mode)
                        (size-indication-mode))))

;; 显示空白字符
(use-package whitespace
  :straight nil
  :hook (after-init . global-whitespace-mode)

  :config
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7")))

  (setq
   whitespace-line-column nil
   whitespace-style
   '(face		; visualize things below:
     empty		; empty lines at beginning/end of buffer
     lines-tail	; lines go beyond `fill-column'
     space-before-tab	; spaces before tab
     trailing           ; trailing blanks
     ;; tabs		; tabs (show by face)
     ;; tab-mark		; tabs (show by symbol)
     ))
  )

;; 当某个文件的某一行特别长的时候，自动优化性能
(use-package so-long
  :straight nil
  :config (global-so-long-mode 1))

;; 文件被外部程序修改后，重新载入buffer
;; (use-package autorevert
;;   :straight nil
;;   :hook (after-init . global-auto-revert-mode))

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


;; 选中后直接输入，不用删除
(use-package delsel
  :straight nil
  :hook (after-init . delete-selection-mode))

;; 高亮显示配对的大括号
(use-package paren
  :straight nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; 最近打开的文件
(use-package recentf
  :straight nil
  :init
  (setq
   recentf-save-file "~/.emacs.d/.local/recentf"
   recentf-max-saved-items 2000
   recentf-max-menu-items 150)
  :hook (after-init . recentf-mode)
  )

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package)

(use-package ag :ensure-system-package ag)

;; 设置主题
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; 安装icon管理
(use-package all-the-icons)

;; 自动保存
;; (use-package super-save
;;   :config
;;   (super-save-mode +1))

(use-package counsel-projectile
  :config
  (setq
   projectile-known-projects-file "~/.emacs.d/.cache/projectile-bookmarks.eld"
   projectile-project-search-path '("~/mugeda/" "~/workspace/" "~/")
   )
  (projectile-discover-projects-in-search-path)
  :bind
	("C-c p f" . counsel-projectile-find-file )
	("C-c p p" . counsel-projectile-switch-project)
	("C-c p b" . counsel-projectile-switch-to-buffer)
	("C-c p s" . counsel-projectile-ag)
	)

;; 设置amx，命令快速查找
(use-package amx
  :after ivy
  :init
  (setq amx-save-file "~/.emacs.d/.local/amx-items")
  :bind
	("M-x" . amx)
  )

;; 快速跳转
(use-package avy
  :bind
	("C-c j j" . avy-goto-char-timer)
	("C-c j c" . avy-goto-char)
	("C-c j l" . avy-goto-line)
	)

;; 括号的多色彩
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; 搜索统计
(use-package anzu
  :hook (after-init . global-anzu-mode))

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

(use-package helm
  :defer t)

;; 美化modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-modal-icon t)
  (doom-modeline-mode 1))

;; 快速选择工具
(use-package expand-region
  :bind
	("C-s-e" . er/expand-region)
  )

;; 增加文件的行号
(use-package linum
  :config
  (global-linum-mode t)
  (setq linum-format "%4d  ")
  (set-face-background 'linum nil)
  )

;; wakatime
(use-package wakatime-mode
  :config
  (global-wakatime-mode))

;; 记录命令使用次数
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package org-web-tools
  :straight (org-web-tools
             :host github
             :repo "alphapapa/org-web-tools"
             :fork (:host github
                          :repo "running-grass/org-web-tools")
             ))

(use-package ggtags
  :ensure-system-package global
  )

(use-package tramp
  :straight nil
  :config
  (setq tramp-persistency-file-name "~/.emacs.d/.cache/tramp"))

(use-package alert
  :config
  (setq alert-default-style 'osx-notifier)
  )

(use-package ebdb
  :config
  (setq ebdb-mua-auto-update-p nil)
  )

(use-package ox-hugo
  :after ox
  :hook (org . org-hugo-auto-export-mode)

  :config
  (setq org-hugo-section "post"
        org-hugo-auto-set-lastmod	t
        )
  ;; (org-hugo-auto-export-mode)
  )

;; (use-package cnfonts
;;   :config
;;   (cnfonts-enable))


;; 打开emacs的初始化文件
(defun gremacs/open-emacs-init ()
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))
;; 加载emacs的初始化文件
(defun gremacs/load-emacs-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(use-package magit)


;; 通用的快捷键绑定

(global-set-key (kbd "C-c j n") 'goto-line)
(global-set-key (kbd "C-c f e i") 'gremacs/open-emacs-init)
(global-set-key (kbd "C-c f e r") 'gremacs/load-emacs-init)

(load-file "~/.emacs.d/develop.el")
(load-file "~/.emacs.d/wiki.el")

(provide 'init)
;; init.el ends here
