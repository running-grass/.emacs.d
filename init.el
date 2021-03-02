;;; package -- sss
;;; Commentary:

;;; Code:

(load-file "~/.emacs.d/reset.el")

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

;; 快捷键提示
(use-package which-key
  :config
  ;; 为define-key增加注释
  (setq which-key-enable-extended-define-key t)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  :hook 
  (after-init . which-key-mode)
  )

;; 内置模块的一些功能

;; 保存了上一次打开文件时的光标位置
(use-package saveplace
  :defer t
  :straight nil
  :init
  (setq save-place-file "~/.emacs.d/.local/places")
  :hook (after-init . save-place-mode))

;; 高亮当前行
(use-package hl-line
  :defer t
  :straight nil
  :hook (after-init . global-hl-line-mode))

;; 文件被外部程序修改后，重新载入buffer
(use-package autorevert
  :defer t
  :straight nil
  :hook (after-init . global-auto-revert-mode))

;; 选中后直接输入，不用删除
(use-package delsel
  :defer t
  :straight nil
  :hook (after-init . delete-selection-mode))

;; 最近打开的文件
(use-package recentf
  :defer t
  :straight nil
  :init
  (setq
   recentf-save-file "~/.emacs.d/.local/recentf"
   recentf-max-saved-items 2000
   recentf-max-menu-items 150)
  :hook (after-init . recentf-mode)
  )

(use-package exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :defer t)

(use-package ag
  :defer t
  :ensure-system-package ag)


;; 设置输入法
(use-package rime
  :defer t
  :custom
  (default-input-method "rime")
  (rime-librime-root "/nix/store/mppwz8hwdgcfc6j46g1ywdg0cy2gpf99-librime-1.5.3")
  (rime-emacs-module-header-root "/nix/store/ihy79xiyvjlpb28pngcg5a0gkziwl0z5-emacs-27.1/include")
  :config
  (define-key rime-mode-map (kbd "s-C-i") 'rime-force-enable)
  (setq rime-user-data-dir "~/Library/Rime")
  (setq rime-share-data-dir "~/Library/Rime")
  (setq rime-disable-predicates
      '(rime-predicate-after-alphabet-char-p
        rime-predicate-prog-in-code-p
        rime-predicate-in-code-string-p
        rime-predicate-hydra-p
        rime-predicate-current-uppercase-letter-p
        ))
  )

;; 
;; (use-package! rime
;;               :custom
;;               (default-input-method "rime")
;;               (rime-librime-root "~/Documents/emacs/depend/librime/dist")
;;               (rime-emacs-module-header-root "/usr/local/opt/emacs-mac/include")
;;               :config
;;               (define-key rime-mode-map (kbd "C-i") 'rime-force-enable)
;;               (setq rime-disable-predicates
;;                     '(rime-predicate-evil-mode-p
;;                       rime-predicate-after-alphabet-char-p
;;                       rime-predicate-current-input-punctuation-p
;;                       rime-predicate-current-uppercase-letter-p
;;                       rime-predicate-punctuation-line-begin-p))
;;               (setq rime-user-data-dir "~/Library/Rime"))

;; 设置主题
(use-package doom-themes
  :defer nil
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
(use-package all-the-icons
  :defer t
  )

;; 自动保存
(use-package super-save
  :defer t
  :config
  (super-save-mode +1))

;; 设置amx，命令快速查找
(use-package amx
  :defer t
  :after ivy
  :init
  (setq amx-save-file "~/.emacs.d/.local/amx-items")
  :bind
	("M-x" . amx)
  )

;; 快速跳转
(use-package avy
  :defer t
  :bind
	("C-c j j" . avy-goto-char-timer)
	("C-c j c" . avy-goto-char)
	("C-c j l" . avy-goto-line)
	)

;; 搜索统计
(use-package anzu
  :defer t
  :hook (after-init . global-anzu-mode))


(use-package helm
  :defer t)

;; 美化modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-modal-icon t)
  (doom-modeline-mode 1))

;; 增加文件的行号
(use-package linum
  :hook (after-init . global-linum-mode)
  :config
  (setq linum-format "%4d  ")
  (set-face-background 'linum nil)
  )

;; wakatime
(use-package wakatime-mode
  :hook (after-init . global-wakatime-mode))

;; 记录命令使用次数
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package alert
  :config
  (setq alert-default-style 'osx-notifier)
  )

(use-package ox-hugo
  :defer t
  :after ox
  :hook (org . org-hugo-auto-export-mode)

  :config
  (setq org-hugo-section "post"
        org-hugo-auto-set-lastmod	t
        )
  )

;; 打开emacs的初始化文件
(defun grass/open-emacs-init ()
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))
;; 加载emacs的初始化文件
(defun grass/load-emacs-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun grass/open-line-next ()
  (interactive)
  (progn
  (move-end-of-line 1)
  (newline)
  (indent-for-tab-command)))

(defun grass/open-line-prev ()
  (interactive)
  (progn
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-for-tab-command)))


(use-package magit)

;; 通用的快捷键绑定
(global-set-key (kbd "C-c j n") 'goto-line)
(global-set-key (kbd "C-c f e i") 'grass/open-emacs-init)
(global-set-key (kbd "C-c f e r") 'grass/load-emacs-init)
(global-set-key (kbd "<C-return>") 'grass/open-line-next)
(global-set-key (kbd "<S-return>") 'grass/open-line-prev)


;; (setq custom-file nil)

(setq custom-file "~/.emacs.d/.local/emacs-custom.el")

(load-file "~/.emacs.d/develop.el")
(load-file "~/.emacs.d/wiki.el")

(load custom-file)

(provide 'init)
