;;; package -- sss
;;; Commentary:

;;; Code:

;; use-package初始化
(progn
;; 引入package，以便使用package功能
(require 'package)
;; 设置清华镜像仓库
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; 自动把包下载安装到.cache中
(setq package-user-dir "~/.emacs.d/.cache/elpa")
(package-initialize)
;; 自动安装use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-compute-statistics t)
)

;; 初始化一些全局变量
(progn
;;关闭启动画面
(setq inhibit-startup-message t)
;; 自动安装所有的包
(setq use-package-always-ensure t)

;; 默认查找目录为home目录
(setq command-line-default-directory "~")

(setq bookmark-default-file "~/.emacs.d/.cache/bookmarks")
;; 定义全局的leader-map
(defvar global-leader-map (make-sparse-keymap)
  "全局Leader快捷键映射表")
)


;; 内置模块的一些功能
(progn
;; 窗口的撤销/恢复功能
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode)
  :bind
  (:map global-leader-map
	("wz" . winner-undo)
	("wZ" . winner-redo)
))

;; 保存了上一次打开文件时的光标位置
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file "~/.emacs.d/.local/places")
  :hook (after-init . save-place-mode))

;; buffer相关的设置
(use-package ibuffer
  :bind
  (:map global-leader-map
	("bg" . ibuffer)))

;; 高亮当前行
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; 显示/隐藏结构化的数据
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (
	 ;; :map evil-normal-state-map
	 :map global-leader-map
         ("TAB" . hs-toggle-hiding)
        )
  )

;; 保存折叠状态
(use-package persistent-overlays
  :init
  (setq persistent-overlays-directory "~/.emacs.d/.cache")
  :hook (hs-minor-mode . persistent-overlays-minor-mode)
  )

;; 简单文件指示
(use-package simple
  :ensure nil
  :hook (after-init . (lambda ()
                         (line-number-mode)
                         (column-number-mode)
                         (size-indication-mode))))

;; 显示空白字符
(use-package whitespace
  :ensure nil
  :hook (after-init . global-whitespace-mode)

  :config
    (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
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
  :ensure nil
  :config (global-so-long-mode 1))

;; 文件被外部程序修改后，重新载入buffer
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; 注释/反注释
(use-package newcomment
  :ensure nil
  :bind (:map global-leader-map ( "cl" . comment-or-uncomment))
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
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; 高亮显示配对的大括号
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; 窗口切换
(use-package window
  :ensure nil
  :bind
  (:map global-leader-map
	("wd" . delete-window)
	("wo" . delete-other-windows)
	)
  )

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  :hook (after-init . recentf-mode)
  )
)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package org-web-tools)


;; 设置主题
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  )


;; 安装icon管理
(use-package all-the-icons)

;; 自动保存
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

;; 番茄钟
(use-package org-pomodoro
  :bind
  (:map global-leader-map
	("ocp" . org-pomodoro))
  )

;; org标题美化
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;; 设置evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :hook (after-init . evil-mode)
  :config
  (progn
  (evil-define-state move
  "Test state."
  :tag "<m>"
  (message (if (evil-move-state-p)
               "Enabling test state."
             "Disabling test state.")))

  (evil-set-initial-state 'org-agenda-mode 'normal)

  (define-key evil-normal-state-map (kbd "SPC") global-leader-map)
  (define-key evil-motion-state-map (kbd "SPC") global-leader-map)
  (define-key evil-visual-state-map (kbd "SPC") global-leader-map)
  (define-key evil-emacs-state-map  (kbd "SPC") global-leader-map)

  (define-key evil-normal-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-visual-state-map (kbd "RET") nil)
  (define-key evil-emacs-state-map  (kbd "RET") nil)

  ;; 把，作为本地模式的保留按键
  (define-key evil-normal-state-map (kbd ",") nil)
  (define-key evil-motion-state-map (kbd ",") nil)
  (define-key evil-visual-state-map (kbd ",") nil)
  (define-key evil-emacs-state-map  (kbd ",") nil)

  (define-key evil-insert-state-map (kbd ",") 'self-insert-command)
  )
  :bind
  (:map evil-move-state-map
	("j" . evil-next-line)
	("k" . evil-previous-line)
	("h" . evil-backward-char)
	("l" . evil-forwark-char)

	:map global-leader-map
	("wv" . evil-window-vsplit)
	("wh" . evil-window-split)
  )
  )

;; 为常用包配置evil按键
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; 设置amx，命令快速查找
(use-package amx
  :after ivy
  :init
  (setq amx-save-file "~/.emacs.d/.local/amx-items")
  :bind
  (:map global-leader-map
	("SPC" . amx))
  )

;; 快速跳转
(use-package avy
  :bind
  (:map global-leader-map
	("jj" . avy-goto-char-timer)
	("jc" . avy-goto-char)
	("jl" . avy-goto-line)
	))

;; 括号的多色彩
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; 搜索统计
(use-package anzu
  :hook (after-init . global-anzu-mode))

;; ivy智能提示后端
(use-package ivy
  :init
  ;; 可以使switch-buffer集成recentf
  (setq ivy-use-virtual-buffers t)
  :hook (after-init . ivy-mode)
  :bind
  (:map global-leader-map
	("bb" . ivy-switch-buffer)
	))

;; 快捷键提示
(use-package which-key
  :init
  ;; 为define-key增加注释
  (setq which-key-enable-extended-define-key t)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  :config
  ;; 替换快捷键提示符
  (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
	which-key-replacement-alist)
  ;; 忽略winum-select-window-[1-9]这9个提示
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
  (which-key-mode)
  )

;; 自动补全
(use-package company
  :config
  (global-company-mode 1)
  )

;; 为窗口绑定序号
(use-package winum
  :config
  (defun winum-assign-0-to-neotree ()
    (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 0))
  (add-to-list 'winum-assign-functions 'winum-assign-0-to-neotree)
  (winum-mode)
  :bind
  (
   :map global-leader-map
   ;; 选择窗口
   ("0" . winum-select-window-0)
   ("1" . winum-select-window-1)
   ("2" . winum-select-window-2)
   ("3" . winum-select-window-3)
   ("4" . winum-select-window-4)
   ("5" . winum-select-window-5)
   ("6" . winum-select-window-6)
   ("7" . winum-select-window-7)
   ("8" . winum-select-window-8)
   ("9" . winum-select-window-9)
   )
  )


;; 窗口切换
(use-package ace-window
  :init
  (setq aw-dispatch-always nil)
  :bind
  (:map global-leader-map
	("ww" . ace-window)
	)
  )

;; Org模式相关的，和GTD相关的
(use-package org
  :init
  (setq org-directory "~/org/")
  (setq org-agenda-files (list "~/org/"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "收集箱")
           "* TODO %?\n  %i\n  %a")
          ("j" "日记" entry (file+datetree "~/org/gtd.org" "日记")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-todo-keywords
        '(
          (sequence "TODO(t!)" "WAIT(w@)" "|" "DONE(d!)" "CANCELED(c@)")
          ))

  :bind
  (:map global-leader-map
	("oa" . org-agenda)
	("ot" . org-todo-list)
	("oo" . org-capture)
	("oci" . org-clock-in)
	("oco" . org-clock-out)
	:map org-mode-map
	(",," . org-todo)
	(",r" . org-refile)
	)
  )

;; 最近打开的文件
(use-package recentf
  :init
  (setq recentf-save-file "~/.emacs.d/.local/recentf")
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  :config
  (recentf-mode)
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

(use-package pocket-reader
  :bind
  (:map global-leader-map
	("lp" . pocket-reader)))

;; 配置editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; 打开emacs的初始化文件
(defun gremacs/open-emacs-init ()
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))
;; 加载emacs的初始化文件
(defun gremacs/load-emacs-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; 通用的快捷键绑定
(progn
(define-key global-leader-map "f" '("files"))
(define-key global-leader-map "ff" 'find-file)
(define-key global-leader-map "fe" '("emacs file"))
(define-key global-leader-map "fei" '("打开Emacs配置文件" . gremacs/open-emacs-init))
(define-key global-leader-map "fer" '("重新加载Emacs配置文件" . gremacs/load-emacs-init))
(define-key global-leader-map "h" '("help"))
(define-key global-leader-map "hp" 'describe-package)
(define-key global-leader-map "hf" 'describe-function)
(define-key global-leader-map "hv" 'describe-variable)
(define-key global-leader-map "hk" 'describe-key)
(define-key global-leader-map "hK" 'describe-keymap)

(define-key global-leader-map "qq" '("退出Emacs" . save-buffers-kill-emacs))
)

(provide 'init)
;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell pocket-reader anzu rainbow-delimiters window zenburn-theme winum which-key wakatime-mode use-package super-save persistent-overlays org-superstar org-pomodoro keyfreq ivy evil-collection editorconfig company amx all-the-icons ace-window))
 '(wakatime-cli-path
   "/usr/local/Cellar/wakatime-cli/13.0.7/libexec/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
