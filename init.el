;;; package -- sss
;;; Commentary:

;;; Code:

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;;关闭启动画面
(setq inhibit-startup-message t)
(setq org-agenda-include-diary nil)


;; (setq my-lisp-dir "/Users/grass/.emacs.d/develop.el")
;; (setq load-path (cons my-lisp-dir load-path))


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

;; 启动时全屏
;; (unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  ;; (toggle-frame-fullscreen))

;; 初始化一些全局变量
;; 设置2个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default default-tab-width 2)

;; 默认查找目录为home目录
(setq command-line-default-directory "~")

(setq bookmark-default-file "~/.emacs.d/.cache/bookmarks")
;; 定义全局的leader-map
(defvar global-leader-map (make-sparse-keymap)
  "全局Leader快捷键映射表")

(defvar gtd-local-map (make-sparse-keymap)
  "自己和gtd相关的快捷键映射表")
(define-key global-leader-map (kbd "o") gtd-local-map)
(define-key global-map (kbd "M-SPC") global-leader-map)

(set-frame-font "Source Code Pro 20" nil t)


;; 快捷键提示
(use-package which-key
  :config
  ;; 为define-key增加注释
  (setq which-key-enable-extended-define-key t)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)

  ;; 替换快捷键提示符
  (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
	which-key-replacement-alist)
  ;; 忽略winum-select-window-[1-9]这9个提示
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
  (which-key-mode)
  )

;; 内置模块的一些功能

;; 窗口的撤销/恢复功能
(use-package winner-mode
  :straight nil
  :hook (after-init . winner-mode)
  :bind
  (:map global-leader-map
	("wz" . winner-undo)
	("wZ" . winner-redo)
	))

(use-package dired
  :straight nil
  :config
  (define-key dired-mode-map (kbd "SPC") global-leader-map)
  )

;; 保存了上一次打开文件时的光标位置
(use-package saveplace
  :straight nil
  :init
  (setq save-place-file "~/.emacs.d/.local/places")
  :hook (after-init . save-place-mode))

;; buffer相关的设置
(use-package ibuffer
  :straight nil
  :bind
  (:map global-leader-map
	("bg" . ibuffer)))

;; 高亮当前行
(use-package hl-line
  :straight nil
  :hook (after-init . global-hl-line-mode))

;; 显示/隐藏结构化的数据
(use-package hideshow
  :straight nil
  :hook (prog-mode . hs-minor-mode)
  :bind (
	 :map global-leader-map
         ("TAB" . hs-toggle-hiding)
         )
  )

;; 保存折叠状态
;; (use-package persistent-overlays
;;   :init
;;   (setq persistent-overlays-directory "~/.emacs.d/.cache")
;;   :hook (hs-minor-mode . persistent-overlays-minor-mode)
;;   )

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
  :straight nil
  :config (global-so-long-mode 1))

;; 文件被外部程序修改后，重新载入buffer
(use-package autorevert
  :straight nil
  :hook (after-init . global-auto-revert-mode))

;; 注释/反注释
(use-package newcomment
  :straight nil
  :bind (
         :map global-leader-map
         ( "/" . comment-or-uncomment)
         )
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

;; 窗口切换
(use-package window
  :straight nil
  :bind
  (:map global-leader-map
	("wd" . delete-window)
	("wo" . delete-other-windows)
	)
  )

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

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; 安装icon管理
(use-package all-the-icons)

;; 自动保存
(use-package super-save
  :config
  (super-save-mode +1))

(use-package counsel-projectile
  :config
  (setq
   projectile-known-projects-file "~/.emacs.d/.cache/projectile-bookmarks.eld"
   projectile-project-search-path '("~/mugeda/" "~/workspace/" "~/")
   )
  (projectile-discover-projects-in-search-path)
  :bind
  (:map global-leader-map
	("pf" . counsel-projectile-find-file )
	("pP" . counsel-projectile-switch-open-project)
	("pp" . counsel-projectile-switch-project)
	("pb" . counsel-projectile-switch-to-buffer)
	("ps" . counsel-projectile-ag)
	))

(use-package treemacs-projectile
  :config
  :bind
  (
   :map global-leader-map
       ("pt" . treemacs-projectile)
       ("0" . treemacs-select-window)
	 ))

;; 设置evil
;; (use-package evil
;;   :init
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
;;   :hook
;;   (after-init . evil-mode )
;;   :config
;;   (setq evil-default-state 'emacs)
;;   ;; (evil-set-initial-state 'prog-mode 'normal)
;;   ;; (evil-set-initial-state 'text-mode 'normal)
;;   ;; (evil-set-initial-state 'special-mode 'emacs)
;;   (progn

;;     (define-key evil-emacs-state-map  (kbd "M-p") global-leader-map)

;;     ;; 把，作为本地模式的保留按键
;;     (define-key evil-emacs-state-map  (kbd "M-,") nil)
;;     )
;;   :bind
;;   (
;;    :map global-leader-map
;;    ("wv" . evil-window-vsplit)
;;    ("wh" . evil-window-split)
;;    ("bd" . evil-delete-buffer)
;;    :map evil-visual-state-map
;;    ("RET" . nil)
;;    ("," . nil)
;;    ("SPC" . nil)
;;    ("SPC" . global-leader-map)
;;    ("f" . indent-region)
;;    ("/" . comment-or-uncomment)
;;    )
;;   )

;; 为常用包配置evil按键
;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

;; (use-package treemacs-evil
;;   :after treemacs)

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
  :config
  ;; 可以使switch-buffer集成recentf
  (setq ivy-use-virtual-buffers t)
  :hook (after-init . ivy-mode)
  :bind
  (:map global-leader-map
	("bb" . ivy-switch-buffer)
	))

;; 自动补全
(use-package company
  :config
  (global-company-mode 1)
  )

;; 为窗口绑定序号
(use-package winum
  :config
  (defun winum-assign-0-to-neotree ()
    (when (string-match-p (buffer-name) ".*Neotree.*") 0))
  (add-to-list 'winum-assign-functions 'winum-assign-0-to-neotree)
  (winum-mode)
  :bind
  (
   :map global-leader-map
   ;; 选择窗口
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
  :config
  (setq
   org-directory "~/org/"
   org-startup-folded 'content
   ;; org-agenda-files (list "~/org/")
   org-agenda-files '("~/org/inbox.org"
                      "~/org/task.org"
                      "~/org/issues.org"
                      "~/org/project.org"
                      "~/org/journal.org"
                      "~/org/tickler.org")
   org-refile-targets '(("~/org/task.org" :level . 1)
                        ("~/org/project.org" :maxlevel . 2)
                        ("~/org/someday.org" :level . 1)
                        ("~/org/love.org" :level . 1)
                        ("~/org/tickler.org" :maxlevel . 1))
   org-todo-keywords '(
                       (sequence "TODO(t!)" "DOING(e!)" "WAITING(w@)" "|" "DONE(d!)" "CANCELLED(c@)")
                       (sequence "MERGED(m!)" "|" )
                       (sequence "REPORT(r!)" "|" )
                       )
   org-clock-string-limit 5
   org-log-refile 'time
   org-log-done 'note
   org-log-into-drawer "LOGBOOK"
   org-clock-stored-history t
   org-tag-alist '(
                   (:startgroup . nil)
                   ("@office" . ?o)
                   ("@home" . ?h)
                   (:endgroup . nil)
                   )
   org-capture-templates '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox") "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:RELATED: %a\n:END:")
			                     ("j" "日记" entry (file+datetree "~/org/journal.org" "Journal") "* %?\n:PROPERTIES:\n:CREATED: %U\n:RELATED: %a\n:END:"))

   org-agenda-custom-commands '(("p" "At the office" tags-todo "project"
      ((org-agenda-overriding-header "Office")
       (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first))))
   )

  (defun my-org-agenda-skip-all-siblings-but-first ()
    "跳过除第一个未完成条目之外的所有条目。"
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (org-entry-is-todo-p))

  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Return `org-capture' template string for new Hugo post."
      (let* ((date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
             (title (read-from-minibuffer "Post Title: "))
             (file-name (read-from-minibuffer "File Name: "))
             (fname (org-hugo-slug file-name)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ,(concat ":EXPORT_DATE: " date)
                     ":END:"
                     "%?\n")
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"
                   "Hugo post"
                   entry
                   (file+olp "~/workspace/blog/post.org" "Blog Ideas")
                   (function org-hugo-new-subtree-post-capture-template))))

  :bind
  (
   :map org-mode-map
   ("," . nil)
   (",," . org-todo)
   (",s" . org-schedule)
   (",d" . org-deadline)
   (",t" . org-toggle-checkbox)
   (",p" . org-pomodoro)
   (",r" . org-refile)
   (",g" . org-set-tags-command)
   (",ci" . org-clock-in)
   (",co" . org-clock-out)
   (",a" . org-archive-subtree-default)
   :map global-leader-map
   ("a" . org-agenda)
   ("c" . org-capture)
   ("oci" . org-clock-in)
   ("oco" . org-clock-out)
   ("ocp" . org-pomodoro)
   ("os" . org-save-all-org-buffers)
   )
  )

;; 番茄钟
(use-package org-pomodoro
  :after org
  :bind
  (
   :map gtd-local-map
   ("p" . org-pomodoro)
   )
  )



;; 绑定快捷键
(use-package org-agenda
  :straight nil
  :after org
  :bind
  (
   :map org-agenda-mode-map
   ("," . nil)
   (",," . org-agenda-todo)
   (",s" . org-agenda-schedule)
   (",r" . org-agenda-refile)
   (",d" . org-agenda-deadline)
   (",p" . org-pomodoro)
   (",a" . org-agenda-archive-default)
   (",ci" . org-agenda-clock-in)
   (",co" . org-agenda-clock-out)
   (",g" . org-agenda-set-tags)
   ))

;; org标题美化
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package helm
  :defer t)
;; 支持redmine
(use-package org-redmine
  :config
  (setq
   org-redmine-limit 100 ; 列表请求条数
   org-redmine-uri "http://redmine.mugeda.com" ; redmine域名
   org-redmine-auth-api-key "63535623b7f690f8c12c8c94b1d827466196cb9a" ; redmine的Api
   org-redmine-template-anything-source "#%i% (%d_date%) [%p_n%] %s% @%as_n%" ; issue列表展示模板
   )
  (defun org-redmine-show-mine-issue ()
    ;; "展示分配给我的Issue"
    (interactive)
    ;; 可选参数 是否为只加载自己的issue
    (org-redmine-helm-show-issue-all t))
  :bind
  (
   :map global-leader-map
	 ("lr" . 'org-redmine-show-mine-issue)
	 ("lR" . 'org-redmine-helm-show-issue-all)
   :map org-mode-map
	 (",ir" . 'org-redmine-get-issue))
  )

;; 美化modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-modal-icon t)
  (doom-modeline-mode 1))

;; 快速选择工具
;; (use-package expand-region
;;   :after evil
;;   :bind
;;   (:map evil-visual-state-map
;; 	("v" . er/expand-region))
;;   )
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

(use-package pocket-reader
  :bind
  (:map global-leader-map
	("lp" . pocket-reader)))

;; 配置swiper
(use-package swiper
  :bind
  (:map global-leader-map
	("fs" . swiper-isearch)))

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

;; (use-package company-tabnine
;;   :config
;;   (add-to-list 'company-backends #'company-tabnine)
;;   ;; Trigger completion immediately.
;;   (setq company-idle-delay 0)

;;   ;; Number the candidates (use M-1, M-2 etc to select completions).
;;   (setq company-show-numbers t)
;;   )

(use-package ox-hugo
  :after ox
  :hook (org. org-hugo-auto-export-mode)

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

(use-package ledger-mode
  :ensure-system-package ledger
  )

(use-package magit)


;; 通用的快捷键绑定
(define-key global-leader-map "ff" 'find-file)
(define-key global-leader-map "fe" '("emacs file"))
(define-key global-leader-map "fei" '("打开Emacs配置文件" . gremacs/open-emacs-init))
(define-key global-leader-map "fer" '("重新加载Emacs配置文件" . gremacs/load-emacs-init))
(define-key global-leader-map "hp" 'describe-package)
(define-key global-leader-map "hf" 'describe-function)
(define-key global-leader-map "hv" 'describe-variable)
(define-key global-leader-map "hk" 'describe-key)
(define-key global-leader-map "hK" 'describe-keymap)

(define-key global-leader-map "qq" '("退出Emacs" . save-buffers-kill-emacs))


(load-file "~/.emacs.d/develop.el")

(provide 'init)
;; init.el ends here
