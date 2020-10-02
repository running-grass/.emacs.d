;;; package -- sss
;;; Commentary:

;;; Code:

;; 引入package，以便使用package功能
(require 'package)
;; 设置清华镜像仓库
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ))
;; 自动把包下载安装到.cache中
(setq package-user-dir "~/.emacs.d/.cache/elpa")
(package-initialize)

;; 自动安装use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;关闭启动画面
(setq inhibit-startup-message t)
;; 自动安装所有的包
(setq use-package-always-ensure t)
;; 默认查找目录为home目录
(setq command-line-default-directory "~")
;; 定义全局的leader-map
(defvar global-leader-map (make-sparse-keymap)
  "全局Leader快捷键映射表")

;; 设置evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-leader nil (kbd "C-SPC"))
  (define-key evil-normal-state-map (kbd "SPC") global-leader-map)
  (define-key evil-motion-state-map (kbd "SPC") global-leader-map)
  (define-key evil-visual-state-map (kbd "SPC") global-leader-map)
  (evil-mode 1)
  )

(use-package amx
  :init
  (setq amx-save-file "~/.emacs.d/.cache/amx-items")
  :config (amx-mode))
(use-package magit)

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

;; 绑定大多数package的evil绑定
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; TLDR,快捷查看命令的用法
(use-package tldr
  :commands tldr
  )

(use-package general
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-define-key
   :states '(normal visual motion)
   "," nil)

  (general-create-definer global-leader-def
    :prefix ",")

  (global-leader-def

    :states '(normal motion)
    "SPC" 'amx


    ;; files
    "f f" 'find-file
    "f e i" '(lambda ()
	       (interactive)
	       (find-file-existing "~/.emacs.d/init.el"))
    "f e r" '(lambda ()
	       (interactive)
	       (load-file "~/.emacs.d/init.el"))

    "f t" 'neotree-toggle
    "f s" 'helm-do-ag-this-file

    ;; org
    "j j" 'org-capture





    ;; projectile
    "p t" 'neotree-toggle
    "p p" 'helm-projectile-switch-project
    "p f f" 'helm-projectile-find-file
    "p f d" 'helm-projectile-find-dir
    "p s" 'helm-projectile-ag
    "p b" 'helm-projectile-switch-buffer
    "p r" 'helm-projectile-recentf

    ;; buffer
    "b b" 'helm-buffers-list
    "b d" 'kill-this-buffer
    "b r" 'helm-recentf

    ;; window
    "w d" 'delete-window
    "w v" 'evil-window-split
    "w h" 'evil-window-vsplit
    "w i" 'delete-other-windows

    ;; evilnc
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator ; if you prefer backslash key


    "r r" '(load-file '(read-file-name /Users/grass/.emacs.d/init.el))
    "r f" 'format-all-buffer

    "m a" 'package-install

    "l f" 'load-file

    "q q" 'save-buffers-kill-emacs

    "t" 'org-todo
    )
  )

(use-package company
  :config
  (global-company-mode 1)
  )

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

(use-package org
  :init
  (setq org-directory "~/org/")
  (setq org-agenda-files (list "~/org/"))

  (setq org-default-notes-file (concat org-directory "/notes.org"))

  :bind
  (:map global-leader-map
	("a" . org-agenda))
  )

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/mugeda/" "~/workspace/"))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

(use-package neotree
  :config
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)


  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
	      (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

  )

(use-package recentf
:init
(setq recentf-save-file "~/.emacs.d/.cache/recentf")
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  (recentf-mode)
  )

;; 增加文件的行号
(use-package linum
  :config
  (global-linum-mode t)
  (setq linum-format "%4d  ")
  (set-face-background 'linum nil)
  )

(use-package wakatime-mode
  :config
  (global-wakatime-mode))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package evil-nerd-commenter)

(use-package format-all
  :config
  (format-all-mode))

(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

  )



(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  )

(use-package js2-mode)

(use-package helm
  :config
  (helm-mode))



(use-package helm-ag
  :init
  (add-to-list 'exec-path "/usr/local/bin")
  )

(use-package helm-projectile
  :config
  (helm-projectile-on)
  )
(use-package helm-themes)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; 打开emacs的初始化文件
(defun gremacs/open-emacs-init ()
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))
;; 加载emacs的初始化文件
(defun gremacs/load-emacs-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))


(define-key global-leader-map "d" '("files"))
(define-key global-leader-map "df" '("files"))
(define-key global-leader-map "fei" '("打开Emacs配置文件" . gremacs/open-emacs-init))
(define-key global-leader-map "fer" 'gremacs/load-emacs-init)


(provide 'init)
;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-collection tldr wakatime-modo org-bullets helm-themes helm-projectile helm-ag helm js2-mode web-mode magit amx rainbow-delimiters format-all evil-nerd-commenter editorconfig keyfreq wakatime-mode neotree projectile which-key winum company general evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
