;;; package -- reset
;;; Commentary: reset emacs build-in feature

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)
;; 关闭文件滑动控件
(scroll-bar-mode -1)
;; 关闭启动画面
(setq inhibit-startup-message t)

;; 初始化一些全局变量

;; 设置2个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default default-tab-width 2)

;; 默认查找目录为home目录
(setq command-line-default-directory "~")

(setq bookmark-default-file "~/.emacs.d/.cache/bookmarks")

(set-frame-font "Source Code Pro 20" 14 t)

;; 添加nix的执行路径
(add-to-list 'exec-path "~/.nix-profile/bin")
(add-to-list 'exec-path "/usr/local/bin")

(provide 'reset)
