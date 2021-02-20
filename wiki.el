;;; package -- wiki
;;; Commentary:

(defvar gtd-map (make-sparse-keymap) "所有gtd相关的全局操作都在这里")
(fset 'gtd-command-map gtd-map)
(setq org-agenda-include-diary nil)

;; Org模式相关的，和GTD相关的
(use-package org
  :config
  (setq
   org-directory "~/org/"
   org-startup-folded 'content
   ;; org-agenda-files (list "~/org/")
   org-agenda-files '("~/org")
   org-refile-targets '(("~/org/task.org" :level . 1)
                        ("~/org/project.org" :maxlevel . 2)
                        ("~/org/someday.org" :level . 1)
                        ("~/org/love.org" :level . 1)
                        ("~/org/tickler.org" :maxlevel . 1))
   org-todo-keywords '(
                       (sequence "TODO(t)" "|" "DONE(d!)" "CANCELLED(c@)")
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

  
  (defvar dynamic-agenda-files nil
    "dynamic generate agenda files list when changing org state")

  (defun update-dynamic-agenda-hook ()
    (let ((done (or (not org-state) ;; nil when no TODO list
                    (member org-state org-done-keywords)))
          (file (buffer-file-name))
          (agenda (funcall (ad-get-orig-definition 'org-agenda-files)) ))
      (unless (member file agenda)
        (if done
            (save-excursion
              (goto-char (point-min))
              ;; Delete file from dynamic files when all TODO entry changed to DONE
              (unless (search-forward-regexp org-not-done-heading-regexp nil t)
                (customize-save-variable
                 'dynamic-agenda-files
                 (cl-delete-if (lambda (k) (string= k file))
                               dynamic-agenda-files))))
          ;; Add this file to dynamic agenda files
          (unless (member file dynamic-agenda-files)
            (customize-save-variable 'dynamic-agenda-files
                                     (add-to-list 'dynamic-agenda-files file)))))))

  (defun dynamic-agenda-files-advice (orig-val)
    (cl-union orig-val dynamic-agenda-files :test #'equal))

  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)
  (add-to-list 'org-after-todo-state-change-hook 'update-dynamic-agenda-hook t)


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
  ("C-c n" . gtd-command-map)
  (:map gtd-map
        ("s" . org-save-all-org-buffers)
        ("t" . org-todo-list)
        ("a" . org-agenda-list)
        )
  )

;; 番茄钟
(use-package org-pomodoro
  :after org
  :bind
  (:map gtd-map
  ("p" . org-pomodoro))
  )

;; org标题美化
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))


(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/org/org-roam/")
      :bind
      (:map gtd-map
            ("f" . org-roam-find-file)
            ("j" . org-roam-dailies-find-today))
      )


;;; 定义一个Helm的source，以便选择要粘贴的.org文件
(defvar *org-refile-eof--helm-source* nil
  "用于提供目标.org文件下拉菜单的来源")

;;; 将当前条目剪切并粘贴到某个目标.org文件的末尾
(defun org-refile-to-eof ()
  "将当前条目剪切到一个.org文件的末尾。"
  (interactive)
  ;; 先调用Helm获取目标.org文件。这里需要处理没有选中任何文件的情况
  (let ((path (helm :sources '(*org-refile-eof--helm-source*))))
    (when path
      (org-cut-subtree)
      (save-excursion
        ;; 打开选中的文件的buffer，并移动到最后
        (find-file path)
        (end-of-buffer)
        ;; 调用org-paste-subtree粘贴进去
        (org-paste-subtree)
        ))))

;; refile到文件末尾
(setq *org-refile-eof--helm-source*
      '((name . "refile到下列的哪个文件")
        (candidates . org-agenda-files)
        (action . (lambda (candidate)
                    candidate))))

(provide 'wiki)
