;;; package -- wiki
;;; Commentary:

(setq org-agenda-include-diary nil)

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
                      "~/org/tickler.org"
                      "~/org/org-roam/daily/")
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
  ;; (org-babel-do-load-languages
  ;;     'org-babel-load-languages
  ;;     '((emacs-lisp . t)
  ;;       (plantuml . t)
  ;;       ))

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
   ("C-c o s" . org-save-all-org-buffers)
   )
  )

;; 番茄钟
(use-package org-pomodoro
  :after org
  :bind
  ("C-c o p" . org-pomodoro)
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
      (:map org-roam-mode-map
            ("C-c n l" . org-roam)
            ("C-c n f" . org-roam-find-file)
            ("C-c n j" . org-roam-dailies-find-today)
            ("C-c n g" . org-roam-graph))
      (:map org-mode-map
            ("C-c n i" . org-roam-insert)
            ("C-c d" . org-roam-dailies-)
            ("C-c n I" . org-roam-insert-immediate)))




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
