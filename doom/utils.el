(defun sl/new-workspace-and-vterm ()
  (interactive)
  (+workspace/new)
  (multi-vterm))

(defun sl/new-vterm-and-split ()
  (interactive)
  (split-window-below)
  (evil-window-down 1)
  (multi-vterm))

(defun sl/new-vterm-and-vsplit ()
  (interactive)
  (split-window-right)
  (evil-window-right 1)
  (multi-vterm))

(setq sl/jira-cache (make-hash-table :test 'equal))

(defun sl/make-draft-pr ()
  (interactive)
  (let* ((line-beg-pos (line-beginning-position))
          (line-end-pos (line-end-position))
          (pr-title (buffer-substring-no-properties line-beg-pos line-end-pos))
          (pr-template-file (cond ((file-exists-p (concat (projectile-project-root) ".github/PULL_REQUEST_TEMPLATE"))
                                    (concat (projectile-project-root) ".github/PULL_REQUEST_TEMPLATE"))
                              ((file-exists-p (concat (projectile-project-root) ".github/PULL_REQUEST_TEMPLATE.md"))
                                (concat (projectile-project-root) ".github/PULL_REQUEST_TEMPLATE.md"))
                              (t (concat (projectile-project-root) "PULL_REQUEST_TEMPLATE.md"))))
          (current-branch (magit-get-current-branch))
          (pr-template (with-temp-buffer
                         (erase-buffer)
                         (insert-file pr-template-file)
                         (buffer-string)))
          (draft-pr-title (->> (or (gethash current-branch sl/jira-cache) pr-title)
                            (s-trim)
                            (s-replace "# " "")
                            (format "---\ndraft: true\ntitle: \"%s\"\n---\n")))
          (issue-url (->> current-branch
                       (s-split "--")
                       (nth 1)
                       (s-concat "https://employmenthero.atlassian.net/browse/")))
          (commits (->> (format "%s log --pretty=format:\"- %s\" %s..HEAD" magit-git-executable "%s" forge--buffer-base-branch)
                     (shell-command-to-string))))
    (delete-region (point-min) (point-max))
    (insert draft-pr-title)
    (insert pr-template)
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (re-search-forward "^# ")
    (+default/newline-below)
    (+default/newline-below)
    (insert issue-url)
    (re-search-forward "^# ")
    (+default/newline-below)
    (+default/newline-below)
    (insert commits)
    (evil-normal-state)))

(defun sl/build-reports (file)
  (find-file file)
  (org-babel-execute-buffer))

(defun sl/run-reports (report)
  (with-current-buffer report
    (goto-char (point-min))
    (while (re-search-forward "tmux .* :file" nil t 1) (org-open-at-point))))

(defun sl/js-log-func ()
  (interactive)
  (goto-char (region-end))
  (insert "(e))")
  (goto-char (region-beginning))
  (insert "e => (console.log({ e }), "))

(defun sl/js-log-var ()
  (interactive)
  (let* ((selected-var (buffer-substring-no-properties (region-beginning) (region-end))))
    (goto-char (region-end))
    (insert ")")
    (goto-char (region-beginning))
    (insert (format "(console.log({ %s }), " selected-var))))

(defun sl/insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun sl/send-cmd-to-multi-vterm-project ()
  (interactive)
  (let ((cmd (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         (window (selected-window)))
    (multi-vterm-project)
    (vterm-send-string cmd)
    (vterm-send-return)
    (select-window window)))

;; Split the windows sensibly.
;; https://gitlab.com/jabranham/emacs/blob/master/init.el#L2537
(defun sl/split-below-last-buffer (prefix)
    "Split the window above/below and display the previous buffer.
If prefix arg is provided, show current buffer twice."
    (interactive "p")
    (split-window-below)
    (other-window 1 nil)
    (if (= prefix 1)
        (switch-to-next-buffer)))

(defun sl/split-right-last-buffer (prefix)
  "Split the window left/right and display the previous buffer
If prefix arg is provided, show current buffer twice."
  (interactive "p")
  (split-window-right)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

(defun words-at-point () (insert (->> (substring-no-properties (car kill-ring)) s-split-words (-map #'s-downcase) (s-join " "))))

(defun sl/projectile-find-file-at-point ()
  (interactive)
  (kill-new (ivy-thing-at-point))
  (minibuffer-with-setup-hook
    'words-at-point
    (call-interactively #'counsel-projectile-find-file)))

(defun sl/eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "yarn eslint --fix " (buffer-file-name))))

(defun sl/eslint-fix-file-and-revert ()
  (interactive)
  (sl/eslint-fix-file)
  (revert-buffer t t))

(defun sl/roam-list-todos ()
  (interactive)
  (+default/search-project-for-symbol-at-point "\\* TODO"))
