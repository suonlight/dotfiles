;;; -*- lexical-binding: t; -*-
(defun projectile-directory-path ()
  "Retrieve the directory path relative to project root.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'.

  Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
				(file-name-directory file-name)
			      list-buffers-directory))
    (file-relative-name
     (file-truename directory-name)
     (projectile-project-root))))

(defun projectile-file-path ()
  "Retrieve the file path relative to project root.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-name (buffer-file-name))
    (file-relative-name (file-truename file-name) (projectile-project-root))))

(defun projectile-file-path-with-line ()
  "Retrieve the file path relative to project root, including line number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-path (projectile-file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun projectile-file-path-with-line-column ()
  "Retrieve the file path relative to project root, including line and column number.

  This function respects the value of the `column-number-indicator-zero-based'
  variable.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-path (projectile-file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
			    ;; Emacs 26 introduced this variable.
			    ;; Remove this check once 26 becomes the minimum version.
			    (boundp column-number-indicator-zero-based)
			    (not column-number-indicator-zero-based))
			   (1+ (current-column))
			 (current-column))))))

(defun projectile-copy-directory-path ()
  "Copy and show the directory path relative to project root.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (projectile-directory-path))
      (message "%s" (kill-new directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun projectile-copy-file-path ()
  "Copy and show the file path relative to project root."
  (interactive)
  (if-let (file-path (projectile-file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun projectile-copy-file-path-with-line ()
  "Copy and show the file path relative to project root, including line number."
  (interactive)
  (if-let (file-path (projectile-file-path-with-line))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun projectile-copy-file-path-with-line-column ()
  "Copy and show the file path relative to project root, including line and column number.

  This function respects the value of the `column-number-indicator-zero-based'
  variable."
  (interactive)
  (if-let (file-path (projectile-file-path-with-line-column))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun directory-path ()
  "Retrieve the directory path of the current buffer.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'.

  Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
				(file-name-directory file-name)
			      list-buffers-directory))
    (file-truename directory-name)))

(defun file-path ()
  "Retrieve the file path of the current buffer.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun file-path-with-line-column ()
  "Retrieve the file path of the current buffer, including line and column number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
			    ;; Emacs 26 introduced this variable.
			    ;; Remove this check once 26 becomes the minimum version.
			    (boundp column-number-indicator-zero-based)
			    (not column-number-indicator-zero-based))
			   (1+ (current-column))
			 (current-column))))))

(defun copy-directory-path ()
  "Copy and show the directory path of the current buffer.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (directory-path))
      (message "%s" (kill-new directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (file-path))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (file-path)))
      (message "%s" (kill-new file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
  buffer."
  (interactive)
  (if-let (file-name (file-name-base (file-path)))
      (message "%s" (kill-new file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (file-path-with-line))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun copy-file-path-with-line-column ()
  "Copy and show the file path of the current buffer, including line and column number.

  This function respects the value of the `column-number-indicator-zero-based'
  variable."
  (interactive)
  (if-let (file-path (file-path-with-line-column))
      (message "%s" (kill-new file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
	     (assoc ?_ register-alist))
	(jump-to-register ?_)
      (progn
	(window-configuration-to-register ?_)
	(delete-other-windows)))))

;; (defadvice org-switch-to-buffer-other-window
;;     (after supress-window-splitting activate)
;;   "Delete the extra window if we're in a capture frame"
;;   (if (equal "capture" (frame-parameter nil 'name))
;;       (delete-other-windows)))

;; (defadvice org-capture-finalize
;;     (after delete-capture-frame activate)
;;   "Advise capture-finalize to close the frame"
;;   (if (equal "capture" (frame-parameter nil 'name))
;;       (delete-frame)))

;; (defun activate-capture-frame (pid app key)
;;   "run org-capture in capture frame"
;;   (select-frame-by-name "capture")
;;   (message "pid: %s - app: %s - key: %s" pid app key)
;;   ;; (switch-to-buffer (get-buffer-create "*scratch*"))
;;   (org-capture))

(defun activate-capture-frame (&optional pid title keys)
  "Run ‘org-capture’ in capture frame.
PID is a pid of the app (the caller is responsible to set that right)
TITLE is a title of the window (the caller is responsible to set that right)
KEYS is a string associated with a template (will be passed to `org-capture')"
  (setq systemwide-capture-previous-app-pid pid)
  (select-frame-by-name "capture")
  (set-frame-position nil 400 400)
  (set-frame-size nil 1000 400 t)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture nil keys)
  (delete-other-windows))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (when (and (equal "capture" (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
    (ag/switch-to-app systemwide-capture-previous-app-pid)
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise ‘org-refile’ to close the frame."
  (delete-frame))

(defun ag/switch-to-app (pid)
  "Using third party tools tries to switch to the app with the given PID"
  (when (and pid (eq system-type 'darwin))
    (call-process (executable-find "hs") nil 0 nil "-c"
                  (concat "require(\"emacs\").switchToApp (\"" pid "\")"))))

(defun activate-capture-notes ()
  "run org-capture in capture frame"
  (switch-to-buffer (get-buffer-create "CAPTURE-notes.org")))

(defun counsel-rg-from-current-directory ()
  (interactive)
  (let ((directory-name (read-directory-name "Dir: " (file-name-directory buffer-file-name))))
    (counsel-rg "" directory-name)))

(defun counsel-rg-thing-at-point ()
  (interactive)
  (counsel-rg (ivy-thing-at-point)))

(defun counsel-find-models ()
  (interactive "P")
  (counsel-projectile-find-file))

(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun open-mindfulness-buffer ()
  (interactive)
  (make-frame '((name . "Mindfulness")))
  (with-current-buffer (get-buffer-create "*mindfulness*")
    (erase-buffer)
    (text-scale-increase 0)
    (insert-image (create-image "~/.emacs.d/assets/mindfulness_1.jpg" 'imagemagick nil :height 300 :width 300))
    (insert "\n\nVào, ra\nSâu, chậm\nKhỏe, nhẹ\nLắng, Cười\nHiện tại, Tuyệt vời\n")
    (switch-to-buffer (current-buffer))
    (delete-other-windows)
    (toggle-frame-maximized)
    (text-scale-increase 5)
    (set-fringe-style '(200 . 200))))

(defun open-zsh-file ()
  (interactive)
  (find-file "~/.zshrc"))

(defun list-processes-other-window ()
  (interactive)
  (list-processes)
  (switch-to-buffer-other-window "*Process List*"))

(defun copy-this-file ()
  "Copy this file"
  (interactive)
  (dired-jump)
  (dired-do-copy)
  (dired-find-file))

(defun mouse-dumb-jump-go ()
  ""
  (dumb-jump-go))

(defun split-window-vertically-2 ()
  "Split screen to 2 windows"
  (interactive)
  (let* ((w (selected-window))
        (previous-place (evil-alternate-buffer)))
    (delete-other-windows)
    (split-window-right)
    (select-window (next-window))
    (when previous-place
      (switch-to-buffer (car previous-place))
      (goto-char (car (last previous-place))))
    (select-window w)))

(defun split-window-vertically-3 ()
  "Split screen to 2 windows"
  (interactive)
  (let ((w (selected-window)))
    (delete-other-windows)
    (split-window-right)
    (select-window (next-window))
    (evil-switch-to-windows-last-buffer)
    (split-window-right)
    (select-window (next-window))
    (centaur-tabs-backward-tab)
    (balance-windows-area)
    (select-window w)))

(defun prodigy-as-default-layout ()
  (interactive)
  (sl/layout-switch-by-pos 0)
  (prodigy))

(defun evil-multi-libvterm-projectile ()
  (interactive)
  (if (string= major-mode "vterm-mode")
    (delete-window)
    (multi-libvterm-projectile)
    (evil-insert-state))
  (balance-windows-area))

(defun persp-terminal ()
  (interactive)
  (let ((terminal-p (persp-with-name-exists-p "terminal")))
    (persp-switch "terminal")
    (unless terminal-p (multi-libvterm))))

(require 'ansi-color)
(defun display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

(defun org-agenda-only-window ()
  (interactive)
  (let ((org-agenda-window-setup 'only-window))
    (sl/layout-switch-by-pos 0)
    (org-agenda nil "a")
    (call-interactively 'org-agenda-day-view)))
