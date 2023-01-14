(when IS-LINUX
  (after! auth-source
    (setq auth-sources '("~/.authinfo")))

  (defun sl/async-run-command (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun sl/exwm-config-custom ()
    "Default configuration of EXWM."
    ;; Set the initial workspace number.
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 2))

    (setq exwm-input-prefix-keys
      '(?\s-x
         ?\C-x
         ?\C-l
         ?\C-h
         ?\C-j
         ?\C-k
         ?\C-c
         ?\C-6
         ?\s-b
         ?\s-w
         ?\s-t
         ?\s-`
         ?\s-1
         ?\s-2
         ?\s-3
         ?\s-4
         ?\s-5
         ?\s-6
         ?\s-7
         ?\s-8
         ?\s-9
         ?\s-0
         ?\C-\ ))

    ;; Global keybindings.
    (unless (get 'exwm-input-global-keys 'saved-value)
      (setq exwm-input-global-keys
        `(
           ;; 's-r': Reset (to line-mode).
           ([?\s-r] . exwm-reset)
           ;; 's-w': Switch workspace.
           ;; ([?\s-w] . exwm-workspace-switch)
           ;; 's-&': Launch application.
           ([?\s-&] . (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

           ;; ([?\s-`] . (lambda ()
           ;;              (interactive)
           ;;              (exwm-workspace-switch-create 0)))

           ;; 's-N': Switch to certain workspace.
           ;; ,@(mapcar (lambda (i)
           ;;             `(,(kbd (format "s-%d" i)) .
           ;;                (lambda ()
           ;;                  (interactive)
           ;;                  (exwm-workspace-switch-create ,i))))
           ;;     (number-sequence 0 9))
           )))

    ;; Line-editing shortcuts
    (unless (get 'exwm-input-simulation-keys 'saved-value)
      (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
           ([?\C-f] . [right])
           ([?\C-p] . [up])
           ([?\C-n] . [down])
           ([?\C-a] . [home])
           ([?\C-e] . [end])
           ([?\M-v] . [prior])
           ([?\C-v] . [next])
           ([?\C-d] . [delete])
           ([?\s-v] . [S-insert])
           ([?\s-c] . [C-c])
           ([?\C-k] . [S-end delete]))))

    (exwm-input-set-key (kbd "s-SPC") #'counsel-linux-app)
    ;; Enable EXWM
    (exwm-enable)
    )

  (defun sl/exwm-init-hook ()
    (exwm-workspace-switch-create 0)
    (menu-bar-mode -1)

    ;; start polybar panel
    (sl/start-panel)

    ;; (sl/async-run-command "pasystray")
    (sl/async-run-command "ibus-daemon --xim"))

  (defun sl/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

  (defun sl/exwm-update-title ()
    (pcase exwm-class-name
      ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

  (defun sl/configure-window-by-class ()
    (interactive)
    (pcase exwm-class-name
      ("Slack" (exwm-layout-toggle-mode-line))))

  (defvar sl/polybar-process nil
    "Holds the process of the running Polybar instance, if any")

  (defun sl/kill-panel ()
    (interactive)
    (when sl/polybar-process
      (ignore-errors
        (kill-process sl/polybar-process)))
    (setq sl/polybar-process nil))

  (defun sl/start-panel ()
    (interactive)
    (sl/kill-panel)
    (setq sl/polybar-process (start-process-shell-command "polybar" nil "polybar -q main -c ~/.config/polybar/config.ini")))

  (defun sl/send-polybar-hook (module-name hook-index)
    (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

  (defun sl/send-polybar-exwm-workspace ()
    (sl/send-polybar-hook "exwm-workspaces" 1))

  (defvar sl/current-buffer nil)

  (defun sl/buffer-icon ()
    (cond
      ((string= major-mode "org-mode") "")
      ((string= major-mode "vterm-mode") "")
      ((string= major-mode "ruby-mode") "")
      ((string= major-mode "js-mode") "")
      ((or (string= major-mode "docker-mode") (string-match-p "Dockerfile" (buffer-name))) "")
      ((string-match-p "Firefox" (buffer-name)) "")
      ((string-match-p "Slack" (buffer-name)) "")
      (t "")))

  (defun sl/send-polybar-emacs-modeline ()
    (setq sl/current-buffer (if buffer-file-truename
                              (format "%s %s %d:%d ⏽ %s %s"
                                (sl/buffer-icon)
                                buffer-file-truename
                                (current-column)
                                (line-number-at-pos)
                                (pcase (coding-system-eol-type buffer-file-coding-system)
                                  (0 "LF")
                                  (1 "CRLF")
                                  (2 "CR"))
                                (coding-system-type buffer-file-coding-system))
                              (format "%s %s" (sl/buffer-icon)
                                (truncate-string-to-width (buffer-name) 100 nil nil t))))
    (sl/send-polybar-hook "emacs-modeline" 1))

  (defun sl/polybar-current-buffer ()
    sl/current-buffer)

  (defun sl/polybar-exwm-workspace ()
    (pcase exwm-workspace-current-index
      (0 "")
      (1 "")
      (2 "")
      (3 "")
      (4 "")))

  (setq-default mode-line-format nil)
  ;; (setq-default doom-modeline-mode nil)

  (use-package! exwm
    :config

    (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)
    (add-hook 'exwm-init-hook #'sl/exwm-init-hook)
    (add-hook 'exwm-update-class-hook #'sl/exwm-update-class)
    (add-hook 'exwm-update-title-hook #'sl/exwm-update-title)
    (add-hook 'exwm-manage-finish-hook #'sl/configure-window-by-class)
    (add-hook 'exwm-workspace-switch-hook #'sl/send-polybar-exwm-workspace)
    (add-hook 'post-command-hook #'sl/send-polybar-emacs-modeline)

    ;; (start-process-shell-command "xrandr" nil "")
    ;; (add-hook 'isearch-update-post-hook 'redraw-display)
    ;; (advice-add 'select-window :around #'sl/send-polybar-emacs-modeline)
    ;; (advice-remove 'select-window #'sl/send-polybar-emacs-modeline)

    ;; ;; load system tray before exwm-init
    ;; (require 'exwm-systemtray)
    ;; (setq exwm-systemtray-height 16)
    ;; (exwm-systemtray-enable)

    (sl/exwm-config-custom)))

(setq default-input-method "vietnamese-telex")
