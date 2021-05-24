(after! org
  (setq evil-org-key-theme '(navigation insert textobjects additional calendar todo))
  ;; (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev))
  ;; (set-company-backend! 'org-mode 'company-org-roam)

  (setq org-capture-templates
    `(
       ("v"
         "Vocabulary"
         entry
         (file "~/org-modes/flashcards.org")
         "* %i%^{prompt} :vocabulary:\n:PROPERTIES:\n:ANKI_DECK: Vocabulary\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%\\1\n\n** Back\n\n")
       ("c"
         "Code Review"
         entry
         (file ,(format-time-string "~/org-modes/roam/%Y-%m-%d.org" (current-time) t))
         "* %?\n:PROPERTIES:\n:Source: %F\n:Captured_On: %U\n:END:\n\n#+BEGIN_SRC ruby\n%:initial\n#+END_SRC\n")
       ("N"
         "Notes with Browser"
         entry
         (file ,(format-time-string "~/org-modes/roam/%Y-%m-%d.org" (current-time) t))
         ;; "* %?\n\nSource: %:link\nCaptured On:%U\n\n%:description\n\n%:initial\n\n")
         "* %?\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:description\n\n%:initial\n\n")
       ("n"
         "Notes with Clipboard"
         entry
         (file ,(format-time-string "~/org-modes/roam/%Y-%m-%d.org" (current-time) t))
         ;; "* %?\n\nSource: %:link\nCaptured On:%U\n\n%:description\n\n%:initial\n\n")
         "* %?\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:description\n\n%c\n\n")
       ("D"
         "EH Debugger"
         entry
         (file ,(format-time-string "~/org-modes/roam/%Y-%m-%d.org" (current-time) t))
         "* %:description\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:description\n\nSuggestion:\n%(hero/suggest-debuggers \"%:initial\")\n\n")
       ("E"
         "EH Task"
         entry
         (file ,(format-time-string "~/org-modes/roam/%Y-%m-%d.org" (current-time) t))
         "* TODO %(get-cleansed-title \"%:description\") \n\nGit Branch: %(git-branch-by-title (get-cleansed-title \"%:description\") \"%:link\")\nSource: %:link\nCaptured On: %U\n\n")
       ("A"
         "EH API"
         entry
         (file "~/org-modes/roam/20210513122118-eh_api.org")
         "* %(hero/get-api-title \"%c\")\n\n#+BEGIN_SRC shell :async :results output :var jwt_token=jwt_token\n%(hero/get-api-curl \"%c\")\n#+END_SRC")))

  (require 'org-download))

(defun org-agenda-only-window ()
  (interactive)
  (let ((org-agenda-window-setup 'only-window))
    (org-agenda nil "a")
    (call-interactively 'org-agenda-day-view)))

(defun git-branch-by-title (title link)
  "Auto generate git branch by title"
  (let* ((card-id (->> link (s-split "/") last car))
          (dashed-title (->> title
                          (s-replace card-id "")
                          s-dashed-words))
          (branch (format "%s/%s--%s"
                    (cond ((s-contains? "refactor" dashed-title) "chore")
                      ((s-contains? "chore" dashed-title) "chore")
                      ((s-contains? "sentry" dashed-title) "df")
                      ((s-contains? "p1" dashed-title) "df")
                      ((s-contains? "p2" dashed-title) "df")
                      ((s-contains? "p3" dashed-title) "df")
                      (t "ft"))
                    dashed-title
                    card-id)))
    (puthash branch title sl/jira-cache) ;; write to cache
    branch))

(defun get-cleansed-title (title)
  "Get cleansed title"
  (->> title (s-replace "- Jira" "") s-trim))

(after! org-download
  (setq
    org-download-image-org-width 750
    org-download-delete-image-after-download t
    org-download-link-format "[[file:./images/%s]]\n"
    org-download-method 'directory)
  (setq-default org-download-image-dir "./images"))

(after! ob-tmux
  (setq org-babel-default-header-args:tmux
    '((:results . "silent")
       (:session . "default")
       (:socket  . nil)))

  (setq ob-tmux-delimiters '((ruby . "#####") (sh . "#####")))

  (setq org-babel-tmux-session-prefix "ob-")
  (setq org-babel-tmux-terminal (if IS-MAC "iterm" "xfce4-termimal"))
  (setq org-babel-tmux-location (if IS-MAC "/usr/local/bin/tmux" "/usr/bin/tmux"))

  (defun org-babel-execute:tmux (body params)
    "Send a block of code via tmux to a terminal using Babel.
\"default\" session is used when none is specified.
Argument BODY the body of the tmux code block.
Argument PARAMS the org parameters of the code block."
    (message "Sending source code block to interactive terminal session...")
    (save-window-excursion
      (let* ((org-session (cdr (assq :session params)))
              (terminal (cdr (assq :terminal params)))
              (lang (or (cdr (assq :lang params)) "sh"))
              (delimiter (cdr (assq (intern lang) ob-tmux-delimiters)))
              (socket (cdr (assq :socket params)))
              (socket (when socket (expand-file-name socket)))
              (ob-session (ob-tmux--from-org-session org-session socket))
              (session-alive (ob-tmux--session-alive-p ob-session))
              (window-alive (ob-tmux--window-alive-p ob-session)))
        ;; Create tmux session and window if they do not yet exist
        (unless session-alive (ob-tmux--create-session ob-session))
        (unless window-alive (ob-tmux--create-window ob-session))
        ;; Start terminal window if the session does not yet exist
        (unless session-alive
          (ob-tmux--start-terminal-window ob-session terminal))
        ;; Wait until tmux window is available
        (while (not (ob-tmux--window-alive-p ob-session)))
        ;; Disable window renaming from within tmux
        (ob-tmux--disable-renaming ob-session)
        (ob-tmux--send-body ob-session
          (org-babel-expand-body:generic (format "%s\n%s" delimiter body) params)))))

  (defun ob-tmux--insert-result ()
    (interactive)
    (let ((info (org-babel-get-src-block-info 'light)))
      (when (and info (string-equal "tmux" (nth 0 info)))
        (let* ((params (nth 2 info))
                (org-session (cdr (assq :session params)))
                (file (cdr (assq :file params)))
                (socket (cdr (assq :socket params)))
                (socket (when socket (expand-file-name socket)))
                (lang (cdr (assq :lang params)))
                (delimiter (cdr (assq (intern lang) ob-tmux-delimiters)))
                (ob-session (ob-tmux--from-org-session org-session socket))
                (output (->> (ob-tmux--execute-string ob-session
                               "capture-pane"
                               "-J"
                               "-p" ;; print to stdout
                               "-S" "-" ;; start at beginning of history
                               "-t" (ob-tmux--target ob-session))
                          (s-split delimiter)
                          last
                          car
                          (s-replace-regexp "=> .*" "")
                          (s-replace-regexp "irb\([a-z]+\):[0-9]+:[0-9]+.*" "")
                          (s-replace-regexp "\n\n" "\n")
                          s-trim
                          (s-split "\n")
                          (-map #'s-trim)
                          (s-join "\n"))))
          (if (eq 'nil file)
            (org-babel-insert-result output '("replace"))
            (write-region output nil file)
            (org-babel-insert-result file '("file" "replace")))))))

  (defun ob-tmux--edit-result ()
    (interactive)
    (pcase (org-babel-get-src-block-info 'light)
      (`(,_ ,_ ,arguments ,_ ,_ ,start ,_)
        (save-excursion
          ;; Go to the results, if there aren't any then run the block.
          (goto-char start)
          (goto-char (or (org-babel-where-is-src-block-result)
                       (progn (org-babel-execute-src-block)
                         (org-babel-where-is-src-block-result))))
          (end-of-line)
          (skip-chars-forward " \r\t\n")
          ;; (org-edit-special)
          (delete-trailing-whitespace)
          (end-of-buffer)
          t))
      (_ nil)))

  (defun ob-tmux--open-src-block-result (orig-fun &rest args)
    (let ((info (org-babel-get-src-block-info 'light)))
      (if (and info (string-equal "tmux" (nth 0 info)))
        (progn
          (ob-tmux--insert-result)
          (ob-tmux--edit-result))
        (apply orig-fun args))))

  (advice-add 'org-babel-open-src-block-result
    :around #'ob-tmux--open-src-block-result))

(after! ob-mermaid
  (setq ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

(after! org-pomodoro
  ;; (setq org-pomodoro-long-break-sound (concat doom-private-dir "/assets/bell.wav"))
  ;; (setq org-pomodoro-ticking-sound (concat doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-start-sound (concat doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-finished-sound (concat doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-overtime-sound  (concat doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-short-break-sound (concat doom-private-dir "/assets/bell.wav")))

(after! org-roam
  (setq deft-directory "~/Dropbox/org-modes/roam")
  (setq org-roam-directory "~/Dropbox/org-modes/roam")
  (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox-bin")
  (setq org-roam-db-location "~/.config/emacs/org-roam.db")
  (setq org-roam-graph-exclude-matcher '("2020-" "2021-"))

  (setq org-roam-capture-templates
    '(("d" "default" plain
        #'org-roam-capture--get-point
        "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}"
        :head "#+TITLE: ${title}\n\n* What is ${title}?\n\n* Why is ${title}?\n\n* References"
        :unnarrowed t
        :immediate-finish t)))

  (setq org-roam-dailies-capture-templates
    '(("d" "daily" plain (function org-roam-capture--get-point)
        ""
        :immediate-finish t
        :file-name "%<%Y-%m-%d>"
        :head "#+TITLE: %<%Y-%m-%d>\n#+TODO: TODO IN-PROGRESS | DONE\n\n* Check Calendar"))))

(after! org-journal
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-dir "~/Dropbox/org-modes/roam")
  (setq org-journal-date-format "%A, %d %B %Y")
  ;; (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
  ;; (add-to-list 'org-agenda-files org-journal-dir)
  )

(use-package! org-roam-server
  :commands org-roam-server-mode
  :config
  (setq org-roam-server-port 8081))

