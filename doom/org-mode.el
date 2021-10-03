(after! org
  (setq evil-org-key-theme '(navigation insert textobjects additional calendar todo))

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
         (file ,(format-time-string "~/org-modes/roam/daily/%Y-%m-%d.org" (current-time) t))
         "* %?\n:PROPERTIES:\n:Source: %F\n:Captured_On: %U\n:END:\n\n#+BEGIN_SRC ruby\n%:initial\n#+END_SRC\n")
       ("N"
         "Notes with Browser"
         entry
         (file ,(format-time-string "~/org-modes/roam/daily/%Y-%m-%d.org" (current-time) t))
         ;; "* %?\n\nSource: %:link\nCaptured On:%U\n\n%:description\n\n%:initial\n\n")
         "* %?\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:description\n\n%:initial\n\n")
       ("n"
         "Notes with Clipboard"
         entry
         (file ,(format-time-string "~/org-modes/roam/daily/%Y-%m-%d.org" (current-time) t))
         ;; "* %?\n\nSource: %:link\nCaptured On:%U\n\n%:description\n\n%:initial\n\n")
         "* %?\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:description\n\n%c\n\n")
       ("D"
         "EH Debugger"
         entry
         (file ,(format-time-string "~/org-modes/roam/daily/%Y-%m-%d.org" (current-time) t))
         "* %:description\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:description\n\nSuggestion:\n%(hero/suggest-debuggers \"%:initial\")\n\n")
       ("E"
         "EH Task"
         entry
         (file ,(format-time-string "~/org-modes/roam/daily/%Y-%m-%d.org" (current-time) t))
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
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir "./images"))

(after! ob-tmux
  (setq org-babel-default-header-args:tmux
    '((:results . "silent")
       (:session . "default")
       (:socket  . nil)))

  (setq ob-tmux-delimiters '((ruby . "#####") (sh . "#####")))

  (setq org-babel-tmux-session-prefix "ob-")
  ;; (setq org-babel-tmux-terminal (if IS-MAC "iterm" "xfce4-termimal"))
  (setq org-babel-tmux-terminal (if IS-MAC "alacritty" "xfce4-termimal"))
  (setq org-babel-tmux-terminal-opts '("-t" "ob-tmux" "-e"))
  (setq org-babel-tmux-location (if IS-MAC "/usr/local/bin/tmux" "/usr/bin/tmux"))

  (defun ob-tmux--generate-uuid ()
    "Generate a 32 character UUID."
    (md5 (number-to-string (random 100000000))))

  (defun org-babel-execute:tmux (body params)
    "Send a block of code via tmux to a terminal using Babel.
\"default\" session is used when none is specified.
Argument BODY the body of the tmux code block.
Argument PARAMS the org parameters of the code block."
    (message "Sending source code block to interactive terminal session...")
    (save-window-excursion
      (let* ((org-session (cdr (assq :session params)))
              (terminal (cdr (assq :terminal params)))
              (lang (cdr (assq :lang params)))
              (lang-with-default (or lang "sh"))
              (delimiter (cdr (assq (intern lang-with-default) ob-tmux-delimiters)))
              (jid (ob-tmux--generate-uuid))
              (start-delimiter (format "%s start:%s" delimiter jid))
              (finish-delimiter (format "%s finish:%s" delimiter jid))
              (file (cdr (assq :file params)))
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
          (org-babel-expand-body:generic (format "%s\n%s\n%s" start-delimiter body finish-delimiter) params))
        (org-babel-insert-result jid '("replace"))
        (async-start
          `(lambda ()
             (setq exec-path ',exec-path)
             (setq load-path ',load-path)
             (package-initialize)
             (org-babel-do-load-languages 'org-babel-load-languages ',org-babel-load-languages)
             (while (not (string-match ,finish-delimiter (ob-tmux--execute-string ,ob-session
                                                           "capture-pane"
                                                           "-J"
                                                           "-p" ;; print to stdout
                                                           "-S" "-" ;; start at beginning of history
                                                           "-t" (ob-tmux--target ,ob-session))))
               (sleep-for 0.1))
             (require 'dash)
             (let* ((raw-output (ob-tmux--execute-string ,ob-session
                                  "capture-pane"
                                  "-J"
                                  "-p" ;; print to stdout
                                  "-S" "-" ;; start at beginning of history
                                  "-t" (ob-tmux--target ,ob-session)))
                     (after-start-delimiter (->> raw-output (s-split ,start-delimiter) -last-item))
                     (after-finish-delimiter (->> raw-output (s-split ,finish-delimiter) -last-item))
                     (output (->> (s-replace after-finish-delimiter "" after-start-delimiter)
                               (s-replace ,start-delimiter "")
                               (s-replace ,finish-delimiter "")
                               (s-replace-regexp "=> .*" "")
                               (s-replace-regexp "irb\([a-z]+\):[0-9]+:[0-9]+.*" "")
                               (s-replace-regexp "^\[[0-9]+\] .* pry\(.*\).*" "")
                               (s-replace-regexp "^>> .*" "")
                               (s-replace-regexp "^nil." "")
                               (s-replace-regexp "[\n]+" "\n")
                               s-trim
                               (s-split "\n")
                               (-map #'s-trim)
                               (s-join "\n"))))
               output))
          `(lambda (result)
             (with-current-buffer ,(current-buffer)
               (save-excursion
                 (let* ((default-directory ,default-directory)
                         (file ,file))
                   (goto-char (point-min))
                   (search-forward ,jid)
                   (search-backward "src")
                   (if (eq 'nil file)
                     (org-babel-insert-result result '("replace"))
                     (write-region result nil file)
                     (org-babel-insert-result file '("file" "replace"))))))))))))

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
  (setq! +org-roam-open-buffer-on-find-file nil)

  (setq org-roam-capture-templates
    '(("d" "default" plain "%?" :if-new
        (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n\n* What is ${title}?\n\n* Why is ${title}?\n\n* References")
        :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
    '(("d" "daily" entry "* %?" :if-new
        (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+TODO: TODO IN-PROGRESS | DONE\n\n* Morning Routines\n\n- Check Calendar\n- Watch Code Review\n- Run Squad Reports\n- Read Finance News")))))

(after! org-journal
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-dir "~/Dropbox/org-modes/roam")
  (setq org-journal-date-format "%A, %d %B %Y")
  ;; (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
  ;; (add-to-list 'org-agenda-files org-journal-dir)
  )

; (use-package! org-roam-server
;   :commands org-roam-server-mode
;   :config
;   (setq org-roam-server-port 8081))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    :hook (org-roam . org-roam-ui-mode))
