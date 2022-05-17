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
          (branch (format "b/%s--%s" dashed-title card-id)))
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

  (setq org-babel-tmux-session-prefix "ob-")
  ;; (setq org-babel-tmux-terminal (if IS-MAC "iterm" "xfce4-termimal"))
  (setq org-babel-tmux-terminal (if IS-MAC "alacritty" "xfce4-termimal"))
  (setq org-babel-tmux-terminal-opts '("-t" "ob-tmux" "-e"))
  (setq org-babel-tmux-location (if IS-MAC "/usr/local/bin/tmux" "/usr/bin/tmux"))

  (load! "ob-tmux-async"))

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
