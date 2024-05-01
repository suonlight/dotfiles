(after! org
  (setq evil-org-key-theme '(navigation insert textobjects additional calendar todo))

  (setq org-use-sub-superscripts nil)
  (setq org-export-with-sub-superscripts nil)
  (setq org-capture-templates
    `(
       ("e" "English")
       ("er" "Reading")
       ("el" "Listening")
       ("es" "Speaking")
       ("ew" "Writing")
       ("ews"
         "PTE SWT"
         entry
         (file+headline "~/notes/roam/pages/20231023201016-summarize_written_text.org" "Learn")
         "* Item #%:description\n** Front\n\n%:initial** Back\n\n%c")
       ("ewe"
         "PTE WE"
         entry
         (file+headline "~/notes/roam/pages/20231110200450-writing_essay.org" "Learn")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE Essay\n:ANKI_NOTE_TYPE: Basic with Hint\n:END:\n** Front\n\nItem #%:description\n\n%:initial\n\n** Back\n\n%c\n\n** Hint\n\nTotal: 10/15\n\n%(pte-phrase-hints \"%:description\" \"~/notes/roam/assets/english/pte_magic/writing_essay.json\")\n\n")
       ("elw"
         "PTE WFD"
         entry
         (file+headline "~/notes/roam/pages/20231013085723-write_from_dictation.org" "Repeated")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE WFD::Repeated\n:ANKI_NOTE_TYPE: Basic (type in the answer)\n:END:\n** Front\n\n[[../assets/english/%(copy-and-return-file-name \"%:description\").mp3]]\n\n** Back\n\n%:initial\n\n")
       ("ell"
         "PTE L Fill in Blanks"
         entry
         (file+headline "~/notes/roam/pages/20231022124135-l_fill_in_the_blanks.org" "Repeated")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE LFIB::Repeated\n:ANKI_NOTE_TYPE: Cloze (LFIB)\n:END:\n** Audio\n\nItem #%:description\n\n[[../assets/english/%(copy-and-return-file-name \"%:description\").mp3]]\n\n** Text\n\n%:initial\n\n** Explain\n\n")
       ("elh"
         "PTE HIW"
         entry
         (file+headline "~/notes/roam/pages/20231022123442-highlight_incorrect_words.org" "Learn")
         "* Item #%:description\n\n%:initial\n\n[[../assets/english/%(copy-and-return-file-name \"%:description\").mp3]]\n\n")
       ("els"
         "PTE SST"
         entry
         (file+headline "~/notes/roam/pages/20231022112746-summarize_spoken_text.org" "Learn")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE SST::Repeated\n:ANKI_NOTE_TYPE: Basic with Hint and Keywords\n:END:\n** Front\n\nItem #%:description [[../assets/english/%(copy-and-return-file-name \"%:description\").mp3]]\n\n** Back\n\n%:initial\n\n** Keywords\n\n%(pte-phrase-hints \"%:description\" \"~/notes/roam/assets/english/pte_magic/summarize_spoken_text.json\")\n\n** Hint\n\nTotal: \n\n%c\n\n")
       ("esr"
         "PTE RS"
         entry
         (file+headline "~/notes/roam/pages/20231013165737-repeat_sentence.org" "Repeated")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE RS::Repeated\n:ANKI_NOTE_TYPE: Basic (type in the answer) with Hint\n:END:\n** Front\n\nItem #%:description\n\n[[../assets/english/%(copy-and-return-file-name \"%:description\").mp3]]\n\n** Back\n\n%:initial\n\n** Hint\n\n[[../assets/english/%(covert-wav-to-mp3 \"%:description.me\").mp3]]\n\n")
       ("esa"
         "PTE Read Aloud"
         entry
         (file+headline "~/notes/roam/pages/20231016143844-read_aloud.org" "Repeated")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE RA::Repeated\n:ANKI_NOTE_TYPE: Basic with Hint\n:END:\n** Front\n\nItem #%:description\n\n%:initial\n\n** Back\n\n[[../assets/english/%(covert-wav-to-mp3 \"%:description\").mp3]]\n\n** Hint\n\n%(pte-ra-hint \"%:description\")\n\n")
       ("esd"
         "PTE Describe Image"
         entry
         (file+headline "~/notes/roam/pages/20231030201035-describe_image.org" "Learn")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE DI\n:ANKI_NOTE_TYPE: Basic with Hint\n:END:\n** Front\n\nItem #%:description\n\n** Back\n\n%(pte-phrase-hints \"%:description\" \"~/notes/roam/assets/english/pte_magic/describe_image.json\")\n\n** Hint\n\n[[../assets/english/%(covert-wav-to-mp3 \"%:description\").mp3]]\n\n")
       ("esl"
         "PTE Retell Lecture"
         entry
         (file+headline "~/notes/roam/pages/20231106211343-retell_lecture.org" "Learn")
         "* Item #%:description\n\n%:initial\n\n[[../assets/english/%(copy-and-return-file-name \"%:description\").mp3]]\n\n")
       ("erf"
         "PTE RW Fill in Blanks"
         entry
         (file+headline "~/notes/roam/pages/20231014134842-rw_fill_in_the_blanks.org" "Repeated")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE RWFIB::Repeated\n:ANKI_NOTE_TYPE: Cloze (FIB)\n:END:\n** Text\n\nItem #%:description\n\n%c\n\n%:initial\n\n** Explain\n\n** Translation\n\n%:initial\n\n")
       ("erd"
         "PTE R Fill in Blanks - Drag & Drop"
         entry
         (file+headline "~/notes/roam/pages/20231025201731-r_fill_in_the_blanks.org" "Repeated")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE RFIB::Repeated\n:ANKI_NOTE_TYPE: Cloze (FIB)\n:END:\n** Text\n\n%c\n\n%:initial\n\n** Explain\n\n** Translation\n\n%:initial\n")
       ("err"
         "PTE Re-order Paragraphs"
         entry
         (file+headline "~/notes/roam/pages/20231101203133-reorder_paragraphs.org" "Repeated")
         "* Item #%:description\n:PROPERTIES:\n:ANKI_DECK: PTE ROP::Repeated\n:ANKI_NOTE_TYPE: Cloze (FIB)\n:END:\n** Text\n\n%:initial\n\n** Explain\n\n%c\n\n")
       ("v"
         "Vocabulary"
         entry
         (file "~/notes/roam/pages/20231103220135-vocabulary.org")
         "* %i%^{prompt} :vocabulary:\n:PROPERTIES:\n:ANKI_DECK: Vocabulary\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%\\1\n\n** Back\n\n")
       ("c"
         "Code Review"
         entry
         (file ,(format-time-string "~/notes/roam/journals/%Y-%m-%d.org" (current-time) t))
         "* %?\n:PROPERTIES:\n:Source: %F\n:Captured_On: %U\n:END:\n\n#+BEGIN_SRC ruby\n%:initial\n#+END_SRC\n")
       ("N"
         "Notes with Browser"
         entry
         (file ,(format-time-string "~/notes/roam/journals/%Y-%m-%d.org" (current-time) t))
         ;; "* %?\n\nSource: %:link\nCaptured On:%U\n\n%:description\n\n%:initial\n\n")
         "* %:description%?\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:initial\n\n")
       ("n"
         "Notes with Clipboard"
         entry
         (file ,(format-time-string "~/notes/roam/journals/%Y-%m-%d.org" (current-time) t))
         ;; "* %?\n\nSource: %:link\nCaptured On:%U\n\n%:description\n\n%:initial\n\n")
         "* %?\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:description\n\n%c\n\n")
       ("D"
         "EH Debugger"
         entry
         (file ,(format-time-string "~/notes/roam/journals/%Y-%m-%d.org" (current-time) t))
         "* %:description\n:PROPERTIES:\n:Source: %:link\n:Captured_On: %U\n:END:\n\n%:description\n\nSuggestion:\n%(hero/suggest-debuggers \"%:initial\")\n\n")
       ("E"
         "EH Task"
         entry
         (file ,(format-time-string "~/notes/roam/journals/%Y-%m-%d.org" (current-time) t))
         "* TODO %(get-cleansed-title \"%:description\") \n\nGit Branch: %(git-branch-by-title (get-cleansed-title \"%:description\") \"%:link\")\nSource: %:link\nCaptured On: %U\n\n")
       ("A"
         "EH API"
         entry
         (file "~/notes/roam/pages/20210513122118-eh_api.org")
         "* %(hero/get-api-title \"%c\")\n\n#+BEGIN_SRC shell :async :results output :var jwt_token=jwt_token\n%(hero/get-api-curl \"%c\")\n#+END_SRC")))

  (require 'ob-async)
  (require 'org-download))

(defun copy-and-return-file-name (file-name)
  (rename-file
    (concat "~/Downloads/" file-name ".mp3")
    (concat "~/notes/roam/assets/english/" file-name ".mp3")
    t)
  file-name)

(defun covert-wav-to-mp3 (file-name)
  (let ((command (concat "echo y | ffmpeg -i " "~/Downloads/"
      (last-download-file-name) ".wav" " -acodec mp3 "
      "~/notes/roam/assets/english/" file-name ".mp3")))
    (shell-command-to-string command))
  file-name)

(defun last-download-file-name ()
  (s-trim
    (shell-command-to-string "ls -t ~/Downloads | head -n 1 | sed 's/\\..*//'")))

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

(defun pte-phrase-hints (orderId jsonFile)
  (let* ((json (with-temp-buffer
                 (insert-file-contents jsonFile)
                 (json-read)))
         (data (cdr (assoc 'data json)))
         (item (car (seq-filter (lambda (item) (string= orderId (cdr (assoc 'orderId item)))) data)))
         (value (cdr (assoc 'phraseHints item)))
         )
    (if (not value)
        (message "No value")
        (message "Value: %s" value)
        (with-temp-buffer
            (insert value)
            (buffer-string)))))

(defun pte-shadow-audio (orderId jsonFile)
  (let* ((json (with-temp-buffer
                 (insert-file-contents jsonFile)
                 (json-read)))
         (data (cdr (assoc 'data json)))
         (item (car (seq-filter (lambda (item) (string= orderId (cdr (assoc 'orderId item)))) data)))
         (shadow-audio (cdr (assoc 'shadowAudio item)))
         (shadow-local-file (concat (file-name-directory jsonFile) orderId ".shadow.mp3")))
    (if (not shadow-audio)
      (message "No shadow audio")
      (message "Downloading shadow audio %s" shadow-audio)
      (if (not (file-exists-p shadow-local-file))
        (url-copy-file shadow-audio shadow-local-file)))
    shadow-local-file))

(defun pte-shadow-text (orderId jsonFile)
  (let* ((json (with-temp-buffer
                 (insert-file-contents jsonFile)
                 (json-read)))
         (data (cdr (assoc 'data json)))
         (item (car (seq-filter (lambda (item) (string= orderId (cdr (assoc 'orderId item)))) data)))
         (shadow-text (cdr (assoc 'shadowText item)))
         )
    (if (not shadow-text)
        (message "No shadow text")
        (message "Shadow text: %s" shadow-text)
        (with-temp-buffer
            (insert shadow-text)
            (goto-char (point-min))
            (while (search-forward "<span class=\"ra-pause\">[pause]</span>" nil t)
                (replace-match "⏸"))
            (buffer-string)))))

(defun pte-ra-hint (orderId)
  (let* ((jsonFile "~/notes/roam/assets/english/pte_magic/read_aloud.json")
         (shadow-audio (pte-shadow-audio orderId jsonFile))
         (shadow-audio (replace-regexp-in-string "~/notes/roam/assets/" "../assets/" shadow-audio))
         (shadow-text (pte-shadow-text orderId jsonFile))
         (heading (concat
                   (format "%s\n" shadow-text)
                   (format "\n[[%s]]\n" shadow-audio))))
    heading))

(defun get-cleansed-title (title)
  "Get cleansed title"
  (->> title (s-replace "- Jira" "") s-trim))

(after! org-download
  (setq
    org-download-image-org-width 750
    org-download-delete-image-after-download t
    org-download-link-format "[[../assets/%s]]\n"
    org-download-method 'directory)
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir "../assets"))

; (after! ob-tmux
;   (setq org-babel-default-header-args:tmux
;     '((:results . "none")
;        (:session . "default")
;        (:socket  . nil)))

;   (setq org-babel-tmux-session-prefix "ob-")
;   ;; (setq org-babel-tmux-terminal (if IS-MAC "iterm" "xfce4-termimal"))
;   (setq org-babel-tmux-terminal (if IS-MAC "alacritty" "xfce4-termimal"))
;   (setq org-babel-tmux-terminal-opts '("-t" "ob-tmux" "-e"))
;   (setq org-babel-tmux-location (if IS-MAC "/usr/local/bin/tmux" "/usr/bin/tmux"))

;   (load! "ob-tmux-async"))

(after! ob-mermaid
  (setq ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

(after! org-pomodoro
  ;; (setq org-pomodoro-long-break-sound (concat doom-private-dir "/assets/bell.wav"))
  ;; (setq org-pomodoro-ticking-sound (concat doom-private-dir "/assets/bell.wav"))
  (defun open-mindfulness-buffer ()
    (interactive)
    (make-frame '((name . "Mindfulness")))
    (with-current-buffer (get-buffer-create "*mindfulness*")
      (erase-buffer)
      (text-scale-increase 0)
      (insert-image (create-image "~/.config/doom/assets/mindfulness_1.jpg" 'jpeg nil :scale 0.6))
      (insert "\n\nVào, ra\nSâu, chậm\nKhỏe, nhẹ\nLắng, Cười\nHiện tại, Tuyệt vời\n")
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (toggle-frame-maximized)
      (text-scale-increase 5)
      (set-fringe-style '(200 . 200))))

  (add-hook 'org-pomodoro-finished-hook #'open-mindfulness-buffer)
  (setq org-pomodoro-start-sound (f-join doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-finished-sound (f-join doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-overtime-sound  (f-join doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-short-break-sound (f-join doom-private-dir "/assets/bell.wav")))

(after! org-roam
  (setq deft-directory "~/notes/roam")
  (setq org-roam-directory "~/notes/roam")
  (setq org-roam-dailies-directory "journals/")
  (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox-bin")
  (setq org-roam-db-location "~/.config/org-roam.db")
  (setq org-roam-graph-exclude-matcher '("2020-" "2021-" "2022-"))
  (setq org-roam-file-exclude-regexp (-map #'expand-file-name
                                           '("~/notes/roam/logseq/bak/journals"
                                             "~/notes/roam/logseq/bak/pages"
                                             "~/notes/roam/assests"
                                             "~/notes/roam/draws"
                                             "~/notes/roam/logseq"
                                             "~/notes/roam/whiteboards"
                                             "~/notes/.attach/")))
  (setq! +org-roam-open-buffer-on-find-file nil)

  (setq org-roam-capture-templates
    '(("d" "default" plain "%?" :if-new
        (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n\n* What is ${title}?\n\n* Why is ${title}?\n\n* References")
        :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
    '(("d" "journals" entry "* %?" :if-new
        (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+TODO: TODO DOING | DONE\n\n")))))

(after! org-journal
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-dir "~/notes/roam")
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

(use-package! org-excalidraw
  :after org-roam
  :init
  (setq org-excalidraw-directory "~/notes/roam/draws")
  :config
  ;; ;; force activate app Excalidraw first
  ;; (defun org-excalidraw--shell-cmd-open (path os-type)
  ;;   "Construct shell cmd to open excalidraw file with PATH for OS-TYPE."
  ;;   (if (eq os-type 'darwin)
  ;;     (concat "open -a Excalidraw && open " (shell-quote-argument path))
  ;;     (concat "xdg-open " (shell-quote-argument path))))
  (org-excalidraw-initialize))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (require 'justify-kp)

  (add-hook! nov-mode #'writeroom-mode)
  (setq writeroom-width 90)
  (setq +zen-text-scale 2)

  (setq nov-text-width 90)
  (add-hook! nov-mode #'visual-line-mode)

  ;; (setq visual-fill-column-center-text t)
  (add-hook! nov-mode #'visual-fill-column-mode)

  (defun my-nov-font-setup ()
    (interactive)
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
      :height 1.0))

  (add-hook! nov-mode #'my-nov-font-setup)

  (defun my-nov-window-configuration-change-hook ()
    (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
      'my-nov-window-configuration-change-hook
      t))

  (defun my-nov-post-html-render-hook ()
    (if (get-buffer-window)
      (let ((max-width (pj-line-width))
             buffer-read-only)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (not (looking-at "^[[:space:]]*$"))
              (goto-char (line-end-position))
              (when (> (shr-pixel-column) max-width)
                (goto-char (line-beginning-position))
                (pj-justify)))
            (forward-line 1))))
      (add-hook 'window-configuration-change-hook
        'my-nov-window-configuration-change-hook
        nil t)))

  (add-hook! nov-post-html-render #'my-nov-post-html-render-hook)

  (map!
    (:map nov-mode-map
    :n   "o"     #'nov-xwidget-view
    :inv "C-l"   #'evil-window-right
    :inv "C-h"   #'evil-window-left
    :inv "C-j"   #'evil-window-down
    :inv "C-k"   #'evil-window-up)))

(use-package! nov-xwidget
  :commands nov-xwidget-view
  :config
  (add-hook! nov-mode #'nov-xwidget-inject-all-files))

(after! doc-view
  (setq doc-view-resolution 500)
  (map!
    (:map doc-view-mode-map
      :inv "]"       #'doc-view-next-page
      :inv "["       #'doc-view-previous-page
      :inv "C-b"     (lambda () (interactive) (doc-view-previous-line-or-previous-page 10))
      :inv "C-d"     (lambda () (interactive) (doc-view-next-line-or-next-page 10)))))

(use-package! org-appear
  :hook (org-mode . org-appear-mode))

(use-package! org-modern
  :hook (org-mode . org-modern-mode))

(use-package! epc
  :ensure t)

(use-package! org-node
  :hook (org-mode . org-node-cache-mode)
  :config
  (setq org-node-creation-fn #'org-node-new-by-roam-capture)
  (setq org-node-slug-fn #'org-node-slugify-like-roam)
  (setq org-node-creation-hook nil))

(load! "ob-astmux")
