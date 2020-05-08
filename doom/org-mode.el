(after! org
  (setq evil-org-key-theme '(navigation insert textobjects additional calendar todo))

  (setq org-capture-templates
    '(
       ("v"
         "Vocabulary"
         entry
         (file "~/org-modes/flashcards.org")
         "* %i%^{prompt} :vocabulary:\n:PROPERTIES:\n:ANKI_DECK: Vocabulary\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%\\1\n\n** Back\n\n")
       ("a"
         "Appointment"
         entry
         (file+headline "~/org-modes/personal.org" "Appointments")
         "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n"
         )
       ("L"
         "Notes"
         entry
         (file "~/org-modes/notes.org")
         "* %:description\n\nSource: %:link\nCaptured On:%U\n\n%:initial\n\n"
         :immediate-finish
         :prepend)
       ("E"
         "Employment Hero Task"
         entry
         (file "~/org-modes/employmenthero.org")
         "* TODO %:initial\n\nGit Branch: %(git-branch-by-title \"%:initial\" \"%:link\")\nSource: %:link\nCaptured On:%U\n\n")
       ("e"
         "Employment Hero Task"
         entry
         (file "~/org-modes/employmenthero.org")
         "* TODO %?")))

  (require 'org-download))

(defun org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.
The Org block can be *any*: src, example, verse, etc., even any
Org Special block.
This function is heavily adapted from `org-between-regexps-p'."
  (save-match-data
    (let ((pos (point))
           (case-fold-search t)
           (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
           (limit-up (save-excursion (outline-previous-heading)))
           (limit-down (save-excursion (outline-next-heading)))
           beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
               (re-search-backward block-begin-re limit-up :noerror))
          (setq beg (match-beginning 0))
          ;; ... and BLOCK-END-RE after it...
          (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                (match-string-no-properties 1)
                                "\\( .*\\)*$")))
            (goto-char (match-end 0))
            (re-search-forward block-end-re limit-down :noerror))
          (> (setq end (match-end 0)) pos)
          ;; ... without another BLOCK-BEGIN-RE in-between.
          (goto-char (match-beginning 0))
          (not (re-search-backward block-begin-re (1+ beg) :noerror))
          ;; Return value.
          (cons beg end))))))

(defun org-split-block ()
  "Sensibly split the current Org block at point.
(1) Point in-between a line
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message▮ \"one\")                   (message \"one\")
    (message \"two\")          -->       #+end_src
    #+end_src                          ▮
               #+begin_src emacs-lisp
               (message \"two\")
               #+end_src
(2) Point at EOL
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message \"one\")▮                   (message \"one\")
    (message \"two\")          -->       #+end_src
    #+end_src                          ▮
               #+begin_src emacs-lisp
               (message \"two\")
               #+end_src
(3) Point at BOL
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message \"one\")                    (message \"one\")
    ▮(message \"two\")          -->      #+end_src
    #+end_src                          ▮
               #+begin_src emacs-lisp
               (message \"two\")
               #+end_src
"
  (interactive)
  (if (org-in-any-block-p)
    (save-match-data
      (save-restriction
        (widen)
        (let ((case-fold-search t)
               (at-bol (bolp))
               block-start
               block-end)
          (save-excursion
            (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
            (setq block-start (match-string-no-properties 0))
            (setq block-end (replace-regexp-in-string
                              "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                              (match-string-no-properties 1))))
          ;; Go to the end of current line, if not at the BOL
          (unless at-bol
            (end-of-line 1))
          (insert (concat (if at-bol "" "\n")
                    block-end
                    "\n\n"
                    block-start
                    (if at-bol "\n" "")))
          ;; Go to the line before the inserted "#+begin_ .." line
          (beginning-of-line (if at-bol -1 0)))))
    (message "Point is not in an Org block")))

(defun org-agenda-only-window ()
  (interactive)
  (let ((org-agenda-window-setup 'only-window))
    (org-agenda nil "a")
    (call-interactively 'org-agenda-day-view)))

(defun git-branch-by-title (title link)
  "Auto generate git branch by title"
  (let* ((dashed-title (s-dashed-words title))
          (card-id (car (last (s-split "/" link)))))
    (message "%s/%s--%s"
      (if (or (s-contains? "refactor" dashed-title)
            (s-contains? "chore" dashed-title))
        "chore" "ft")
      dashed-title
      card-id)))

(after! org-download
  (setq
    org-download-image-org-width 800
    org-download-delete-image-after-download t
    org-download-link-format "[[file:%s]]\n"
    org-download-method 'directory)
  (setq-default org-download-image-dir "./images"))

(after! ob-tmux
  (setq org-babel-tmux-terminal "iterm")
  (setq org-babel-default-header-args:tmux
    '((:results . "silent")
       (:session . "default")
       (:socket  . nil)))
  (setq org-babel-tmux-session-prefix "ob-")
  (setq org-babel-tmux-location "/usr/local/bin/tmux"))

(after! ob-mermaid
  (setq ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

(after! org-pomodoro
  ;; (setq org-pomodoro-long-break-sound (concat doom-private-dir "/assets/bell.wav"))
  ;; (setq org-pomodoro-ticking-sound (concat doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-start-sound (concat doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-finished-sound (concat doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-overtime-sound  (concat doom-private-dir "/assets/bell.wav"))
  (setq org-pomodoro-short-break-sound (concat doom-private-dir "/assets/bell.wav")))
