;;; -*- lexical-binding: t; -*-

(defun org-drill-sound ()
  (interactive)
  (if-let ((drill-sound (org-entry-get (point) "DRILL_SOUND")))
    (start-process-shell-command "mplayer" "*sound*" (format "mplayer %s" (expand-file-name drill-sound)))
    (read-aloud--string (org-get-heading t t t t) "word")))

(defvar org-drill-p nil)
(defun org-drill-wrapper ()
  (interactive)
  (unless org-drill-p
    (use-package org-drill
      :after org
      :straight (org-drill :type git :host gitlab :repo "phillord/org-drill")
      :defer 1
      :commands org-drill
      :config
      (message "Loading org-drill...")

      (defvar org-drill--repeat-key ?r "")
      (defun org-drill-presentation-prompt-in-mini-buffer (session &optional prompt)
        "Create a card prompt with a timer and user-specified    if returns
    (or (cdr (assoc ch returns)))
 menu.

Arguments:

PROMPT: A string that overrides the standard prompt.
"
        (let* ((item-start-time (current-time))
                (input nil)
                (ch nil)
                (prompt
                  (or prompt
                    (format (concat "Press key for answer, "
                              "%c=edit, %c=tags, %c=skip, %c=repeat, %c=quit.")
                      org-drill--edit-key
                      org-drill--tags-key
                      org-drill--skip-key
                      org-drill--repeat-key
                      org-drill--quit-key)))
                (full-prompt
                  (org-drill--make-minibuffer-prompt session prompt)))
          (org-drill-sound)
          (if (and (eql 'warn org-drill-leech-method)
                (org-drill-entry-leech-p))
            (setq full-prompt (concat
                                (propertize "!!! LEECH ITEM !!!
You seem to be having a lot of trouble memorising this item.
Consider reformulating the item to make it easier to remember.\n"
                                  'face '(:foreground "red"))
                                full-prompt)))
          (while (memq ch '(nil org-drill--tags-key))
            (setq ch nil)
            (while (not (input-pending-p))
              (let ((elapsed (time-subtract (current-time) item-start-time)))
                (message (concat (if (>= (time-to-seconds elapsed) (* 60 60))
                                   "++:++ "
                                   (format-time-string "%M:%S " elapsed))
                           full-prompt))
                (sit-for 1)))
            (setq input (org-drill--read-key-sequence nil))
            (if (stringp input) (setq ch (elt input 0)))
            (if (eql ch org-drill--tags-key)
              (org-set-tags-command)))
          (cond
            ((eql ch org-drill--quit-key) nil)
            ((eql ch org-drill--edit-key) 'edit)
            ((eql ch org-drill--skip-key) 'skip)
            (t t))))
      (defun org-drill-reschedule (session)
        "Returns quality rating (0-5), or nil if the user quit."
        (let ((ch nil)
               (input nil)
               (next-review-dates (org-drill-hypothetical-next-review-dates))
               (typed-answer-statement (if (oref session typed-answer)
                                         (format "Your answer: %s\n"
                                           (oref session typed-answer))
                                         ""))
               (key-prompt (format "(0-5, %c=help, %c=edit, %c=tags, %c=repeat, %c=quit)"
                             org-drill--help-key
                             org-drill--edit-key
                             org-drill--tags-key
                             org-drill--repeat-key
                             org-drill--quit-key)))
          (save-excursion
            (while (not (memq ch (list org-drill--quit-key
                                   org-drill--edit-key
                                   7          ; C-g
                                   ?0 ?1 ?2 ?3 ?4 ?5)))
              (run-hooks 'org-drill-display-answer-hook)
              (setq input (org-drill--read-key-sequence
                            (if (eq ch org-drill--help-key)
                              (format "0-2 Means you have forgotten the item.
3-5 Means you have remembered the item.

0 - Completely forgot.
1 - Even after seeing the answer, it still took a bit to sink in.
2 - After seeing the answer, you remembered it.
3 - It took you awhile, but you finally remembered. (+%s days)
4 - After a little bit of thought you remembered. (+%s days)
5 - You remembered the item really easily. (+%s days)

%sHow well did you do? %s"
                                (round (nth 3 next-review-dates))
                                (round (nth 4 next-review-dates))
                                (round (nth 5 next-review-dates))
                                typed-answer-statement
                                key-prompt)
                              (format "%sHow well did you do? %s"
                                typed-answer-statement key-prompt))))
              (cond
                ((stringp input)
                  (setq ch (elt input 0)))
                ((and (vectorp input) (symbolp (elt input 0)))
                  (cl-case (elt input 0)
                    (up (ignore-errors (forward-line -1)))
                    (down (ignore-errors (forward-line 1)))
                    (left (ignore-errors (backward-char)))
                    (right (ignore-errors (forward-char)))
                    (prior (ignore-errors (scroll-down))) ; pgup
                    (next (ignore-errors (scroll-up)))))  ; pgdn
                ((and (vectorp input) (listp (elt input 0))
                   (eventp (elt input 0)))
                  (cl-case (car (elt input 0))
                    (wheel-up (ignore-errors (mwheel-scroll (elt input 0))))
                    (wheel-down (ignore-errors (mwheel-scroll (elt input 0)))))))
              (if (eql ch org-drill--tags-key)
                (org-set-tags-command))
              (if (eql ch org-drill--repeat-key)
                (org-drill-sound))))
          (cond
            ((and (>= ch ?0) (<= ch ?5))
              (let ((quality (- ch ?0))
                     (failures (org-drill-entry-failure-count)))
                (unless (oref session cram-mode)
                  (save-excursion
                    (let ((quality (if (org-drill--entry-lapsed-p session) 2 quality)))
                      (org-drill-smart-reschedule quality
                        (nth quality next-review-dates))))
                  (push quality (oref session qualities))
                  (cond
                    ((<= quality org-drill-failure-quality)
                      (when org-drill-leech-failure-threshold
                        ;;(setq failures (if failures (string-to-number failures) 0))
                        ;; (org-set-property "DRILL_FAILURE_COUNT"
                        ;;                   (format "%d" (1+ failures)))
                        (if (> (1+ failures) org-drill-leech-failure-threshold)
                          (org-toggle-tag "leech" 'on))))
                    (t
                      (let ((scheduled-time (org-get-scheduled-time (point))))
                        (when scheduled-time
                          (message "Next review in %d days"
                            (- (time-to-days scheduled-time)
                              (time-to-days (current-time))))
                          (sit-for 0.5)))))
                  (org-set-property "DRILL_LAST_QUALITY" (format "%d" quality))
                  (org-set-property "DRILL_LAST_REVIEWED"
                    (org-drill-time-to-inactive-org-timestamp (current-time))))
                quality))
            ((= ch org-drill--edit-key)
              'edit)
            (t nil))))
      )
    (setq org-drill-p t))
  (org-drill))

(use-package org
  ;; :defer 10
  ;; :straight (org :type git :host github :repo "emacsmirror/org")
  :straight org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-imenu-depth 3)
  (setq org-confirm-babel-evaluate nil)
  (setq org-log-done 'time)
  (defvar org-babel-do-load-languages-p nil)
  :config
  ;; (require 'org-notify)
  ;; (org-notify-start)

  ;; ;; Example setup:
  ;; ;;
  ;; (org-notify-add 'appt
  ;; 		  '(:time "-1s" :period "20s" :duration 10
  ;; 			  :actions (-message -ding))
  ;; 		  '(:time "15m" :period "2m" :duration 100
  ;; 			  :actions -notify)
  ;; 		  '(:time "2h" :period "5m" :actions -message)
  ;; 		  '(:time "3d" :actions -email))

  ;; (defalias 'origin-org-babel-execute-src-block 'org-babel-execute-src-block)
  (defun sl/org-babel-execute-src-block (&optional orig-fun arg info params)
    (interactive "P")
    (unless org-babel-do-load-languages-p
      (use-package ob-async
        :after org
        :config (advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block))

      (use-package ob-tmux
        ;; Install package automatically (optional)
        :after org
        :init
        (setq org-babel-tmux-terminal "iterm")
        :custom
        (org-babel-default-header-args:tmux
          '((:results . "silent")	 ;
             ;; (:results . "output")	 ;
             (:session . "default")	 ; The default tmux session to send code to
             (:socket  . nil)))
        ;; The tmux sessions are prefixed with the following string.
        ;; You can customize this if you like.
        (org-babel-tmux-session-prefix "ob-")
        ;; Finally, if your tmux is not in your $PATH for whatever reason, you
        ;; may set the path to the tmux binary as follows:
        (org-babel-tmux-location "/usr/local/bin/tmux"))

      (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
           (dot . t)
           (sql . t)
           (ruby . t)
           (lua . t)
           (R . t)
           (org . t)
           (screen . t)
           (shell . t)
           (plantuml . t)
           (C . t)
           (js . t)))
      (setq org-babel-do-load-languages-p t))
    (funcall orig-fun arg info params))

  (defun sl/org-babel-execute-ruby (&optional orig-fun body params)
    (ansi-color-apply (funcall orig-fun body params)))

  (advice-add 'org-babel-execute:ruby :around 'sl/org-babel-execute-ruby)
  (advice-add 'org-babel-execute-src-block :around 'sl/org-babel-execute-src-block)

  (setq org-capture-templates
    '(
       ("v"
         "Vocabulary"
         entry
         (file "~/org-modes/flashcards.org")
         "* %i%^{prompt} :drill: \n\nTranslate\n\n** Answer\n\n[[file:~/org-modes/images/%\\1.png]]\n\n** Image Source\n\n#+begin_src shell\ntest -f ~/org-modes/images/%\\1.png || wget -O ~/org-modes/images/%\\1.png \"%\\1%?\"\n#+end_src")
       ("s"
         "Speaking English"
         entry
         (file "~/org-modes/flashcards.org")
         "* %i%^{sentence} :drill:speaking:\n:PROPERTIES:\n:DRILL_SOUND: ~/org-modes/english/%^{sound}\n:END:\n\nSpeaking loudly man\n\n** Answer\n\n[[file:~/org-modes/english/%\\2]]\n\n** Source\n\n#+begin_src shell\ntest -f ~/org-modes/english/%\\2 || wget -O ~/org-modes/english/%\\2 \"%^{link}\"\n#+end_src")
       ("a"
         "Appointment"
         entry
         (file+headline "~/org-modes/personal.org" "Appointments")
         "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n"
         )
       ("l"
         "Learn"
         entry
         (file "~/org-modes/learn.org")
         "* %? :drill:\n"
         )
       ("p"
         "Personal"
         entry
         (file "~/org-modes/personal.org")
         "* %i\n\nCaptured On:%U\n\n")
       ("t"
         "Token"
         plain
         (file "/tmp/token.org")
         "%:initial"
         :immediate-finish
         :prepend)
       ("L"
         "Notes"
         entry
         (file "~/org-modes/notes.org")
         "* %:description\n\nSource: %:link\nCaptured On:%U\n\n%:initial\n\n"
         :immediate-finish
         :prepend)
       ("z"
         "Capture Notes"
         entry
         (file "~/org-modes/notes.org")
         "* %i\n\nCaptured On:%U\n\n%c")
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

  ;; org-drill
  (setq org-drill-maximum-items-per-session 40)
  (setq org-drill-maximum-duration 30)   ; 30 minutes
  (setq org-drill-learn-fraction 0.1)
  (setq org-drill-spaced-repetition-algorithm 'sm2)
  (setq org-plantuml-jar-path plantuml-jar-path))

;; a hack to fix org-version
(defun org-release ()
  "The release version of Org.
Inserted by installing Org mode or when a release is made."
  (let ((org-release "9.2.5"))
    org-release))

(defun org-git-version ()
  "The Git version of Org mode.
Inserted by installing Org or when a release is made."
  (let ((org-git-version "release_9.2.5-431-gf5d27e046"))
    org-git-version))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
    (lambda ()
      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (setq evil-org-use-additional-insert t
    evil-org-key-theme `(textobjects
                          navigation
                          additional
                          calendar
                          todo)
    org-image-actual-width 600
    org-babel-temporary-directory "/tmp")
  (evil-org-agenda-set-keys))

(defun org-babel-execute:tmux (body params)
  "Send a block of code via tmux to a terminal using Babel.
\"default\" session is used when none is specified.
Argument BODY the body of the tmux code block.
Argument PARAMS the org parameters of the code block."
  (message "Sending source code block to interactive terminal session...")
  (save-window-excursion
    (let* ((org-session (cdr (assq :session params)))
            (terminal (cdr (assq :terminal params)))
            (socket (cdr (assq :socket params)))
            (socket (when socket (expand-file-name socket)))
            (ob-session (ob-tmux--from-org-session org-session socket))
            (log-file (expand-file-name (concat "~/tmux." org-session ".log")))
            (log-file-before (expand-file-name (concat "/tmp/tmux." org-session ".log.before")))
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
      (message "Store log file")
      (copy-file log-file log-file-before t)
      (ob-tmux--send-body
        ob-session (org-babel-expand-body:generic body params)))))

(use-package org-tree-slide
  :commands (org-tree-slide-mode org-tree-slide-skip-done-toggle)
  :config (org-tree-slide-simple-profile))

(use-package org-pomodoro
  :after org
  :commands org-pomodoro
  :config
  (add-hook 'org-pomodoro-finished-hook #'open-mindfulness-buffer))

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

(defhydra hydra-org-babel
  (:color pink :hint nil)
  ("n" org-babel-next-src-block "next src block" :column "Move")
  ("p" org-babel-previous-src-block "prev src block")
  ("o" org-babel-open-src-block-result)
  ("g" org-babel-goto-named-src-block)
  ("r" org-babel-goto-named-result)
  ("I" org-babel-view-src-block-info)
  ("s" org-split-block "split src block" :column "Actions")
  ("k" org-babel-remove-result-one-or-many "remove result")
  ("a" org-babel-sha1-hash)
  ("b" org-babel-execute-buffer)
  ("c" org-babel-check-src-block)
  ("d" org-babel-demarcate-block)
  ("e" org-babel-execute-maybe)
  ("f" org-babel-tangle-file)
  ("i" org-babel-lob-ingest)
  ("j" org-babel-insert-header-arg)
  ("l" org-babel-load-in-session)
  ("S" org-babel-execute-subtree)
  ("t" org-babel-tangle)
  ("q" nil "cancel" :color blue :column nil))

(use-package ob-session-async
  :straight (ob-session-async :type git :host github :repo "jackkamm/ob-session-async")
  :config (require 'ob-session-async-ruby))
