(require 'dash)

(defvar ob-tmux-delimiters '((ruby . "#####") (sh . "#####")))
(defconst ob-tmux-async-file load-file-name)

(defun ob-tmux-get-delimiters (lang jid)
  (let* ((lang-with-default (or lang "sh"))
          (delimiter (cdr (assq (intern lang-with-default) ob-tmux-delimiters)))
          (start-delimiter (format "%s start:%s" delimiter jid))
          (finish-delimiter (format "%s finish:%s" delimiter jid)))
    `(,start-delimiter ,finish-delimiter)))

(defun ob-tmux--generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (random 100000000))))

(defun ob-tmux-view-at-point (&optional arg)
  (interactive "P")
  (let* ((info (org-babel-get-src-block-info 'light))
          (params (nth 2 info))
          (org-session (cdr (assq :session params)))
          (file (cdr (assq :file params)))
          (socket (cdr (assq :socket params)))
          (socket (when socket (expand-file-name socket)))
          (ob-session (ob-tmux--from-org-session org-session socket))
          (session-alive (ob-tmux--session-alive-p ob-session))
          (window-alive (ob-tmux--window-alive-p ob-session))
          (buffer (get-buffer-create "*ob-tmux-view*"))
          (raw-output (ob-tmux--execute-string ob-session
                        "capture-pane"
                        "-J"
                        "-p" ;; print to stdout
                        "-S" "-" ;; start at beginning of history
                        "-t" (ob-tmux--target ob-session))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert raw-output)
      (goto-char (point-max)))
    (switch-to-buffer-other-window buffer)))

(defun ob-tmux-format-code:ruby (jid body)
  (let* ((delimiters (ob-tmux-get-delimiters "ruby" jid))
          (start-delimiter (car delimiters))
          (finish-delimiter (car (cdr delimiters))))
    (format "puts \'%s\'\n %s\n\nputs \'%s\'" start-delimiter body finish-delimiter)))

(defun ob-tmux-parse-output:ruby (raw-output jid body)
  (let* ((delimiters (ob-tmux-get-delimiters "ruby" jid))
          (start-delimiter (car delimiters))
          (finish-delimiter (car (cdr delimiters)))
          (after-start-delimiter (->> raw-output (s-split start-delimiter) -last-item))
          (after-finish-delimiter (->> raw-output (s-split finish-delimiter) -last-item)))
    (->> (s-replace after-finish-delimiter "" after-start-delimiter)
      (s-replace start-delimiter "")
      ;; (s-replace ,finish-delimiter "")
      (s-replace-regexp (format "^.*%s.*$" finish-delimiter) "")
      (s-replace-regexp "=> .*" "")
      (s-replace-regexp "irb\([a-z]+\):[0-9]+:[0-9]+.*" "")
      (s-replace-regexp "^\[[0-9]+\] .* pry\(.*\).*" "")
      (s-replace-regexp "^>> .*" "")
      (s-replace-regexp "^nil." "")
      (s-replace-regexp "[\n]+" "\n")
      s-trim
      (s-split "\n")
      (-map #'s-trim-right)
      (--remove (s-contains? it body))
      (s-join "\n"))))

(defun ob-tmux-job-finish:ruby (jid ob-session)
  (let* ((delimiters (ob-tmux-get-delimiters "ruby" jid))
          (finish-delimiter (car (cdr delimiters)))
          (raw-output (ob-tmux--execute-string ob-session
                        "capture-pane"
                        "-J"
                        "-p" ;; print to stdout
                        "-S" "-" ;; start at beginning of history
                        "-t" (ob-tmux--target ob-session))))
    (string-match (concat "^" finish-delimiter) raw-output)))

(defun ob-tmux-format-code:sh (jid body)
  (let* ((delimiters (ob-tmux-get-delimiters "sh" jid))
          (start-delimiter (car delimiters))
          (finish-delimiter (car (cdr delimiters))))
    (format "echo \'%s\'; %s;\necho \'%s\';" start-delimiter
      (->> body
        (s-replace-regexp "[\\]\s*\n\s*" " ")
        (s-replace-regexp "[\n\r]+" "; "))
      finish-delimiter)))

(defun ob-tmux-format-code (lang jid body)
  (let* ((delimiters (ob-tmux-get-delimiters "sh" jid))
          (start-delimiter (car delimiters))
          (finish-delimiter (car (cdr delimiters)))
          (formatter (intern (concat "ob-tmux-format-code:" lang))))
    (if (fboundp formatter)
      (funcall formatter jid body)
      (format "%s\n%s\n%s" start-delimiter body finish-delimiter))))

(defun ob-tmux-parse-output:sh (raw-output jid body)
  (let* ((delimiters (ob-tmux-get-delimiters "sh" jid))
          (start-delimiter (car delimiters))
          (finish-delimiter (car (cdr delimiters)))
          (after-start-delimiter (->> raw-output (s-split start-delimiter) -last-item))
          (after-finish-delimiter (->> raw-output (s-split finish-delimiter) -last-item)))
    (->> (s-replace after-finish-delimiter "" after-start-delimiter)
      (s-replace start-delimiter "")
      ;; (s-replace ,finish-delimiter "")
      (s-replace-regexp (format "^.*%s.*$" finish-delimiter) "")
      ;; (s-split "\n$ .*\n")
      ;; (-map (lambda (cmd)
      ;;         (->> cmd
      ;;           (s-replace-regexp "[\n]+" "\n"))))
      ;; (s-join "\n")
      ;; (s-replace-regexp "^$ .*" "")
      s-trim
      (s-split "\n")
      (-map #'s-trim-right)
      (-drop-last 1)
      (--remove (s-contains? it body))
      (s-join "\n"))))

(defun ob-tmux-job-finish:sh (jid ob-session)
  (let* ((delimiters (ob-tmux-get-delimiters "sh" jid))
          (finish-delimiter (car (cdr delimiters)))
          (raw-output (ob-tmux--execute-string ob-session
                        "capture-pane"
                        "-J"
                        "-p" ;; print to stdout
                        "-S" "-" ;; start at beginning of history
                        "-t" (ob-tmux--target ob-session))))
    (string-match (concat "^" finish-delimiter) raw-output)))

(defun ob-tmux-parse-output (lang jid ob-session body)
  (let* ((raw-output (ob-tmux--execute-string ob-session
                       "capture-pane"
                       "-J"
                       "-p" ;; print to stdout
                       "-S" "-" ;; start at beginning of history
                       "-t" (ob-tmux--target ob-session)))
          (parser (intern (concat "ob-tmux-parse-output:" lang))))
    (let ((job-finish (intern (concat "ob-tmux-job-finish:" lang))))
      (while (not (funcall job-finish jid ob-session))
        (sleep-for 0.1)))
    (if (fboundp parser) (funcall parser raw-output jid body) raw-output)))

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
            (jid (ob-tmux--generate-uuid))
            ;; (lang-with-default (or lang "sh"))
            ;; (delimiter (cdr (assq (intern lang-with-default) ob-tmux-delimiters)))
            ;; (start-delimiter (format "%s start:%s" delimiter jid))
            ;; (finish-delimiter (format "%s finish:%s" delimiter jid))
            (delimiters (ob-tmux-get-delimiters lang jid))
            (start-delimiter (car delimiters))
            (finish-delimiter (car (cdr delimiters)))
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
        (org-babel-expand-body:generic (ob-tmux-format-code lang jid body) params))
      (org-babel-insert-result jid '("replace"))
      (async-start
        `(lambda ()
           (setq exec-path ',exec-path)
           (setq load-path ',load-path)
           (setq lang (or ,lang "sh"))

           (package-initialize)
           (org-babel-do-load-languages 'org-babel-load-languages ',org-babel-load-languages)
           (load-file ,ob-tmux-async-file)

           (ob-tmux-parse-output lang ,jid ,ob-session ,body))
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
                   (org-babel-insert-result file '("file" "replace")))))))))))
