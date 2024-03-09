(require 'epc)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((astmux . t)))

(setq org-babel-default-header-args:astmux
    '((:results . "none")
       (:session . "default")
       (:socket  . nil)))

(defconst astmux-server-path
  (expand-file-name "astmux_server.py" (file-name-directory load-file-name))
  "Path to the astmux server script.")

(defcustom astmux-session-prefix "ob-"
  "The default prefix for tmux sessions."
  :group 'astmux
  :type 'string)

(defun astmux--generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (random 100000000))))

;; Customize language
(defun astmux-get-lang (current-lang)
  (let* ((info (org-babel-get-src-block-info))
          (params (nth 2 info))
          (custom-lang (cdr (assq :lang params))))

    (cond ((string= current-lang "tmux")
            custom-lang)
      (t current-lang))))

(add-to-list 'org-src-lang-modes '("tmux" . sh))

(defun org-src---get-lang-mode (orig-fun &rest args)
  (let* ((lang (astmux-get-lang (car args))))
    (apply orig-fun (list lang))))

(advice-add 'org-src-get-lang-mode :around #'org-src---get-lang-mode)

(defun org-babel-execute:astmux (body params)
  "Execute the astmux commands specified in BODY asynchronously using EPC."
  (let* ((session (cdr (assoc :session params)))
         (session (concat astmux-session-prefix session))
         (socket (or (cdr (assoc :socket params)) "/tmp/tmux-501/default"))
         (lang (cdr (assoc :lang params)))
         (results (or (cdr (assq :results params))))
         (file (cdr (assq :file params)))
         (jid (astmux--generate-uuid))
         (connection (epc:start-epc "python3" `(,astmux-server-path))))
    (deferred:$
      (epc:call-deferred connection 'execute_astmux `(,socket ,session ,lang ,jid ,body))
      (deferred:nextc it
        `(lambda (response)
           (with-current-buffer ,(current-buffer)
             (save-excursion
               (let* ((result (plist-get response :message))
                       (file ,file))
                 (goto-char (point-min))
                 (search-forward ,jid)
                 (search-backward "src")
                 (if (not (eq file 'nil))
                   (progn
                     (write-region result nil file)
                     (org-babel-insert-result file '("file" "replace")))
                   (cond
                     ((s-contains? "silent" ,results)
                       (org-babel-remove-result)
                       (message result))
                     (t (org-babel-insert-result result '("replace")))))))))))
    (org-babel-insert-result jid '("replace"))))
