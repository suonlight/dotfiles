(require 'uuidgen)
(require 'chatgpt)
(require 'python)

(setq chatgpt-repo-path "~/.config/emacs/.local/straight/repos/ChatGPT.el/")

(defconst ob-chatgpt-file load-file-name)

(defvar org-babel-default-header-args:chatgpt '((:wrap . "example"))
  "Default header arguments for forth code blocks.")

(defun org-babel-execute:chatgpt (body params)
  "Execute a block of ChatGPT code with org-babel.
          This function is called by `org-babel` when it needs to execute a code block in the ChatGPT language.

          BODY is the code block to be executed.
          PARAMS is an alist of parameters passed to the code block."
  (let* ((result-params (cdr (assq :result-params params)))
         (type (cdr (assq :type params)))
         (info (org-babel-get-src-block-info))
         (jid (uuidgen-4))
          (query body))
    (if (string= 'api type)
        (chatgpt-api--query-open-api body
                                     `(lambda (results)
                                        (with-current-buffer ,(current-buffer)
                                          (save-excursion
                                            (let ((info (org-babel-get-src-block-info)))
                                              (goto-char (point-min))
                                              (search-forward ,jid)
                                              (search-backward "src")
                                              (org-babel-insert-result
                                               (decode-coding-string (encode-coding-string results 'iso-8859-1) 'utf-8)
                                               ',result-params info))
                                            ))))
      (unless chatgpt-process
        (chatgpt-init))
      (deferred:$
        (deferred:$
          (epc:call-deferred chatgpt-process 'query (list query))
          (eval `(deferred:nextc it
                   (lambda (results)
                     (with-current-buffer ,(current-buffer)
                       (save-excursion
                         (goto-char (point-min))
                         (search-forward ,jid)
                         (search-backward "src")
                         (org-babel-insert-result results ',result-params ',info)))))))
        (eval
          `(deferred:error it
             (lambda (err)
               (string-match "\"Error('\\(.*\\)')\"" (error-message-string err))
               (let ((error-str (match-string 1 (error-message-string err))))
                 (when (yes-or-no-p (format "Error encountered. Reset chatgpt (If reset doesn't work, try \"\"pkill ms-playwright/firefox\"\" in the shell then reset)?" error-str))
                   (chatgpt-reset))))))))
    jid))

(provide 'ob-chatgpt)
