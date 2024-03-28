(use-package! gemini
  :commands (gemini-chat)
  :config
  (setq gemini-api-token (getenv "GEMINI_TOKEN"))

  ; (defun bard-chat-with-message (prompt)
  ;   (message "[Bard] Please wait for Bard...")
  ;   (bard-call-async "bard_chat"
  ;     prompt
  ;     (buffer-name)))

  ; (defun bard-chat ()
  ;   (interactive)
  ;   (let ((prompt (read-string "Chat with Bard: ")))
  ;     (if (string-empty-p (string-trim prompt))
  ;       (message "Please do not enter an empty prompt.")
  ;       (save-excursion
  ;         (goto-char (point-max))
  ;         (insert "## User:\n")
  ;         (insert (format "%s\n" prompt)))
  ;       (bard-chat-with-message prompt))))

  ;; override bard-response to insert the response with org format in the current buffer
  ; (defun bard-response (serial-number content buffer)
  ;   (let ((formatted-content (sl/markdown-to-org content)))
  ;     (if (equal serial-number 1)
  ;       (progn
  ;         (setq bard-drafts (list))
  ;         (push formatted-content bard-drafts)
  ;         (with-current-buffer buffer
  ;           (save-excursion
  ;             (goto-char (point-max))
  ;             (insert "\n### Bard:\n")
  ;             (setq bard-draft--begin (point-max))
  ;             (insert formatted-content)
  ;             (setq bard-draft--end (point-max)))))
  ;       (push formatted-content bard-drafts))))

  ; (setq bard-http-proxy "")
  (map! :leader "aa" #'gemini-chat))

(use-package! aichat
  :commands (aichat-read-region-or-input aichat-bingai-conversation aichat-bingai-chat)
  :config
  ;; (setq aichat-bingai-cookies-file (format "%s/.config/bing.cookies.json" (getenv "HOME")))
  (setq aichat-http-backend 'url)
  (map! :leader "ai" #'aichat-bingai-chat))

(defun bing-chat-with-region (text &optional selected)
  "Send the region or input to Bing and replace the selected region or insert at the current position with the returned result."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (when (not selected)
      (insert "\n## User:\n")
      (insert (format "%s\n\n\n" text))))
  (message "[Bing AI] Please wait for Bing AI...")
  (aichat-bingai-conversation text
    :on-success (lambda (msg)
                  (when-let ((content (aichat-bingai-message-type-2-text msg)))
                    (save-excursion
                      (goto-char (point-max))
                      (insert "## Bing AI:\n")
                      (insert (format "%s\n" content)))
                    (message "Success: %s" content)))
    :on-error (lambda (err) (message "Error: %s" err))))

(defun bard-chat-with-region (prompt &optional selected)
  (save-excursion
    (when (not selected)
      (goto-char (point-max))
      (insert "## User:\n")
      (insert (format "%s\n" prompt))))
  (bard-chat-with-message prompt))

;; write a method to firstly choose AI prompts: fix grammar, explain code, custom prompt
(defun ai-chat ()
  (interactive)
  (let* ((selected (use-region-p))
          (prompt (read-string "Prompt: "
                    (if selected
                      (buffer-substring (region-beginning) (region-end)))))
          (ai (completing-read "AI: " '("Google Bard" "Bing AI"))))
    (when selected
      (deactivate-mark))
    (cond ((equal ai "Google Bard")
            (bard-chat-with-region prompt selected))
      ((equal ai "Bing AI")
        (bing-chat-with-region prompt selected)))))

; (map! :leader "aq" #'ai-chat)
