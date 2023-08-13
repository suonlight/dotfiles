(use-package! bard
  :defer t
  :commands (bard-chat bard-chat-with-multiline bard-chat-with-message)
  :config

  ;; override bard-response to insert the response with org format in the current buffer
  (defun bard-response (serial-number content buffer)
    (let ((formatted-content (sl/markdown-to-org content)))
      (if (equal serial-number 1)
        (progn
          (setq bard-drafts (list))
          (push formatted-content bard-drafts)
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-max))
              (insert "\n### Bard:\n")
              (setq bard-draft--begin (point-max))
              (insert formatted-content)
              (setq bard-draft--end (point-max)))))
        (push formatted-content bard-drafts))))

  (setq bard-http-proxy "")
  (map! :leader "aa" #'bard-chat))

(use-package! aichat
  :commands (aichat-read-region-or-input aichat-bingai-conversation aichat-bingai-chat)
  :config
  ;; (setq aichat-bingai-cookies-file (format "%s/.config/bing.cookies.json" (getenv "HOME")))
  (setq aichat-http-backend 'url)
  (map! :leader "ai" #'aichat-bingai-chat))

(defun bing-chat-with-message (text &optional style)
  "Send the region or input to Bing and replace the selected region or insert at the current position with the returned result."
  (interactive (list (aichat-read-region-or-input "Input text: ")))
  (when (and text (not (string-empty-p text)))
    (let* ((cur-buf (current-buffer))
           (cur-pos (with-current-buffer cur-buf (point)))
           (reg-beg (when (use-region-p) (region-beginning)))
           (reg-end (when (use-region-p) (region-end))))
      (aichat-bingai-conversation text
                                  :style style
                                  :on-success (lambda (msg)
                                                (when-let ((content (aichat-bingai-message-type-2-text msg)))
                                                  (save-excursion
                                                    (goto-char (point-max))
                                                    (insert "\n## User:\n")
                                                    (insert (cdr (assq 'text (aichat-json-access msg "{item}{messages}[0]"))))
                                                    (insert "\n\n## Bing AI:\n")
                                                    (insert (format "%s\n" content)))
                                                  (message "Success: %s" content)))
                                  :on-error (lambda (err)
                                              (message "Error: %s" err))))))

;; write a method to firstly choose AI prompts: fix grammar, explain code, custom prompt
;; then ask to choose which AI: bard-chat or aichat-bingai-chat to use
;; then passing the chosen prompt to chose AI to chat
(defun ai-chat ()
  (interactive)
  (let* ((prompt (read-string "Prompt: "))
         (ai (completing-read "AI: " '("Google Bard" "Bing AI"))))
    (cond ((equal ai "Google Bard")
           (bard-chat-with-message prompt))
          ((equal ai "Bing AI")
           (bing-chat-with-message prompt)))))

(map! :leader "aq" #'ai-chat)
