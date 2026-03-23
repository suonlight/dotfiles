(load! "~/projects/faa/bot/emacs-remote-server.el")

(defun agent-shell-attention-notify-telegram (buffer title message)
  "Send notification to Telegram using bot API.
BUFFER is the source buffer, TITLE and MESSAGE are notification info.
Will extract content from BUFFER starting from the last 'Thought process' to end."
  (let* ((telegram-bot-token (getenv "TELEGRAM_BOT_TOKEN"))
         (telegram-chat-id (getenv "TELEGRAM_ALLOWED_CHAT_ID"))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (content (with-current-buffer buffer
                    (save-excursion
                      (goto-char (point-max))
                      (if (re-search-backward "Thought process" nil t)
                          (let ((extracted (buffer-substring-no-properties (point) (point-max))))
                            ;; Remove "Thought process" text from the beginning of extracted content
                            (replace-regexp-in-string "^Thought process" "" extracted))
                        (buffer-substring-no-properties (point-min) (point-max))))))
         ;; Properly format the body data
         (url-request-data (format "chat_id=%s&text=%s&parse_mode=HTML"
                                   (url-hexify-string telegram-chat-id)
                                   (url-hexify-string (format "🔔 %s\n\n%s" title content)))))
    (when (and telegram-bot-token telegram-chat-id)
      (url-retrieve
       (format "https://api.telegram.org/bot%s/sendMessage" telegram-bot-token)
       (lambda (status)
         (if (plist-get status :error)
             (message "Telegram notification failed: %S" (plist-get status :error))
           (message "Telegram notification sent successfully!")))))))

(use-package! acp
  :config
  (require 'agent-shell)
  (require 'agent-shell-attention)
  ;; (setq agent-shell-attention-mode t)
  (add-hook! agent-shell-mode #'agent-shell-attention-mode)
  (setq! agent-shell-attention-notify-function
    (lambda (buffer title message)
      (agent-shell-attention-notify-telegram buffer title message)
      (agent-shell-attention-notify-default buffer title message)))
  ;; (setq agent-shell-google-authentication
  ;;   (agent-shell-google-make-authentication :login t))
  (setq agent-shell-google-authentication
    (agent-shell-google-make-authentication :api-key (getenv "GEMINI_API_KEY")))
  (setq agent-shell-auggie-authentication
      (agent-shell-make-auggie-authentication :login t))
  (setq agent-shell-mistral-authentication
    (agent-shell-mistral-make-authentication :api-key (getenv "MISTRAL_API_KEY")))
  (setq agent-shell-goose-authentication
    (agent-shell-make-goose-authentication :openai-api-key (getenv "OPENROUTER_API_KEY")))
  (setq agent-shell-anthropic-claude-environment
    (agent-shell-make-environment-variables
      ;; "ANTHROPIC_BASE_URL" "http://localhost:11434"
      ;; "ANTHROPIC_AUTH_TOKEN" "ollama"
      ;; "ANTHROPIC_MODEL" "crow-heretic:latest"
      ;; https://bailian.console.alibabacloud.com/cn-beijing/?tab=model&accounttraceid=390346ff833e4f0f914694eb78765c94dzzo#/model-market/detail/qwen3-coder-next
      "ANTHROPIC_BASE_URL" "https://dashscope.aliyuncs.com/compatible-mode/v1"
      "ANTHROPIC_API_KEY" (getenv "DASHSCOPE_API_KEY")
      "ANTHROPIC_MODEL" "qwen3.5-plus"))

  ;; https://dashscope.aliyuncs.com/compatible-mode/v1
  (add-to-list 'agent-shell-agent-configs
    '(ollama-crow
       :command ("ollama" "run" "crow-heretic")
       :header-line "Ollama: Crow-9B Heretic"))

  (emacs-remote-server-start))
