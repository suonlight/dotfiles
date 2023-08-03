(use-package! bard
  :defer t
  :commands (bard-chat bard-chat-with-multiline)
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

  (setq bard-http-proxy ""))

(use-package! aichat
  :config
  ;; (setq aichat-bingai-cookies-file (format "%s/.config/bing.cookies.json" (getenv "HOME")))
  (setq aichat-http-backend 'url))
