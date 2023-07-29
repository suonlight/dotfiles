(use-package! bard
  :defer t
  :commands (bard-chat bard-chat-with-multiline)
  :config
  (setq bard-http-proxy ""))

(use-package! aichat
  :config
  ;; (setq aichat-bingai-cookies-file (format "%s/.config/bing.cookies.json" (getenv "HOME")))
  (setq aichat-http-backend 'url))
