(after! company
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0.3))

(setq +lsp-company-backends '(:separate company-files company-capf company-yasnippet company-dabbrev-code))
(setq lsp-use-plists nil)

(setq lsp-idle-delay 0.300)
(setq lsp-completion-provider :capf)
(setq lsp-ui-doc-mode t)
(setq lsp-ui-doc-enable t)
(setq lsp-auto-guess-root nil)
;; (setq lsp-enable-symbol-highlighting nil)
(setq lsp-response-timeout 20)
(setq lsp-enable-links nil)

;; not work
(setq lsp-ui-sideline-show-code-actions nil)
;; (setq lsp-completion-enable nil)

(setq rustic-lsp-server 'rust-analyzer)

(after! lsp-mode
  ;; configurations https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;; speed up lsp http://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html
  ;; need to compile t. But it's not stable now
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "~/.config/doom/assets/rls-macos/reason-language-server")
      :major-modes '(reason-mode)
      :notification-handlers (ht ("client/registerCapability" 'ignore))
      :priority 1
      :server-id 'reason-ls)))

(after! sh (add-hook! sh-mode #'lsp-mode))

;; enable lsp-grammarly
(defun register-lsp-grammarly ()
  (require 'lsp-grammarly)
  (lsp))

(setq grammarly-username "minh.nh1989@gmail.com")
(add-hook! org-mode #'register-lsp-grammarly)
(add-hook! markdown-mode #'register-lsp-grammarly)

;; enable lsp-ltex
(setq lsp-ltex-version "15.2.0")  ; make sure you have set this, see below
(setq lsp-ltex-user-rules-path (expand-file-name "~/org-modes/lsp-ltex"))

(defun register-lsp-ltex ()
  (require 'lsp-ltex)
  (lsp))

(add-hook! org-mode #'register-lsp-ltex)
(add-hook! markdown-mode #'register-lsp-ltex)
