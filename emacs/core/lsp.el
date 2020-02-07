(use-package lsp-mode
  ;; :hook (prog-major-mode . lsp-prog-major-mode-enable)
  ;; :hook (ruby-mode . lsp-ruby-mode-enable)
  ;; :hook ((js-mode . lsp-deferred)
  ;;        (ruby-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  ;; :init (add-hook 'ruby-mode #'lsp-ruby-mode-enable)
  ;; :init (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-prefer-flymake nil
  	lsp-auto-guess-root t
  	lsp-keep-workspace-alive nil)
  ;; (ht-set! lsp--default-notification-handlers "client/registerCapability" 'ignore)

  (setq lsp-response-timeout 5)
  ;; (setq lsp-print-io t)
  ;; (setq lsp-eldoc-render-all t)

  ;; ruby
  ;; (setq ruby-language-server-path (format "%s/ruby/bin/solargraph" user-emacs-directory))
  ;; (defun lsp-clients-ruby--make-init-options ()
  ;;   "Init options for Ruby. (syntax checking enabled)"
  ;;   '(:diagnostics t))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection
  ;; 				     (list ruby-language-server-path "stdio")
  ;; 				     )
  ;; 		    :major-modes '(ruby-mode)
  ;; 		    :initialization-options #'lsp-clients-ruby--make-init-options
  ;; 		    :priority -1
  ;; 		    :server-id 'm-ruby-ls))
  )

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
;; (use-package company-lsp
;;   ;; :after (lsp-mode company)
;;   :commands company-lsp
;;   :config
;;   (push 'company-lsp company-backends)
;;   (setq company-lsp-async t)
;;   (setq company-lsp-cache-candidates 'auto)
;;   (add-hook 'ruby-mode-hook (lambda ()
;; 			      (add-to-list 'company-lsp-filter-candidates '(m-ruby-ls . nil))
;; 			      (if (get-buffer-process "*m-ruby-ls*") (lsp))))
;;   )

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   ;; :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-sideline-show-hover nil)
;;   (setq lsp-ui-doc-enable nil)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix slow LSP flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar lsp-ui-flycheck--stale-diagnostics nil)

;; (defun lsp-ui-flycheck-enable (_)
;;   "Enable flycheck integration for the current buffer."
;;   (setq-local flycheck-check-syntax-automatically nil)
;;   (setq-local flycheck-checker 'lsp-ui)
;;   (lsp-ui-flycheck-add-mode major-mode)
;;   (add-to-list 'flycheck-checkers 'lsp-ui)
;;   (run-with-idle-timer 0.2 t
;; 		       (lambda () (when (and lsp-ui-flycheck--stale-diagnostics flycheck-mode)
;; 				    (flycheck-buffer)
;; 				    (setq lsp-ui-flycheck--stale-diagnostics nil))))
;;   (add-hook 'lsp-after-diagnostics-hook (lambda ()
;; 					  (setq lsp-ui-flycheck--stale-diagnostics t)
;; 					  )))

(general-define-key
 :keymaps 'lsp-ui-imenu-mode-map
 :states 'normal
 "q" #'lsp-ui-imenu--kill)
