;; Company mode
(use-package company
  :init
  (setq company-minimum-prefix-length 3)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0)
  (setq company-require-match 'never)
  ;; (setq company-frontends
	;; '(company-pseudo-tooltip-unless-just-one-frontend
	;;   company-preview-frontend
	;;   company-echo-metadata-frontend))
  (setq tab-always-indent 'complete)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  ;; (setq company-backends ())
  ;; (defvar completion-at-point-functions-saved nil)
  :hook (after-init . global-company-mode)
  :config
  ;; (push 'fuzzy completion-styles)
  ;; (setq completion-styles '(fuzzy company-flx-try-completion company-flx-all-completions "An intelligent fuzzy matching completion style."))

  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (interactive)
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))
  ;; (global-company-mode 1)

  ;; (push 'fuzzy completion-styles)
  ;; (company-flx-mode +1)
  (setq completion-styles '(partial-completion basic emacs22))
  (company-mode t)
  (setq company-backends '((company-capf company-abbrev company-dabbrev-code company-files company-etags company-keywords company-yasnippet)
                            company-dabbrev))

  (add-hook 'ruby-mode-hook
    (lambda ()
      (set (make-local-variable 'company-backends)
        '((company-capf company-abbrev company-dabbrev-code company-files company-etags company-keywords company-yasnippet)))))

  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (push 'company-elisp company-backends)))

  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "C-<return>") 'do-yas-expand)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-mode-map [remap indent-for-tab-command] 'tab-indent-or-complete))

(use-package company-statistics
  :after company
  :hook (company-mode . company-statistics-mode))

;; (use-package company-flx
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'company
;;     (company-flx-mode +1)))
