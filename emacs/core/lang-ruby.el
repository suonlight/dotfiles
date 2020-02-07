(use-package ruby-test-mode
  :hook (ruby-mode . ruby-test-mode)
  :config
  (defun ruby-test-spec-command (filename &optional line-number)
  "Return command to run spec in FILENAME at LINE-NUMBER."
  (let ((command "bundle exec rspec")
        (options ruby-test-rspec-options)
        (filename (if line-number
                      (format "%s:%s" filename line-number)
                    filename)))
    (format "%s %s %s" command (mapconcat 'identity options " ") filename))))

(use-package ruby-end
  :hook (ruby-mode . ruby-end-mode)
  :custom
  (ruby-end-insert-newline nil))

(use-package inf-ruby
  :after ruby-mode
  :hook (ruby-mode . inf-ruby-minor-mode))

;; (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
;; (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(use-package rubocop
  :after ruby-mode
  :hook (ruby-mode . rubocop-mode))

(setq inf-ruby-console-environment "development")
