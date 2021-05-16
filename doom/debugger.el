(after! dap-mode
  (defun file-path ()
    "Retrieve the file path of the current buffer.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
    (when-let (file-path (buffer-file-name))
      (file-truename file-path)))

  (defun file-path-with-line ()
    "Retrieve the file path of the current buffer, including line number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
    (when-let (file-path (file-path))
      (concat file-path ":" (number-to-string (line-number-at-pos)))))

  (defun file-path-with-line-column ()
    "Retrieve the file path of the current buffer, including line and column number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
    (when-let (file-path (file-path-with-line))
      (concat
        file-path
        ":"
        (number-to-string (if (and
                                ;; Emacs 26 introduced this variable.
                                ;; Remove this check once 26 becomes the minimum version.
                                (boundp column-number-indicator-zero-based)
                                (not column-number-indicator-zero-based))
                            (1+ (current-column))
                            (current-column))))))

  (defun copy-file-path-with-line ()
    "Copy and show the file path of the current buffer, including line number."
    (interactive)
    (if-let (file-path (file-path-with-line))
      (message "%s" (kill-new file-path))
      (message "WARNING: Current buffer is not attached to a file!")))

  (defun hero/ruby-version ()
    (let ((version-file-path (f-join (projectile-project-root) ".ruby-version")))
      (s-replace "ruby-" ""
        (-> version-file-path
          f-read
          s-trim))))

  (defun hero/ruby-path-for (program)
    (expand-file-name
      (format "~/.asdf/installs/ruby/%s/bin/%s" (hero/ruby-version) program)))

  (defun hero/rspec-program ()
    (if (f-exists? (f-join (projectile-project-root) "bin/rspec"))
      (f-join (projectile-project-root) "bin/rspec")
      (hero/ruby-path-for "rspec")))

  (defun dap-ruby--populate-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
      (dap--put-if-absent :dap-server-path dap-ruby-debug-program)
      (dap--put-if-absent :type "Ruby")
      (dap--put-if-absent :debuggerPort 1234)
      (dap--put-if-absent :cwd (projectile-project-root))
      (dap--put-if-absent :program (buffer-file-name))
      (dap--put-if-absent :name "Ruby Debug")))

  (defun dap-ruby-run-test-at-point ()
    "Run Ruby test at point"
    (interactive)
    (let ((debug-args (list :type "Ruby"
                        :request "launch"
                        ;; :program (rbenv-rspec-program)
                        :program (hero/rspec-program)
                        :args `(,(copy-file-path-with-line))
                        :environment-variables '(("DISABLE_SPRING" . "true"))
                        :name "Rspec File At Point")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
                             (gethash dap--debug-providers)
                             (funcall debug-args)))))

  (defun dap-ruby-run-test ()
    "Run Ruby test file"
    (interactive)
    (let ((debug-args (list :type "Ruby"
                        :request "launch"
                        :program (hero/rspec-program)
                        :environment-variables '(("DISABLE_SPRING" . "true"))
                        :args `(,buffer-file-name)
                        :name "Rspec File")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
                             (gethash dap--debug-providers)
                             (funcall debug-args)))))

  (dap-register-debug-provider "Ruby" 'dap-ruby--populate-start-file-args))
