(use-package! chatgpt
  :init
  (require 'python)
  (setq chatgpt-repo-path "~/.config/emacs/.local/straight/repos/ChatGPT.el/")
  :bind ("C-c q" . chatgpt-query)
  :config
  (setq chatgpt-query-format-string-map
    '(("doc" . "Please write the documentation for the following function.\n\n%s")
       ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
       ("understand" . "What does the following function do?\n\n%s")
       ("improve" . "Please improve the following code.\n\n%s")
       ;; your new prompt
       ("question" . "%s"))))
