(use-package! chatgpt
  :defer t
  :bind ("C-c q" . chatgpt-query)
  :config
  (setq chatgpt-repo-path (expand-file-name "straight/repos/ChatGPT.el/" doom-local-dir))
  (setq chatgpt-query-format-string-map
    '(("doc" . "Please write the documentation for the following function.\n\n%s")
       ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
       ("understand" . "What does the following function do?\n\n%s")
       ("improve" . "Please improve the following code.\n\n%s")
       ;; your new prompt
       ("question" . "%s")
       ("translate" . "Translate to Vietnamese\n\n%s")
       ("correct grammar" . "Correct grammar for the following text.\n\n%s")
       ("explain grammar" . "Explain grammar used in the following text.\n\n%s")
       ("paraphase" . "Write it in another way.\n\n%s")))
  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'bottom :size .5 :ttl nil :quit t :modeline nil))

(use-package! ob-chatgpt
  :after '(org chatgpt))
