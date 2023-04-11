(after! magit
  (map! (:map magit-mode-map
          :nv "gcp" #'magit-circleci-pull
          :nv "gcb" #'magit-circleci-browse-build
          :nv "gct" #'magit-circleci-mode)))

(use-package! magit-circleci
  :commands (magit-circleci-pull magit-circleci-browse-build magit-circleci-mode)
  :config
  (setq magit-circleci-token (getenv "MAGIT_CIRCLECI_TOKEN"))
  (setq magit-circleci--project-slug  (getenv "MAGIT_CIRCLECI_ORGANISATION_NAME")))
