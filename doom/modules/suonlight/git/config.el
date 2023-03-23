(use-package! magit-circleci
  ;; :after 'magit
  :config
  ;; (setq magit-circleci-organisation-name (getenv "MAGIT_CIRCLECI_ORGANISATION_NAME"))
  (setq magit-circleci-token (getenv "MAGIT_CIRCLECI_TOKEN"))
  (setq magit-circleci--project-slug  (getenv "MAGIT_CIRCLECI_ORGANISATION_NAME")))
