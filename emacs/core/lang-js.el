(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
          (web-mode . prettier-js-mode)))

(use-package import-js :defer t)

;; prevent eslint check command: eslint --print-config .
(advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

(use-package emmet-mode
  :defer t
  :hook (rjsx-mode . emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-expand-jsx-className? t))
