(eval-when-compile
  (defmacro embark-split (fn)
    `(defun ,(intern (concat "embark-"
                       (symbol-name fn)
                       "-"
                       "split")) ()
       (interactive)
       (evil-window-split)
       (windmove-down)
       (call-interactively #',fn)))
  (defmacro embark-vsplit (fn)
    `(defun ,(intern (concat "embark-"
                       (symbol-name fn)
                       "-"
                       "vsplit")) ()
       (interactive)
       (evil-window-vsplit)
       (windmove-right)
       (call-interactively #',fn))))

(after! embark
  (define-key embark-buffer-map   (kbd "C-v") (embark-vsplit switch-to-buffer))
  (define-key embark-file-map     (kbd "C-v") (embark-vsplit projectile-find-file))
  (define-key embark-bookmark-map (kbd "C-v") (embark-vsplit bookmark-jump))

  (define-key embark-buffer-map   (kbd "C-s") (embark-split switch-to-buffer))
  (define-key embark-file-map     (kbd "C-s") (embark-split projectile-find-file)))
