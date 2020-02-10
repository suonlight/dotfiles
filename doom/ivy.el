(after! ivy
  (defun counsel-file-vsplit (x)
    (let* ((file (if (and ivy--directory
                       (ivy--dirname-p (ivy-state-current ivy-last)))
                   (substring (ivy-state-current ivy-last) 0 -1)
                   (ivy-state-current ivy-last)))
            (absolute-file (if (projectile-project-p)
                             (expand-file-name file (projectile-project-root))
                             file)))
      (evil-window-vsplit)
      (windmove-right)
      (find-file absolute-file)))

  (defun counsel-file-split (x)
    (let* ((file (if (and ivy--directory
                       (ivy--dirname-p (ivy-state-current ivy-last)))
                   (substring (ivy-state-current ivy-last) 0 -1)
                   (ivy-state-current ivy-last)))
            (absolute-file (if (projectile-project-p)
                             (expand-file-name file (projectile-project-root))
                             file)))
      (evil-window-split)
      (windmove-down)
      (find-file absolute-file)))

  (ivy-add-actions
    'counsel-projectile-find-file
    '(("v" counsel-file-vsplit "open file in vsplit window")
       ("s" counsel-file-split "open file in split window")))
  (ivy-add-actions
    'counsel-find-file
    '(("v" counsel-file-vsplit "open file in vsplit window")
       ("s" counsel-file-split "open file in split window")))

  (defun counsel-projectile-switch-to-buffer-vsplit (x)
    (let ((buffer (if (and ivy--directory
                        (ivy--dirname-p (ivy-state-current ivy-last)))
                    (substring (ivy-state-current ivy-last) 0 -1)
                    (ivy-state-current ivy-last))))
      (evil-window-vsplit)
      (windmove-right)
      (switch-to-buffer buffer)))

  (defun counsel-projectile-switch-to-buffer-split (x)
    (let ((buffer (if (and ivy--directory
                        (ivy--dirname-p (ivy-state-current ivy-last)))
                    (substring (ivy-state-current ivy-last) 0 -1)
                    (ivy-state-current ivy-last))))
      (evil-window-split)
      (windmove-down)
      (switch-to-buffer buffer)))

  (ivy-add-actions
    'counsel-projectile-switch-to-buffer
    '(("v" counsel-projectile-switch-to-buffer-vsplit "open buffer in vsplit window")
       ("s" counsel-projectile-switch-to-buffer-split "open buffer in split window")))

  (defun ivy-switch-buffer-vsplit (x)
    (let ((buffer (if (and ivy--directory
                        (ivy--dirname-p (ivy-state-current ivy-last)))
                    (substring (ivy-state-current ivy-last) 0 -1)
                    (ivy-state-current ivy-last))))
      (if (get-buffer buffer)
        (progn
          (evil-window-vsplit)
          (windmove-right)
          (switch-to-buffer buffer))
        (message "The buffer does not exist"))))

  (defun ivy-switch-buffer-split (x)
    (let ((buffer (if (and ivy--directory
                        (ivy--dirname-p (ivy-state-current ivy-last)))
                    (substring (ivy-state-current ivy-last) 0 -1)
                    (ivy-state-current ivy-last))))
      (if (get-buffer buffer)
        (progn
          (evil-window-split)
          (windmove-down)
          (switch-to-buffer buffer))
        (message "The buffer does not exist"))))

  (ivy-add-actions
    'ivy-switch-buffer
    '(("v" ivy-switch-buffer-vsplit "open buffer in vsplit window")
       ("s" ivy-switch-buffer-split "open buffer in split window")))

  (defun counsel-rg-file ()
    (->> (ivy-state-current ivy-last)
      (s-split  ":")
      -first-item))

  (defun counsel-rg-split (x)
    (let* ((file (counsel-rg-file))
            (absolute-file (if (projectile-project-p)
                             (expand-file-name file (projectile-project-root))
                             file)))
      (evil-window-split)
      (windmove-down)
      (find-file absolute-file)))

  (defun counsel-rg-vsplit (x)
    (let* ((file (counsel-rg-file))
            (absolute-file (if (projectile-project-p)
                             (expand-file-name file (projectile-project-root))
                             file)))
      (evil-window-vsplit)
      (windmove-right)
      (find-file absolute-file)))

  (ivy-add-actions
    'counsel-rg
    '(("v" counsel-rg-vsplit "open buffer in vsplit window")
       ("s" counsel-rg-split  "open buffer in split window"))))

(add-hook! ivy-mode ivy-posframe-mode)

(after! ivy-posframe
  (setq ivy-posframe-display-functions-alist
    '((complete-symbol . ivy-posframe-display-at-point)
       (counsel-M-x . ivy-posframe-display-at-frame-top-center)
       (swiper . ivy-posframe-display-at-frame-top-center)
       (swiper-isearch . ivy-posframe-display-at-frame-top-center)
       (counsel-projectile-find-file . ivy-posframe-display-at-frame-top-center)
       (counsel-find-file . ivy-posframe-display-at-frame-top-center)
       (counsel-projectile-switch-project . ivy-posframe-display-at-frame-top-center)
       (ivy-switch-buffer . ivy-posframe-display-at-frame-top-center)
       (counsel-projectile-switch-to-buffer . ivy-posframe-display-at-frame-top-center)
       (counsel-rg . ivy-posframe-display-at-frame-top-center)
       (read-file-name-internal . ivy-posframe-display-at-frame-top-center)
       (magit-checkout . ivy-posframe-display-at-frame-top-center)
       (t               . ivy-posframe-display-at-point))))
