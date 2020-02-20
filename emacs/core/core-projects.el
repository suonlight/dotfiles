;;; -*- lexical-binding: t; -*-

(use-package projectile
  :commands (projectile-mode projectile-project-p projectile-project-root sl/persp-hydra/body)
  :custom
  ;; (counsel-etags-ctags-options-file "~/.etags")
  ;; (tags-file-name "ETAGS")
  ;; (counsel-etags-tags-file-name "ETAGS")
  ;; (projectile-tags-file-name "ETAGS")
  (projectile-require-project-root nil))

(use-package ivy
  :defer 1
  ;; :hook (after-init . ivy-mode)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  :config
  (message "Rebuild ivy-re-builders-alist")

  (setq projectile-sort-order 'recently-active)
  ;; (setq projectile-sort-order 'access-time)
  (setq projectile-indexing-method 'hybrid)

  (setq ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-fuzzy)
                                  (swiper . ivy--regex-plus)
                                  ;; (counsel-projectile-find-file . ivy--regex-fuzzy)
                                  (t . ivy--regex-fuzzy)))

  (defun counsel-insert-models ()
    (interactive)
    (insert "models"))

  (defun counsel-insert-controllers ()
    (interactive)
    (insert "controllers"))

  (defun counsel-insert-policies ()
    (interactive)
    (insert "policies"))

  (defun counsel-insert-tests ()
    (interactive)
    (insert "spec"))

  (defun counsel-insert-services ()
    (interactive)
    (insert "services"))

  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-w") #'ivy-backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-u") #'ivy-kill-line)

  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "TAGS")
  (add-to-list 'ivy-ignore-buffers "\\*scratch\\*")

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
     ("s" counsel-rg-split  "open buffer in split window")))

  (ivy-mode))

(use-package ivy-hydra :after ivy)

(defhydra hydra-buffer (:hint nil :color pink)
  ("n" next-buffer "Next buffer")
  ("p" previous-buffer "Prev buffer")
  ("d" kill-this-buffer "Kill buffer" :column "Actions" :exit t)
  ("i" ibuffer "Ibuffer" :exit t :column "Open")
  ("m" view-echo-area-messages "Message" :exit t)
  ("b" ivy-switch-buffer "Switch buffer" :exit t)
  ("q" nil "Cancel" :color blue :column nil))

(use-package counsel-projectile
  :after projectile
  :defer 1
  :config
  (counsel-projectile-mode)
  (setq projectile-enable-caching t)
  (setq counsel-projectile-sort-files t)
  ;; (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never '' | fzf %s")
  ;; (setq counsel-rg-base-command "rg --column --line-number --no-heading --color=never --smart-case '' | fzf")
  (ivy-add-actions
    'counsel-find-file
    '(("v" counsel-file-vsplit "open file in vsplit window")
       ("s" counsel-file-split "open file in split window")))

  (ivy-add-actions
    'counsel-fzf
    '(("v" counsel-file-vsplit "open file in vsplit window")
       ("s" counsel-file-split "open file in split window"))))

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(require 'dired)
(define-key dired-mode-map (kbd "SPC") nil)

;; (use-package exec-path-from-shell
;;   :config
;;   (setq exec-path-from-shell-check-startup-files nil)
;;   (exec-path-from-shell-initialize))

(use-package prescient)
(use-package ivy-prescient
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

;; (use-package counsel-etags
;;   :init
;;   (add-hook 'prog-mode-hook
;;         (lambda ()
;;           (add-hook 'after-save-hook
;;             'counsel-etags-virtual-update-tags 'append 'local)))
;;   :config
;;   (setq tags-revert-without-query 1)
;;   (setq counsel-etags-update-interval 600)
;;   ;; (setq counsel-etags-find-program "/usr/local/bin/fd")
;;   (setq counsel-etags-tags-program "/usr/local/bin/ctags")
;;   (add-to-list 'counsel-etags-ignore-directories "build")
;;   (add-to-list 'counsel-etags-ignore-directories "org-modes"))

;; (use-package snails
;;   :straight (snails :type git :host github :repo "manateelazycat/snails")
;;   :config
;;   (setq snails-default-backends '(snails-backend-fd snails-backend-projectile snails-backend-buffer)))
;; (if (featurep 'cocoa)
;;     (progn
;;       (setq ns-use-native-fullscreen nil)
;;       (setq ns-use-fullscreen-animation nil)
;;       (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;       (run-at-time "2sec" nil
;;                    (lambda ()
;;                      (toggle-frame-fullscreen)
;;                      )))
;;   (require 'fullscreen)
;;   (fullscreen))

;; (use-package fzf :load-path "~/projects/fzf.el")
;; (use-package flx)
;; (use-package fuz :straight (fuz.el :type git :host github :repo "cireu/fuz.el"))
