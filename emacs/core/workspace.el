;;; -*- lexical-binding: t; -*-

(use-package persp-mode
  :after projectile
  :init
  (setq persp-auto-resume-time -1.0 persp-auto-save-opt 0)
  :config
  (defun sl/layout-switch-by-pos (pos)
    "Switch to perspective of position POS."
    (let ((persp-to-switch
            (nth pos (persp-names-current-frame-fast-ordered))))
      (setq sl/persp-last-layout (safe-persp-name (get-current-persp)))
      (if persp-to-switch
        (persp-switch persp-to-switch))))

  (dolist (i (number-sequence 9 0 -1))
    (eval `(defun ,(intern (format "persp-switch-to-%s" i)) nil
	     ,(format "Switch to layout %s.\n%s"
		      i "See `layout-switch-by-pos' for details.")
	     (interactive)
	     (sl/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

  (defvar sl/persp-last-layout "none")
  (defun persp-switch-last-layout ()
    (interactive)
    (let ((persp-current-name (safe-persp-name (get-current-persp))))
      (persp-switch sl/persp-last-layout)
      (setq sl/persp-last-layout persp-current-name)))

  (defun sl/persp-layout ()
    "Switch to perspective of position POS."
    (interactive)
    (let* ((persp-current-name (safe-persp-name (get-current-persp)))
	   (highlight-persps (lambda (elt idx)
			       (if (string= elt persp-current-name )
				   (propertize
				    (format "%d:%s" (+ idx 1) (f-base elt))
				    'face '(:foreground "red" :background "yellow"))
				 (format "%d:%s" (+ idx 1) (f-base elt))))))
      (string-join (seq-map-indexed highlight-persps (persp-names-current-frame-fast-ordered)) " | ")))

  (defhydra sl/persp-hydra
    (:color pink :hint nil :exit t)
    "Layout: %s(sl/persp-layout)"
    ("n" persp-next "Next Layout" :column "Go to")
    ("p" persp-prev "Prev Layout")
    ("l" persp-switch "Switch Layout")
    ("0" persp-switch-to-0)
    ("1" persp-switch-to-1)
    ("2" persp-switch-to-2)
    ("3" persp-switch-to-3)
    ("4" persp-switch-to-4)
    ("5" persp-switch-to-5)
    ("6" persp-switch-to-6)
    ("7" persp-switch-to-7)
    ("8" persp-switch-to-8)
    ("9" persp-switch-to-9)
    ("<tab>" persp-switch-last-layout "Last Layout")

    ("d" persp-kill "Delete Layout" :column "Actions")
    ("r" persp-rename "Rename Layout")
    ("s" persp-save-state-to-file "Save Layout")
    ("L" persp-load-state-from-file "Load Layout")
    ("q" nil "cancel" :color blue :column nil))
  (persp-mode 1))

(defun ivy-persp-switch-project (arg)
  (interactive "P")
  (require 'counsel-projectile)
  (ivy-read "Switch to Project Perspective: "
	    (if (projectile-project-p)
		(cons (abbreviate-file-name (projectile-project-root))
		      (projectile-relevant-known-projects))
	      projectile-known-projects)
	    :action (lambda (project)
		      (let* ((persp-reset-windows-on-nil-window-conf t)
			     (exists (persp-with-name-exists-p project)))
			(persp-switch project)
			(unless exists
			  (progn
			    (let ((projectile-completion-system 'ivy))
			      (projectile-switch-project-by-name project))))))))
