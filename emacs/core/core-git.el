;;; -*- lexical-binding: t; -*-
(use-package magit :defer 3)

(use-package magit-popup :after magit)

(use-package evil-magit :after magit)

(use-package git-messenger
  :defer 3
  :custom (git-messenger:use-magit-popup t))
(use-package git-link
  :defer 3
  :custom
  (git-link-open-in-browser t))

(use-package git-timemachine
  :commands sl/git-timemachine-on
  :config
  (defhydra sl/git-timemachine
    (:color pink :hint nil)
    ("n" git-timemachine-show-next-revision "Next Revision" :column "Go to")
    ("p" git-timemachine-show-previous-revision "Next Revision")
    ("c" git-timemachine-show-current-revision "Current Revision")
    ("g" git-timemachine-show-nth-revision "Nth Revision")
    ("t" git-timemachine-show-revision-fuzzy "Search")
    ("W" git-timemachine-kill-revision "Copy full revision" :column "Actions")
    ("w" git-timemachine-kill-abbreviated-revision "Copy abbreviated revision" :column "Actions")
    ("C" git-timemachine-show-commit "Show commit")
    ("b" git-timemachine-blame "Blame")
    ("q" git-timemachine-quit "cancel" :color blue :column nil))

  (defun sl/git-timemachine-on ()
    (interactive)
    (git-timemachine)
    (sl/git-timemachine/body)))

(use-package forge
  :after magit)

(use-package smerge-mode
   :after hydra
   ;; :defer 5
   :commands smerge-mode
   :config
   (defhydra unpackaged/smerge-hydra
     (:color pink :hint nil :post (smerge-auto-leave))
     "
 ^Move^       ^Keep^               ^Diff^                 ^Other^
 ^^-----------^^-------------------^^---------------------^^-------
 _n_ext       _b_ase               _<_: upper/base        _C_ombine
 _p_rev       _u_pper              _=_: upper/lower       _r_esolve
 ^^           _l_ower              _>_: base/lower        _k_ill current
 ^^           _a_ll                _R_efine
 ^^           _RET_: current       _E_diff
 "
     ("n" smerge-next)
     ("p" smerge-prev)
     ("b" smerge-keep-base)
     ("u" smerge-keep-upper)
     ("l" smerge-keep-lower)
     ("a" smerge-keep-all)
     ("RET" smerge-keep-current)
     ("\C-m" smerge-keep-current)
     ("<" smerge-diff-base-upper)
     ("=" smerge-diff-upper-lower)
     (">" smerge-diff-base-lower)
     ("R" smerge-refine)
     ("E" smerge-ediff)
     ("C" smerge-combine-with-next)
     ("r" smerge-resolve)
     ("k" smerge-kill-current)
     ("ZZ" (lambda ()
	     (interactive)
	     (save-buffer)
	     (bury-buffer))
      "Save and bury buffer" :color blue)
     ("q" nil "cancel" :color blue))
   :hook (magit-diff-visit-file . (lambda ()
				    (when smerge-mode
				      (unpackaged/smerge-hydra/body)))))
