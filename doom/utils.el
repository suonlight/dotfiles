(defun sl/new-workspace-and-vterm ()
  (interactive)
  (+workspace/new)
  (+vterm/here nil))

(defun sl/new-vterm-and-split ()
  (interactive)
  (split-window-below)
  (evil-window-down 1)
  (+vterm/here nil))

(defun sl/new-vterm-and-vsplit ()
  (interactive)
  (split-window-right)
  (evil-window-right 1)
  (+vterm/here nil))
