;;; -*- lexical-binding: t; -*-

; (use-package multi-libvterm :after vterm)

(use-package vterm
  :commands (multi-libvterm multi-libvterm-next multi-libvterm-prev multi-libvterm-dedicated-toggle multi-libvterm-projectile toggle-tmux)
  ;; :straight (vterm :type git :host github :repo "jixiuf/emacs-libvterm")
  :config
  (defun toggle-tmux ()
    (interactive)
    (sl/layout-switch-by-pos 0)
    (let ((tmux-buffer-name "*tmux*"))
      (if-let ((tmux-buffer (get-buffer tmux-buffer-name)))
        (switch-to-buffer tmux-buffer)
        (with-current-buffer (multi-libvterm)
          (rename-buffer tmux-buffer-name)
          (vterm-send-string "tmux a -d\n")))))

  (defun vterm-send-return ()
    "Sends C-m to the libvterm."
    (interactive)
    (process-send-string vterm--process "\C-m"))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (add-hook 'vterm-mode-hook
    (lambda ()
      ;; (compilation-shell-minor-mode)
      (setq-local evil-insert-state-cursor 'box)
      (evil-insert-state)))

  (setq vterm-keymap-exceptions nil)

  (evil-define-key 'insert vterm-mode-map (kbd "C-l")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-h")      #'vterm--self-insert)

  )
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-libvterm)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-libvterm-next)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-libvterm-prev)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)

; (require 'tramp)
; (setq tramp-default-method "ssh")

; (require 'tramp-sh nil t)
; (setf tramp-ssh-controlmaster-options (concat "-o SendEnv TRAMP=yes " tramp-ssh-controlmaster-options))
