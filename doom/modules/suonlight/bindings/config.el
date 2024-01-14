;;; -*- lexical-binding: t; -*-

(when (modulep! :editor evil +everywhere)
  ;; NOTE SPC u replaces C-u as the universal argument.

  ;; Minibuffer
  (map! :map (evil-ex-completion-map evil-ex-search-keymap)
        "C-a" #'evil-beginning-of-line
        "C-b" #'evil-backward-char
        "C-f" #'evil-forward-char
        :gi "C-j" #'next-complete-history-element
        :gi "C-k" #'previous-complete-history-element)

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-a"    #'move-beginning-of-line
    "C-r"    #'evil-paste-from-register
    "C-u"    #'evil-delete-back-to-indentation
    "C-v"    (general-simulate-key "C-; C-v")
    "C-s"    (general-simulate-key "C-; C-s")
    "C-w"    #'doom/delete-backward-word
    "C-z"    (cmd! (ignore-errors (call-interactively #'undo))))
  (define-key! :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line
    "C-S-j"  #'scroll-up-command
    "C-S-k"  #'scroll-down-command)
  (define-key! :states 'insert :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line)
  (define-key! read-expression-map
    "C-j" #'next-line-or-history-element
    "C-k" #'previous-line-or-history-element))

;;
;;; Global keybindings

;; Smart tab, these will only work in GUI Emacs
(map! :i [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
                 (and (modulep! :editor snippets)
                      (bound-and-true-p yas-minor-mode)
                      (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                 #'yas-expand
                 (and (modulep! :completion company +tng)
                      (+company-has-completion-p))
                 #'+company/complete)
      :n [tab] (general-predicate-dispatch nil
                 (and (modulep! :editor fold)
                      (save-excursion (end-of-line) (invisible-p (point))))
                 #'+fold/toggle
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)
      :v [tab] (general-predicate-dispatch nil
                 (and (bound-and-true-p yas-minor-mode)
                      (or (eq evil-visual-selection 'line)
                          (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                 #'yas-insert-snippet
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)

      ;; Smarter newlines
      :i [remap newline] #'newline-and-indent  ; auto-indent on newline
      :i "C-j"           #'+default/newline    ; default behavior

      (:after help :map help-mode-map
        :n "o"       #'link-hint-open-link)
      (:after helpful :map helpful-mode-map
        :n "o"       #'link-hint-open-link)
      (:after info :map Info-mode-map
        :n "o"       #'link-hint-open-link)
      (:after apropos :map apropos-mode-map
        :n "o"       #'link-hint-open-link
        :n "TAB"     #'forward-button
        :n [tab]     #'forward-button
        :n [backtab] #'backward-button)
      (:after view :map view-mode-map
        [escape]  #'View-quit-all)
      (:after man :map Man-mode-map
        :n "q"    #'kill-current-buffer)

      :m "gs"     #'+evil/easymotion  ; lazy-load `evil-easymotion'
      (:after org
        :map org-mode-map
        :prefix "<easymotion>"
        "h" #'+org/goto-visible)

      (:when (modulep! :editor multiple-cursors)
        :prefix "gz"
        :nv "d" #'evil-mc-make-and-goto-next-match
        :nv "D" #'evil-mc-make-and-goto-prev-match
        :nv "j" #'evil-mc-make-cursor-move-next-line
        :nv "k" #'evil-mc-make-cursor-move-prev-line
        :nv "m" #'evil-mc-make-all-cursors
        :nv "n" #'evil-mc-make-and-goto-next-cursor
        :nv "N" #'evil-mc-make-and-goto-last-cursor
        :nv "p" #'evil-mc-make-and-goto-prev-cursor
        :nv "P" #'evil-mc-make-and-goto-first-cursor
        :nv "q" #'evil-mc-undo-all-cursors
        :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
        :nv "u" #'evil-mc-undo-last-added-cursor
        :nv "z" #'+multiple-cursors/evil-mc-toggle-cursor-here
        :v  "I" #'evil-mc-make-cursor-in-visual-selection-beg
        :v  "A" #'evil-mc-make-cursor-in-visual-selection-end)

      ;; misc
      :n "C-S-f"  #'toggle-frame-fullscreen
      :n "C-+"    #'doom/reset-font-size
      ;; Buffer-local font resizing
      :n "C-="    #'text-scale-increase
      :n "C--"    #'text-scale-decrease
      ;; Frame-local font resizing
      :n "M-C-="  #'doom/increase-font-size
      :n "M-C--"  #'doom/decrease-font-size)

;;
;;; Module keybinds

;;; :completion
(map! (:when (modulep! :completion company)
        :i "C-@"      #'+company/complete
        :i "C-SPC"    #'+company/complete
        (:after company
          (:map company-active-map
            "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
            "C-n"     #'company-select-next
            "C-p"     #'company-select-previous
            "C-j"     #'company-select-next
            "C-k"     #'company-select-previous
            "C-h"     #'company-show-doc-buffer
            "C-u"     #'company-previous-page
            "C-d"     #'company-next-page
            "C-s"     #'company-filter-candidates
            "C-S-s"   #'counsel-company
            "C-/"     #'counsel-company
            "C-SPC"   #'company-complete-common
            "TAB"     #'company-complete-common-or-cycle
            [tab]     #'company-complete-common-or-cycle
            [backtab] #'company-select-previous
            [f1]      nil)
          (:map company-search-map  ; applies to `company-filter-map' too
            "C-n"     #'company-select-next-or-abort
            "C-p"     #'company-select-previous-or-abort
            "C-j"     #'company-select-next-or-abort
            "C-k"     #'company-select-previous-or-abort
            "C-s"     (λ! (company-search-abort) (company-filter-candidates))
            [escape]  #'company-search-abort))

        ;; TAB auto-completion in term buffers
        (:after comint :map comint-mode-map
          "TAB" #'company-complete
          [tab] #'company-complete))

      (:when (modulep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          "C-SPC" #'ivy-call-and-recenter  ; preview file
          "C-l"   #'ivy-alt-done)
        (:after counsel
          :map counsel-ag-map
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "C-l"      #'ivy-done
          [C-return] #'+ivy/git-grep-other-window-action)))

;;; :ui
(map! (:when (modulep! :ui popup)
        :n "C-`"   #'+popup/toggle
        :n "C-~"   #'+popup/raise
        :g "C-x p" #'+popup/other)
      (:when IS-LINUX
        :gn "s-w"      #'delete-window
        :gn "s-s"      #'save-buffer
        :gn "s-v"      #'yank
        :gn "s-c"      #'kill-ring-save
        :gn "C-c c"    #'counsel-linux-app
        :gn "s-x"      #'counsel-M-x
        :gn "s-t"      #'multi-vterm)
      (:when (modulep! :ui workspaces)
        :n "C-t"   #'sl/new-workspace-and-vterm
        :n "C-S-t" #'+workspace/display
        ;; :g "M-1"   #'+workspace/switch-to-0
        ;; :g "M-2"   #'+workspace/switch-to-1
        ;; :g "M-3"   #'+workspace/switch-to-2
        ;; :g "M-4"   #'+workspace/switch-to-3
        ;; :g "M-5"   #'+workspace/switch-to-4
        ;; :g "M-6"   #'+workspace/switch-to-5
        ;; :g "M-7"   #'+workspace/switch-to-6
        ;; :g "M-8"   #'+workspace/switch-to-7
        ;; :g "M-9"   #'+workspace/switch-to-8
        ;; :g "M-0"   #'+workspace/switch-to-final
        :g "s-t"   #'sl/new-workspace-and-vterm
        :g "s-T"   #'+workspace/display
        :n "s-1"   #'+workspace/switch-to-0
        :n "s-2"   #'+workspace/switch-to-1
        :n "s-3"   #'+workspace/switch-to-2
        :n "s-4"   #'+workspace/switch-to-3
        :n "s-5"   #'+workspace/switch-to-4
        :n "s-6"   #'+workspace/switch-to-5
        :n "s-7"   #'+workspace/switch-to-6
        :n "s-8"   #'+workspace/switch-to-7
        :n "s-9"   #'+workspace/switch-to-8
        :n "s-0"   #'+workspace/switch-to-final))

;;; :editor
(map! (:when (modulep! :editor format)
        :n "gQ" #'+format:region)

      (:when (modulep! :editor rotate-text)
        :n "!"  #'rotate-text)

      (:when (modulep! :editor multiple-cursors)
        ;; evil-multiedit
        :v  "R"     #'evil-multiedit-match-all
        :n  "M-d"   #'evil-multiedit-match-symbol-and-next
        :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
        :v  "M-d"   #'evil-multiedit-match-and-next
        :v  "M-D"   #'evil-multiedit-match-and-prev
        :nv "C-M-d" #'evil-multiedit-restore
        (:after evil-multiedit
          (:map evil-multiedit-state-map
            "M-d"    #'evil-multiedit-match-and-next
            "M-D"    #'evil-multiedit-match-and-prev
            "RET"    #'evil-multiedit-toggle-or-restrict-region
            [return] #'evil-multiedit-toggle-or-restrict-region)))

      (:when (modulep! :editor snippets)
        ;; auto-yasnippet
        :i  [C-tab] #'aya-expand
        :nv [C-tab] #'aya-create))

;;; :tools
(when (modulep! :tools eval)
  (map! "M-r" #'+eval/buffer))


;;
;;; <leader>
(map! :leader
      (:prefix-map ("j" . "jump")
        "D"         #'dired-jump-other-window
        "b"         #'avy-pop-mark
        "d"         #'dired-jump
        "f"         #'find-function
        "r"         #'avy-resume
        "j"         #'evil-avy-goto-char-timer
        "l"         #'evil-avy-goto-line
        "w"         #'evil-avy-goto-word-or-subword-1
        "n"         #'sp-newline
        "o"         #'open-line
        "q"         #'dumb-jump-go-prefer-external
        "Q"         #'dumb-jump-go-other-window
        "s"         #'sp-split-sexp
        "v"         #'find-variable))

(map! :leader
      :desc "Eval expression"       ";"    #'pp-eval-expression
      :desc "M-x"                   ":"    #'execute-extended-command
      :desc "Org Capture"           "X"    #'org-capture

      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "window"                "w"    evil-window-map
      :desc "help"                  "h"    help-map

      (:when (modulep! :ui popup)
        :desc "Toggle last popup"   "~"  #'+popup/toggle)
      :desc "Find file"             "."    #'find-file

      :desc "Switch buffer"         ","    #'switch-to-buffer
      :desc "Switch to last buffer" "<tab>"    #'evil-switch-to-windows-last-buffer
      :desc "Resume last search"    "'"
      (cond ((modulep! :completion vertico)    #'vertico-repeat)
            ((modulep! :completion ivy)        #'ivy-resume)
            ((modulep! :completion helm)       #'helm-resume))


      :desc "Search project"               "/" #'+default/search-project
      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point

      :desc "Find file in project"  "SPC"  #'projectile-find-file
      :desc "Jump to bookmark"      "RET"  #'bookmark-jump

      ;;; <leader> TAB --- workspace
      (:when (modulep! :ui workspaces)
        :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
        :desc "Switch buffer"           "<" #'switch-to-buffer
        (:prefix-map ("l" . "workspace")
          :desc "Display tab bar"           "TAB" #'+workspace/display
          :desc "Switch workspace"          "."   #'+workspace/switch-to
          :desc "Switch to last workspace"  "`"   #'+workspace/other
          :desc "New workspace"             "n"   #'+workspace/new
          :desc "Load workspace from file"  "l"   #'+workspace/load
          :desc "Save workspace to file"    "s"   #'+workspace/save
          :desc "Delete session"            "x"   #'+workspace/kill-session
          :desc "Delete this workspace"     "d"   #'+workspace/delete
          :desc "Rename workspace"          "r"   #'+workspace/rename
          :desc "Restore last session"      "R"   #'+workspace/restore-last-session
          :desc "Next workspace"            "]"   #'+workspace/switch-right
          :desc "Previous workspace"        "["   #'+workspace/switch-left
          :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
          :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
          :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
          :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
          :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
          :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
          :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
          :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
          :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
          :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))
      ;; (:when IS-LINUX
      ;;   :desc "Switch buffer"           "<" #'exwm-workspace-switch-to-buffer
      ;;   (:prefix-map ("l" . "workspace")
      ;;     :desc "Display tab bar"           "TAB" #'exwm-workspace-switch
      ;;     :desc "Load workspace from file"  "l"   #'exwm-workspace-switch))
      ;;; <leader> b --- buffer
      (:prefix-map ("b" . "buffer")
        :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
        :desc "Previous buffer"             "["   #'previous-buffer
        :desc "Next buffer"                 "]"   #'next-buffer
        (:when (modulep! :ui workspaces)
          :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           "B" #'switch-to-buffer)
        (:unless (modulep! :ui workspaces)
          :desc "Switch buffer"           "b" #'switch-to-buffer)
        :desc "Kill buffer"                 "d"   #'kill-current-buffer
        :desc "ibuffer"                     "i"   #'ibuffer
        :desc "Kill buffer"                 "k"   #'kill-current-buffer
        :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
        :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
        :desc "Set bookmark"                "m"   #'bookmark-set
        :desc "Delete bookmark"             "M"   #'bookmark-delete
        :desc "Next buffer"                 "n"   #'next-buffer
        :desc "New empty buffer"            "N"   #'evil-buffer-new
        :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
        :desc "Previous buffer"             "p"   #'previous-buffer
        :desc "Revert buffer"               "r"   #'revert-buffer
        :desc "Save buffer"                 "s"   #'basic-save-buffer
        :desc "Save all buffers"            "S"   #'evil-write-all
        :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
        :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
        :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
        :desc "Bury buffer"                 "z"   #'bury-buffer
        :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)

      ;;; <leader> c --- code
      (:prefix-map ("c" . "code")
       (:when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
        :desc "LSP Execute code action" "a" #'lsp-execute-code-action
        :desc "LSP Organize imports" "o" #'lsp-organize-imports
        (:when (modulep! :completion ivy)
         :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
         :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
        (:when (modulep! :completion helm)
         :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
         :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol)
        (:when (modulep! :completion vertico)
         :desc "Jump to symbol in current workspace" "j"   #'consult-lsp-symbols
         :desc "Jump to symbol in any workspace"     "J"   (cmd!! #'consult-lsp-symbols 'all-workspaces))
        :desc "LSP"                                  "l"   #'+default/lsp-command-map
        :desc "LSP Rename"                           "r"   #'lsp-rename)
       (:when (modulep! :tools lsp +eglot)
        :desc "LSP Execute code action" "a" #'eglot-code-actions
        :desc "LSP Rename" "r" #'eglot-rename
        :desc "LSP Find declaration" "j" #'eglot-find-declaration)
       :desc "Compile"                               "c"   #'compile
       :desc "Recompile"                             "C"   #'recompile
       :desc "Jump to definition"                    "d"   #'+lookup/definition
       :desc "Jump to references"                    "D"   #'+lookup/references
       :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
       :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
       :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
       :desc "Find implementations"                  "i"   #'+lookup/implementations
       :desc "Jump to documentation"                 "k"   #'+lookup/documentation
       :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
       :desc "Find type definition"                  "t"   #'+lookup/type-definition
       :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
       :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
       :desc "List errors"                           "x"   #'flymake-show-diagnostics-buffer
       (:when (modulep! :checkers syntax)
        :desc "List errors"                         "x"   #'flycheck-list-errors))

      ;;; <leader> f --- file
      (:prefix-map ("f" . "file")
        :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
        :desc "Copy this file"              "C"   #'doom/copy-this-file
        :desc "Find directory"              "d"   #'dired
        :desc "Delete this file"            "D"   #'doom/delete-this-file
        :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
        :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
        :desc "Find file"                   "f"   #'find-file
        :desc "Find file from here"         "F"   #'+default/find-file-under-here
        :desc "Locate file"                 "l"   #'locate
        :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
        :desc "Browse private config"       "P"   #'doom/open-private-config
        :desc "Recent files"                "r"   #'recentf-open-files
        :desc "Rename/move file"            "R"   #'doom/move-this-file
        :desc "Save file"                   "s"   #'save-buffer
        :desc "Save file as..."             "S"   #'write-file
        :desc "Sudo find file"              "u"   #'doom/sudo-find-file
        :desc "Sudo this file"              "U"   #'doom/sudo-this-file
        :desc "Yank file path"              "y"   #'+default/yank-buffer-path
        :desc "Yank file path from project" "Y"   #'+default/yank-buffer-path-relative-to-project)

      ;;; <leader> g --- git
      (:prefix-map ("g" . "git")
        :desc "Git revert file"             "R"   #'vc-revert
        :desc "Copy link to remote"         "y"   #'+vc/browse-at-remote-kill
        :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
        :desc "Timemachine"                 "t"   #'git-timemachine
       (:when (modulep! :ui hydra)
          :desc "SMerge"                    "m"   #'+vc/smerge-hydra/body)
        (:when (modulep! :ui vc-gutter)
          :desc "Git revert hunk"           "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"          "t"   #'git-timemachine-toggle
          :desc "Jump to next hunk"         "]"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"     "["   #'git-gutter:previous-hunk)
        (:when (modulep! :tools magit)
          :desc "Magit dispatch"            "/"   #'magit-dispatch
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit switch branch"       "B"   #'magit-branch-checkout
          :desc "Magit status"              "s"   #'magit-status
          :desc "Magit file delete"         "D"   #'magit-file-delete
          :desc "Magit blame"               "b"   #'magit-blame-addition
          :desc "Magit clone"               "C"   #'magit-clone
          :desc "Magit fetch"               "F"   #'magit-fetch
          :desc "Magit buffer log"          "L"   #'magit-log
          :desc "Git stage file"            "S"   #'magit-stage-file
          :desc "Git unstage file"          "U"   #'magit-unstage-file
          :desc "Code Review"               "r"   #'code-review-start
          (:prefix ("f" . "find")
            :desc "Find file"                 "f"   #'magit-find-file
            :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
            :desc "Find commit"               "c"   #'magit-show-commit
            :desc "Find issue"                "i"   #'forge-visit-issue
            :desc "Find pull request"         "p"   #'forge-visit-pullreq)
          (:prefix ("o" . "open in browser")
            :desc "Browse file or region"     "o"   #'browse-at-remote
            :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
            :desc "Browse remote"             "r"   #'forge-browse-remote
            :desc "Browse commit"             "c"   #'forge-browse-commit
            :desc "Browse an issue"           "i"   #'forge-browse-issue
            :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
            :desc "Browse issues"             "I"   #'forge-browse-issues
            :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
          (:prefix ("l" . "list")
            (:when (modulep! :tools gist)
              :desc "List gists"              "g"   #'+gist:list)
            :desc "List repositories"         "r"   #'magit-list-repositories
            :desc "List submodules"           "s"   #'magit-list-submodules
            :desc "List issues"               "i"   #'forge-list-issues
            :desc "List pull requests"        "p"   #'forge-list-pullreqs
            :desc "List notifications"        "n"   #'forge-list-notifications)
          (:prefix ("c" . "create")
            :desc "Initialize repo"           "r"   #'magit-init
            :desc "Clone repo"                "R"   #'magit-clone
            :desc "Commit"                    "c"   #'magit-commit-create
            :desc "Fixup"                     "f"   #'magit-commit-fixup
            :desc "Branch"                    "b"   #'magit-branch-and-checkout
            :desc "Issue"                     "i"   #'forge-create-issue
            :desc "Pull request"              "p"   #'forge-create-pullreq)))

      ;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
        :desc "Current file name"             "f"   #'+default/insert-file-path
        :desc "Current file path"             "F"   (λ!! #'+default/insert-file-path t)
        :desc "Evil ex path"                  "p"   (λ! (evil-ex "R!echo "))
        :desc "From evil register"            "r"   #'evil-ex-registers
        :desc "Snippet"                       "s"   #'yas-insert-snippet
        :desc "Unicode"                       "u"   #'unicode-chars-list-chars
        :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
        :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
        :desc "Org agenda"                   "a" #'org-agenda
        :desc "Toggle org-clock"             "c" #'+org/toggle-clock
        :desc "Cancel org-clock"             "C" #'org-clock-cancel
        :desc "Open deft"                    "d" #'deft
        :desc "Find file in notes"           "f" #'+default/find-in-notes
        :desc "Browse notes"                 "F" #'+default/browse-notes
        :desc "Org store link"               "l" #'org-store-link
        :desc "Tags search"                  "m" #'org-tags-view
        :desc "Org capture"                  "n" #'org-capture
        :desc "Active org-clock"             "o" #'org-clock-goto
        :desc "Todo list"                    "t" #'sl/roam-list-todos
        :desc "Search notes"                 "s" #'+default/org-notes-search
        :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
        :desc "View search"                  "v" #'org-search-view
        :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
        :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text

        (:when (modulep! :lang org +roam)
          (:prefix ("r" . "roam")
            :desc "Switch to buffer" "b" #'org-roam-switch-to-buffer
            :desc "Org Roam Capture" "c" #'org-roam-capture
            :desc "Org Roam Server"  "s" #'org-roam-server-mode
            :desc "Find file"        "f" #'org-roam-find-file
            :desc "Show graph"       "g" #'org-roam-graph-show
            :desc "Insert"           "i" #'org-roam-insert
            :desc "Org Roam"         "r" #'org-roam
            (:prefix ("d" . "by date")
              :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
              :desc "Today"          "t" #'org-roam-dailies-find-today
              :desc "Tomorrow"       "m" #'org-roam-dailies-find-tomorrow
              :desc "Yesterday"      "y" #'org-roam-dailies-find-yesterday)))

        (:when (modulep! :lang org +roam2)
          (:prefix ("r" . "roam")
            :desc "Open random node"           "a" #'org-roam-node-random
            :desc "Find node"                  "f" #'org-roam-node-find
            :desc "Find ref"                   "F" #'org-roam-ref-find
            :desc "Show graph"                 "g" #'org-roam-graph
            :desc "Insert node"                "i" #'org-roam-node-insert
            :desc "Capture to node"            "n" #'org-roam-capture
            :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
            :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
            :desc "Sync database"              "s" #'org-roam-db-sync
            :desc "UI"                         "S" #'org-roam-ui-mode
            (:prefix ("d" . "by date")
              :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
              :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
              :desc "Capture date"              "D" #'org-roam-dailies-capture-date
              :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
              :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
              :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
              :desc "Capture today"             "n" #'org-roam-dailies-capture-today
              :desc "Goto today"                "t" #'org-roam-dailies-goto-today
              :desc "Capture today"             "T" #'org-roam-dailies-capture-today
              :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
              :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
              :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))

        (:when (modulep! :lang org +journal)
          (:prefix ("j" . "journal")
            :desc "New Entry"      "j" #'org-journal-new-entry
            :desc "Search Forever" "s" #'org-journal-search-forever)))

      ;;; <leader> o --- open
      (:prefix-map ("o" . "open")
        :desc "Org agenda"       "A"  #'org-agenda
        (:prefix ("a" . "org agenda")
          :desc "Agenda"         "a"  #'org-agenda
          :desc "Todo list"      "t"  #'org-todo-list
          :desc "Tags search"    "m"  #'org-tags-view
          :desc "View search"    "v"  #'org-search-view)
        :desc "Browse Web"         "w"  #'browse-url
        :desc "Default browser"    "b"  #'browse-url-of-file
        :desc "Start debugger"     "d"  #'+debugger/start
        :desc "New frame"          "f"  #'make-frame
        :desc "REPL"               "r"  #'+eval/open-repl-other-window
        :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
        :desc "Dired"              "-"  #'dired-jump
        (:when (modulep! :ui neotree)
          :desc "Project sidebar"              "p" #'+neotree/open
          :desc "Find file in project sidebar" "P" #'+neotree/find-this-file)
        (:when (modulep! :ui treemacs)
          :desc "Project sidebar" "p" #'+treemacs/toggle
          :desc "Find file in project sidebar" "P" #'+treemacs/find-file)
        (:when (modulep! :term shell)
          :desc "Toggle shell popup"    "t" #'+shell/toggle
          :desc "Open shell here"       "T" #'+shell/here)
        (:when (modulep! :term term)
          :desc "Toggle terminal popup" "t" #'+term/toggle
          :desc "Open terminal here"    "T" #'+term/here)
        (:when (modulep! :term vterm)
          :desc "Toggle vterm popup"    "t" #'multi-vterm-project
          :desc "Open vterm here"       "T" #'multi-vterm)
        (:when (modulep! :term eshell)
          :desc "Toggle eshell popup"   "e" #'+eshell/toggle
          :desc "Open eshell here"      "E" #'+eshell/here)
        (:when (modulep! :tools macos)
          :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
          :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
          :desc "Send to Transmit"           "u" #'+macos/send-to-transmit
          :desc "Send project to Transmit"   "U" #'+macos/send-project-to-transmit
          :desc "Send to Launchbar"          "l" #'+macos/send-to-launchbar
          :desc "Send project to Launchbar"  "L" #'+macos/send-project-to-launchbar)
        (:when (modulep! :tools docker)
          :desc "Docker" "D" #'docker))

      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
        :desc "Toggle Implementation and Test"              "a" #'projectile-toggle-between-implementation-and-test
        :desc "Find file at point"           "*" #'sl/projectile-find-file-at-point
        :desc "Browse project"               "." #'+default/browse-project
        :desc "Browse other project"         ">" #'doom/browse-in-other-project
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Add new project"              "A" #'projectile-add-known-project
        :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
        :desc "Compile in project"           "c" #'projectile-compile-project
        :desc "Repeat last command"          "C" #'projectile-repeat-last-command
        :desc "Remove known project"         "d" #'projectile-remove-known-project
        :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
        :desc "Find file in project"         "f" #'projectile-find-file
        :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
        :desc "Find tag"                     "g" #'projectile-find-tag
        :desc "Generate tag"                 "G" #'projectile-regenerate-tags
        :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
        :desc "Kill project buffers"         "k" #'projectile-kill-buffers
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Find recent project files"    "r" #'projectile-recentf
        :desc "Run project"                  "R" #'projectile-run-project
        :desc "Save project files"           "s" #'projectile-save-project-buffers
        :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
        :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
        :desc "List project tasks"           "t" #'magit-todos-list
        :desc "Test project"                 "T" #'projectile-test-project)

      ;;; <leader> q --- quit/session
      (:prefix-map ("q" . "quit/session")
        :desc "Restart emacs server"         "d" #'+default/restart-server
        :desc "Delete frame"                 "f" #'delete-frame
        :desc "Clear current frame"          "F" #'doom/kill-all-buffers
        :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
        :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
        :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
        :desc "Quick save current session"   "s" #'doom/quicksave-session
        :desc "Restore last session"         "l" #'doom/quickload-session
        :desc "Save session to file"         "S" #'doom/save-session
        :desc "Restore session from file"    "L" #'doom/load-session
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart)

      ;;; <leader> r --- remote
      (:when (modulep! :tools upload)
        (:prefix-map ("r" . "remote")
          :desc "Upload local"               "u" #'ssh-deploy-upload-handler
          :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
          :desc "Download remote"            "d" #'ssh-deploy-download-handler
          :desc "Diff local & remote"        "D" #'ssh-deploy-diff-handler
          :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
          :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

      ;;; <leader> s --- search
      (:prefix-map ("s" . "search")
        :desc "Search buffer"                "b"
        (cond ((modulep! :completion vertico)   #'+default/search-buffer)
              ((modulep! :completion ivy)       #'swiper)
              ((modulep! :completion helm)      #'swiper))
        :desc "Search current directory"     "d" #'+default/search-cwd
        :desc "Search other directory"       "D" #'+default/search-other-cwd
        :desc "Locate file"                  "f" #'locate
        :desc "Jump to symbol"               "i" #'imenu
        :desc "Jump to visible link"         "l" #'link-hint-open-link
        :desc "Jump to link"                 "L" #'ffap-menu
        :desc "Jump list"                    "j" #'evil-show-jumps
        :desc "Jump to bookmark"             "m" #'bookmark-jump
        :desc "Look up online"               "o" #'+lookup/online
        :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
        :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
        :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
        :desc "Search project"               "p" #'+default/search-project
        :desc "Search other project"         "P" #'+default/search-other-project
        :desc "Jump to mark"                 "r" #'evil-show-marks
        :desc "Search buffer"                "s" #'+default/search-buffer
        :desc "Search buffer for thing at point" "S"
        (cond ((modulep! :completion vertico)   #'+vertico/search-symbol-at-point)
              ((modulep! :completion ivy)       #'swiper-isearch-thing-at-point)
              ((modulep! :completion helm)      #'swiper-isearch-thing-at-point))

        :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
        :desc "Thesaurus"                    "T" #'+lookup/synonyms)

      ;;; <leader> r --- registers
      (:prefix-map ("r" . "registers")
        :desc "List Registers"                "e" #'evil-show-registers)

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Flymake"                      "f" #'flymake-mode
        (:when (modulep! :checkers syntax)
          :desc "Flycheck"                   "f" #'flycheck-mode)
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        :desc "Indent guides"                "i" #'indent-guide-global-mode
        :desc "Indent style"                 "I" #'doom/toggle-indent-style
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        (:when (modulep! :lang org +present)
          :desc "org-tree-slide mode"        "p" #'org-tree-slide-mode)
        :desc "Read-only mode"               "r" #'read-only-mode
        (:when (modulep! :checkers spell)
          :desc "Flyspell"                   "s" #'flyspell-mode)
        (:when (modulep! :lang org +pomodoro)
          :desc "Pomodoro timer"             "t" #'org-pomodoro)
        :desc "Word-wrap mode"               "w" #'+word-wrap-mode
        (:when (modulep! :ui minimap)
          :desc "Minimap"                      "m" #'minimap-mode)
        (:when (modulep! :ui zen)
          :desc "Zen mode"                   "z" #'+zen/toggle
          :desc "Zen mode (fullscreen)"      "Z" #'+zen/toggle-fullscreen)))

;; Custom bindings
(map! :nv "C-l"   #'evil-window-right
      :nv "C-h"   #'evil-window-left
      :nv "C-j"   #'evil-window-down
      :nv "C-k"   #'evil-window-up
      :nv "C-p"   #'projectile-find-file
      :nv "s-b"   #'projectile-switch-to-buffer
      :nv "<f10>" #'doom/window-maximize-buffer
      :nv "<f9>"   #'org-roam-node-find
      :nv "<f12>" #'multi-vterm-project
      :nv "gh"    #'sl/jump-to-cursor
      :nv "s-`"   #'+evil/next-frame
      :nv "s-<f12>" #'list-processes
      :nv ";"     #'evil-repeat-find-char
      :nv "s"     #'evil-avy-goto-word-or-subword-1
      :nv "gy"    (general-simulate-key "yyPgccj" :state 'normal)
      :nv "g]"    #'dumb-jump-go
      :nv "g["    #'dumb-jump-back)

(map! :leader
  "2" (general-simulate-key "SPC w C-o SPC w v SPC w l SPC b p SPC w h")
  "3" (general-simulate-key "SPC w C-o SPC w v SPC w l SPC b p SPC w v SPC w l SPC b p 2 SPC w h")
  "!"   #'sl/send-cmd-to-multi-vterm-project
  (:prefix-map ("a" . "application")
    "c" #'calendar
    "d" #'projectile-dired)
  (:prefix-map ("x" . "text")
    "gt" #'google-translate-at-point
    "s"  #'read-aloud-this
    (:prefix-map ("f" . "format")
      "g"       #'copy-as-format-github
      "h"       #'copy-as-format-html
      "m"       #'copy-as-format-markdown
      "o"       #'copy-as-format-org-mode
      "s"       #'copy-as-format-slack)
    (:prefix-map ("i" . "string-inflection")
      "-"       #'string-inflection-kebab-case
      "C"       #'string-inflection-camelcase
      "U"       #'string-inflection-upcase
      "_"       #'string-inflection-underscore
      "c"       #'string-inflection-lower-camelcase
      "k"       #'string-inflection-kebab-case
      "u"       #'string-inflection-underscore))
  :nv "ft" #'+treemacs/toggle)

(map!
  :inv "C-<f6>"    #'projectile-toggle-between-implementation-and-test
  :g  "C-\\"       #'toggle-input-method
  (:map ruby-mode-map
    :i  "s-r l"    #'eacl-complete-line
    :i  "s-r m"    #'eacl-complete-multiline)
  (:map js-mode-map
    :i  "s-r l"    #'eacl-complete-line
    :i  "s-r m"    #'eacl-complete-multiline)
  (:map typescript-mode-map
    :i  "s-r l"    #'eacl-complete-line
    :i  "s-r m"    #'eacl-complete-multiline)
  (:map typescript-tsx-mode-map
    :i  "s-r l"    #'eacl-complete-line
    :i  "s-r m"    #'eacl-complete-multiline)
  (:map ruby-ts-mode-map
    :i  "s-r l"    #'eacl-complete-line
    :i  "s-r m"    #'eacl-complete-multiline)
  (:map js-ts-mode-map
    :i  "s-r l"    #'eacl-complete-line
    :i  "s-r m"    #'eacl-complete-multiline)
  (:map typescript-ts-mode-map
    :i  "s-r l"    #'eacl-complete-line
    :i  "s-r m"    #'eacl-complete-multiline)
  (:map tsx-ts-mode-map
    :i  "s-r l"    #'eacl-complete-line
    :i  "s-r m"    #'eacl-complete-multiline)
  (:map comint-mode-map
    :i "s-1"       #'+workspace/switch-to-0
    :i "s-2"       #'+workspace/switch-to-1
    :i "s-3"       #'+workspace/switch-to-2
    :i "s-4"       #'+workspace/switch-to-3
    :i "s-5"       #'+workspace/switch-to-4
    :i "s-6"       #'+workspace/switch-to-5
    :i "s-7"       #'+workspace/switch-to-6
    :i "s-8"       #'+workspace/switch-to-7
    :i "s-9"       #'+workspace/switch-to-8
    :i "s-0"       #'+workspace/switch-to-final)
  (:after vterm :map vterm-mode-map
    :i "s-1"       #'+workspace/switch-to-0
    :i "s-2"       #'+workspace/switch-to-1
    :i "s-3"       #'+workspace/switch-to-2
    :i "s-4"       #'+workspace/switch-to-3
    :i "s-5"       #'+workspace/switch-to-4
    :i "s-6"       #'+workspace/switch-to-5
    :i "s-7"       #'+workspace/switch-to-6
    :i "s-8"       #'+workspace/switch-to-7
    :i "s-9"       #'+workspace/switch-to-8
    :i "s-0"       #'+workspace/switch-to-final)
  (:after vterm :map vterm-mode-map
    :nv "i"        #'evil-insert-resume
    :nv "o"        #'evil-insert-resume
    :nv "<return>" #'evil-insert-resume
    :nv ",s"       #'sl/new-vterm-and-split
    :nv ",v"       #'sl/new-vterm-and-vsplit
    :nv ",n"       #'multi-vterm-next
    :nv ",p"       #'multi-vterm-prev
    :i "s-'"       #'multi-vterm-project
    :i "<f12>"     #'multi-vterm-project
    :i "s-`"       #'+evil/next-frame
    :i "C-j"       #'evil-window-down
    :i "C-k"       #'evil-window-up
    :i "C-h"       #'evil-window-left
    :i "C-l"       #'evil-window-right
    :i "C-c"       #'vterm--self-insert
    :i "C-d"       #'vterm--self-insert
    :i "C-g"       #'vterm--self-insert
    :i "C-e"       #'vterm--self-insert
    :i "C-a"       #'vterm--self-insert
    :i "C-u"       #'vterm--self-insert
    :i "C-w"       #'vterm--self-insert
    :i "C-n"       #'vterm--self-insert
    :i "C-p"       #'vterm--self-insert
    :i "C-r"       #'vterm--self-insert
    :i "C-t"       #'vterm--self-insert
    :i "C-SPC"     #'vterm--self-insert)
  (:after treemacs :map treemacs-mode-map
    "C-l"          #'evil-window-right
    "C-h"          #'evil-window-left
    "C-j"          #'evil-window-down
    "C-k"          #'evil-window-up)
  (:after org :map org-mode-map
    :i  "s-r"    #'org-roam-node-insert
    :nv "t"      #'org-todo)
  (:map xwidget-webkit-mode-map
    :nv "j"      #'xwidget-webkit-scroll-up ;; main object is scroller
    :nv "k"      #'xwidget-webkit-scroll-down
    :nv "gg"     #'xwidget-webkit-scroll-top
    :nv "G"      #'xwidget-webkit-scroll-bottom
    :nv "H"      #'xwidget-webkit-back
    :nv "L"      #'xwidget-webkit-forward
    :nv "s-+"    #'xwidget-webkit-zoom-in
    :nv "s--"    #'xwidget-webkit-zoom-out
    :nv "s-r"    #'xwidget-webkit-reload
    :nv "o"      #'browse-url)
  (:after dired :map dired-mode-map
    :nv "O"      #'dired-display-file
    :nv "o"      #'dired-find-file-other-window
    :nv "w"      #'dired-kill-subdir)
  (:map rspec-mode-map
    "<f5>"       #'rspec-verify-single)
  (:map rspec-compilation-mode-map
    :inv "C-l"   #'evil-window-right
    :inv "C-h"   #'evil-window-left
    :inv "C-j"   #'evil-window-down
    :inv "C-k"   #'evil-window-up)
  (:map magit-status-mode-map
    :n "`"       #'forge-browse-topic)
  (:map magit-revision-mode-map
    :n "o"       #'browse-at-remote
    :n "`"       #'forge-browse-topic)
  (:map magit-log-mode-map
    :n "o"       #'browse-at-remote
    :n "`"       #'forge-browse-topic)
  (:after ivy :map ivy-switch-buffer-map
    "C-v" (general-simulate-key "M-o a v <return>")
    "C-s" (general-simulate-key "M-o a s <return>"))
  (:after ivy :map ivy-minibuffer-map
    "C-v" (general-simulate-key "M-o a v <return>")
    "C-s" (general-simulate-key "M-o a s <return>")))

(map! :localleader
  (:map image-mode-map
    :desc "Copy image to clipboard" :nv "y"    #'sl/copy-image-file-to-clipboard)
  (:map nov-mode-map
    "t" #'google-translate-at-point
    "n" #'nov-next-document
    "p" #'nov-previous-document)
  (:after org :map org-mode-map
    "t" #'google-translate-at-point)
  (:map org-tree-slide-mode-map
    "e" #'org-tree-slide-slide-in-effect-toggle
    "p" #'org-tree-slide-move-previous-tree
    "n" #'org-tree-slide-move-next-tree
    "c" #'org-tree-slide-content
    "q" #'org-tree-slide-mode)
  (:map ruby-mode-map
    "tv"  nil
    "ts"  nil
    "tb"  #'rspec-verify)
  (:map rspec-mode-map
    "tv"  nil
    "ts"  nil
    "tt"  #'rspec-verify-single
    "tb"  #'rspec-verify)
  (:map forge-post-mode-map
    :n "," #'sl/make-draft-pr)
  (:after anki-editor :map org-mode-map
    "Mi" #'anki-editor-insert-note
    "Mp" #'anki-editor-push-notes
    "Mr" #'anki-editor-retry-failure-notes)
  (:after org :map org-mode-map
    "'"          #'org-edit-special
    ","          #'org-babel-execute-src-block
    "K"          #'org-shiftup
    "J"          #'org-shiftdown
    "H"          #'org-shiftleft
    "L"          #'org-shiftright
    "cp"         #'org-pomodoro
    "cs"         #'org-gcal-sync
    (:prefix-map ("d" . "date")
      "T"         #'org-time-stamp-inactive
      "d"         #'org-deadline
      "s"         #'org-schedule
      "t"         #'org-time-stamp)
    ;; (:prefix-map ("c" . "clock")
    ;;   "i"         #'org-clock-in
    ;;   "o"         #'org-clock-out
    ;;   "g"         #'org-clock-goto
    ;;   "R"         #'org-clock-report
    ;;   "c"         #'org-clock-cancel
    ;;   "d"         #'org-clock-display
    ;;   "p"         #'org-pomodoro)
    (:prefix-map ("b" . "babel")
      "a"         #'org-babel-sha1-hash
      "b"         #'org-babel-execute-buffer
      "c"         #'org-babel-check-src-block
      "d"         #'org-babel-demarcate-block
      "e"         #'org-babel-execute-maybe
      "f"         #'org-babel-tangle-file
      "g"         #'org-babel-goto-named-src-block
      "i"         #'org-babel-lob-ingest
      "I"         #'org-babel-view-src-block-info
      "j"         #'org-babel-insert-header-arg
      "l"         #'org-babel-load-in-session
      "n"         #'org-babel-next-src-block
      "o"         #'org-babel-open-src-block-result
      "p"         #'org-babel-previous-src-block
      "r"         #'org-babel-goto-named-result
      "S"         #'org-babel-execute-subtree
      "t"         #'org-babel-tangle
      "u"         #'org-babel-goto-src-block-head
      "v"         #'org-babel-expand-src-block
      "x"         #'org-babel-do-key-sequence-in-edit-buffer
      "z"         #'org-babel-switch-to-session
      "Z"         #'org-babel-switch-to-session-with-code
      "k"         #'org-babel-remove-result-one-or-many)
    (:prefix-map ("m" . "modes")
      "a" #'anki-editor-mode)))

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
