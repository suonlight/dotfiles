;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
;(package! pinned-package :pin nil)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
(package! evil-snipe :disable t)
(package! evil-easymotion :disable t)
;; (package! evil-goggles :disable t)
;; (package! evil-escape :disable t)
;; (package! centered-window :disable t) ;; depends by org presentation
;; (package! rjsx-mode :disable t)
;; (package! js2-mode :disable t)
;; (package! gitconfig-mode :disable t)
;; (package! gitignore-mode :disable t)
;; (package! csv-mode :disable t)
;; (package! jsonnet :disable t)
(package! dhall-mode :disable t)
(package! coffee-mode :disable t)
;; (package! npm-mode :disable t)
;; (package! skewer-mode :disable t)
(package! yard-mode :disable t)
(package! pug-mode :disable t)
(package! slim-mode :disable t)
(package! sass-mode :disable t)
(package! stylus-mode :disable t)
(package! sws-mode :disable t)
;; (package! rainbow-mode :disable t) ;; used by org-roam-server
;; (package! js2-refactor :disable t)
(package! eslintd-fix :disable t)
(package! flycheck-plantuml :disable t)
;; (package! flycheck-cask :disable t)
(package! edit-indirect :disable t)
(package! markdown-doc :disable t)
(package! bundler :disable t)
(package! rake :disable t)
(package! robe :disable t)
;; (package! tide :disable t)
;; (package! xref-js2 :disable t)
;; (package! nodejs-repl :disable t)
(package! minitest :disable t)
(package! counsel-css :disable t)
;; (package! anzu :disable t)
;; (package! evil-anzu :disable t)
;; (package! ibuffer-vc :disable t)
(package! dired-git-info :disable t)
(package! fd-dired :disable t)
(package! buttercup :disable t)
(package! quickrun :disable t)
;; (package! eros :disable t)

;; (package! orgit :disable t)

(package! doom-snippets :ignore t)

;; (package! magit-todos :disable t)
;; (package! magit-gitflow :disable t)

(package! prettier-js)
(package! indent-guide)
(package! evil-matchit)
(package! google-translate)
(package! copy-as-format)

(package! protobuf-mode)
(package! evil-string-inflection)

(package! multi-vterm)
(package! read-aloud)
(package! websocket)
(package! eacl :recipe (:host github :repo "redguardtoo/eacl" :files ("*.el" "out")))
;; (package! nano-theme :recipe (:host github :repo "rougier/nano-theme"))
;; (package! nano-modeline :recipe (:host github :repo "rougier/nano-modeline"))

;; use in case
;; (package! ob-mermaid)
(package! anki-editor)
;; (package! reason-mode)

;; (package! package-lint)
;; (package! clip2org)

;; (package! grammarly)
;; (package! flycheck-grammarly)
