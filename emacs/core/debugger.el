(defun hero/ruby-test-smart-run ()
  (if dap-mode
    (dap-ruby-run-test-at-point)
    (ruby-test-run-at-point)))

(defun hero/ruby-server-smart-run ()
  (if dap-mode
    (dap-ruby-run-rackup)))

(defun dap-ruby-smart-run ()
  "Run Ruby test at point or Rackup"
  (interactive)
  (if (ruby-test-implementation-p)
    (hero/ruby-server-smart-run)
    (hero/ruby-test-smart-run)))

(use-package dap-mode
  :commands (dap-ruby-run-test-at-point dap-ruby-run-rackup dap-mode)
  :config
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  ;; (tooltip-mode 1)

  (defun hero/ruby-version ()
    (let ((version-file-path (f-join (projectile-project-root) ".ruby-version")))
      (s-replace "ruby-" ""
		 (-> version-file-path
		     f-read
		     s-trim))))

  (defun hero/ruby-path-for (program)
    (expand-file-name
     (format "~/.asdf/installs/ruby/%s/bin/%s" (hero/ruby-version) program)))

  (defun hero/rspec-program ()
    (if (f-exists? (f-join (projectile-project-root) "bin/rspec"))
	"bin/rspec"
      (expand-file-name
       (format "~/.asdf/installs/ruby/%s/bin/rspec" (hero/ruby-version)))))

  (defun hero/rackup-path ()
    (expand-file-name
     (format "~/.asdf/installs/ruby/%s/bin/rackup" (hero/ruby-version))))

  (defun dap-ruby--populate-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
	(dap--put-if-absent :dap-server-path dap-ruby-debug-program)
	(dap--put-if-absent :type "Ruby")
	(dap--put-if-absent :debuggerPort 1234)
	(dap--put-if-absent :cwd (projectile-project-root))
	(dap--put-if-absent :program (buffer-file-name))
	(dap--put-if-absent :name "Ruby Debug")))

  (defun dap-ruby-run-rackup ()
    (interactive)
    (let ((debug-args (list :type "Ruby"
			    :request "launch"
			    :program (hero/ruby-path-for "rackup")
			    :debuggerPort 23000
			    :args '()
			    :name "Rackup")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
				    (gethash dap--debug-providers)
				    (funcall debug-args)))))

  (defun dap-ruby-run-rails-s ()
    (interactive)
    (let ((debug-args (list :type "Ruby"
			    :request "launch"
			    :program "bin/rails"
			    :args '("s")
			    :debuggerPort 23000
			    :name "Rails Server")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
				    (gethash dap--debug-providers)
				    (funcall debug-args)))))


  (defun dap-ruby-attach-rails-s ()
    (interactive)
    (let ((debug-args (list :type "Ruby"
			    :request "attach"
			    :program "bin/rails"
			    :args '("s")
			    :debuggerPort 13000
			    :wait-for-port t
			    :port 3000
			    :host "localhost"
			    :hostName "localhost"
			    :name "Attach Rails Server")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
				    (gethash dap--debug-providers)
				    (funcall debug-args)))))

  (defun dap-ruby-attach-rackup ()
    (interactive)
    (let ((debug-args (list :type "Ruby"
			    :request "attach"
			    ;; :program "rackup"
			    ;; :program-to-start "rackup"
			    ;; :args '()
			    ;; :debuggerPort 13000
			    :remotePort "1234"
			    :remoteHost "127.0.0.1"
			    :remoteWorkspaceRoot (projectile-project-root)
			    :preLaunchTask "start-debug"
			    :wait-for-port t
			    :port 9292
			    :host "localhost"
			    :hostName "localhost"
			    :name "Attach Sinatra Server")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
				    (gethash dap--debug-providers)
				    (funcall debug-args)))))


  (defun dap-ruby-run-test-at-point ()
    "Run Ruby test at point"
    (interactive)
    (let ((debug-args (list :type "Ruby"
			    :request "launch"
			    ;; :program (rbenv-rspec-program)
			    :program (hero/rspec-program)
			    :args `(,(copy-file-path-with-line))
			    :environment-variables '(("DISABLE_SPRING" . "true"))
			    :name "Rspec File At Point")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
				    (gethash dap--debug-providers)
				    (funcall debug-args)))))

  (defun dap-ruby-run-test ()
    "Run Ruby test file"
    (interactive)
    (let ((debug-args (list :type "Ruby"
			    :request "launch"
			    :program (hero/ruby-path-for "rspec")
			    :environment-variables '(("DISABLE_SPRING" . "true"))
			    :args `(,buffer-file-name)
			    :name "Rspec File")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
				    (gethash dap--debug-providers)
				    (funcall debug-args)))))

  (setq dap-ruby-debug-program `("node" ,(expand-file-name "~/.emacs.d/.extension/vscode/rebornix.Ruby/extension/out/debugger/main.js")))
  (dap-register-debug-provider "Ruby" 'dap-ruby--populate-start-file-args)

  (defun dap-node-run-test ()
    "Run JUnit test.
					If there is no method under cursor it will fallback to test class."
    (interactive)
    (require 'dap-node)
    (let ((debug-args (list :type "node"
			    :request "launch"
			    :environment-variables '(("NODE_PATH" . "src/packages")
						     ("REACT_APP_MOCK_DATA" . "true")
						     ("CI" . "true"))
			    :program (format "%s/node_modules/eh-react-scripts/bin/react-scripts.js" (projectile-project-root))
			    :protocol "inspector"
			    ;; :console "integratedTerminal"
			    ;; :internalConsoleOptions "neverOpen"
			    :cwd (projectile-project-root)
			    ;; :args `("run" "eh-react-scripts" "test"
			    :args `("test"
				    ;; "--inspect-brk" "24066"
				    "--env=jsdom" "--runInBand"
				    "--no-cache")
			    ;; "--watchAll=false"
			    ;; "-u" ,buffer-file-name)
			    :name "Debug CRA Tests")))
      (dap-start-debugging (-some-> (plist-get debug-args :type)
				    (gethash dap--debug-providers)
				    (funcall debug-args)))))

  (leader-define-key js-mode-map
    "d"    #'(:ignore t :which-key "debugger")
    "dn"   #'dap-next
    "di"   #'dap-step-in
    "do"   #'dap-step-out
    "dc"   #'dap-continue
    "dR"   #'dap-ui-repl
    "dr"   #'dap-restart-frame
    "ds"   #'(:ignore t :which-key "switch")
    "dss"  #'dap-switch-session
    "dst"  #'dap-switch-thread
    "dsf"  #'dap-switch-stack-frame
    "dsl"  #'dap-ui-locals
    "dsb"  #'dap-ui-breakpoints
    "dsS"  #'dap-ui-sessions
    "db"   #'(:ignore t :which-key "breakpoints")
    "dbt"  #'dap-breakpoint-toggle
    "dba"  #'dap-breakpoint-add
    "dbd"  #'dap-breakpoint-delete
    "dbc"  #'dap-breakpoint-condition
    "dbh"  #'dap-breakpoint-hit-condition
    "dbl"  #'dap-breakpoint-log-message
    "de"   #'(:ignore t #':which-key "eval")
    "dee"  #'dap-eval
    "der"  #'dap-eval-region
    "des"  #'dap-eval-thing-at-point
    "deii" #'dap-ui-inspect
    "deir" #'dap-ui-inspect-region
    "deis" #'dap-ui-inspect-thing-at-point
    "d."   #'dap-hydra
    "dt"   #'(:ignore t :which-key "test")
    "dtt"  #'dap-ruby-run-test-at-point
    "dtb"  #'dap-ruby-run-test
    "dd"   #'dap-debug)

  (general-define-key
   :states '(normal visual emacs)
   ;; :keymaps 'js2-mode-map
   :keymaps 'js-mode-map
   "<f5>"       #'dap-node-run-test)

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'dap-mode-map
   "M-<f8>"       #'dap-eval
   "<f8>"         #'dap-next
   "<f7>"         #'dap-step-in
   "S-<f7>"       #'dap-step-out
   "<f9>"         #'dap-continue
   "M-<f8>"       #'dap-eval
   "s-S-<f8>"     #'dap-ui-breakpoints
   "s-."          #'dap-hydra
   "s-<f8>"       #'dap-breakpoint-toggle)

  (leader-define-key ruby-mode-map
    "d"    #'(:ignore t :which-key "debugger")
    "dn"   #'dap-next
    "di"   #'dap-step-in
    "do"   #'dap-step-out
    "dc"   #'dap-continue
    "dR"   #'dap-ui-repl
    "dr"   #'dap-restart-frame
    "ds"   #'(:ignore t :which-key "switch")
    "dss"  #'dap-switch-session
    "dst"  #'dap-switch-thread
    "dsf"  #'dap-switch-stack-frame
    "dsl"  #'dap-ui-locals
    "dsb"  #'dap-ui-breakpoints
    "dsS"  #'dap-ui-sessions
    "db"   #'(:ignore t :which-key "breakpoints")
    "dbt"  #'dap-breakpoint-toggle
    "dba"  #'dap-breakpoint-add
    "dbd"  #'dap-breakpoint-delete
    "dbc"  #'dap-breakpoint-condition
    "dbh"  #'dap-breakpoint-hit-condition
    "dbl"  #'dap-breakpoint-log-message
    "de"   #'(:ignore t #':which-key "eval")
    "dee"  #'dap-eval
    "der"  #'dap-eval-region
    "des"  #'dap-eval-thing-at-point
    "deii" #'dap-ui-inspect
    "deir" #'dap-ui-inspect-region
    "deis" #'dap-ui-inspect-thing-at-point
    "d."   #'dap-hydra
    "dt"   #'(:ignore t :which-key "test")
    "dtt"  #'dap-ruby-run-test-at-point
    "dtb"  #'dap-ruby-run-test
    "dd"   #'dap-debug)

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'dap-ui-breakpoints-ui-list-mode-map
   "D" #'dap-ui-breakpoints-delete-selected
   "d" #'dap-ui-breakpoints-delete
   "RET" #'dap-ui-breakpoints-goto
   "q" #'quit-window))
