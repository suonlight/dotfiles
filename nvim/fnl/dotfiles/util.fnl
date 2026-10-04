;; Localize modules and frequently used APIs
;; (local {:nvim_command :nvim_win_get_cursor :nvim_buf_set_text :nvim_win_get_buf} vim.api)
;; (local fn vim.fn)
(local bo vim.bo)

;; Helper function to replace aniseed.string.split
;; (fn split [input sep]
;;   (when (= sep nil) (set sep "%s"))
;;   (let [fields []]
;;     (var pattern (.. "[^" sep "]+"))
;;     (each [field (string.gmatch input pattern)]
;;       (table.insert fields field))
;;     fields))

;; --- Utility Functions ---

(fn expand [path]
  (vim.fn.expand path))

(fn glob [path]
  (vim.fn.glob path true true))

(fn exists? [path]
  (= (vim.fn.filereadable path) 1))

(fn lua-file [path]
  (nvim_command (.. "luafile " path)))

;; (local config-path (fn.stdpath "config"))

(fn filename []
  (expand "%"))

(fn readonly []
  (if (and bo.readonly (not= bo.filetype "help"))
    "RO"
    ""))

(fn safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :dotfiles.plugin. name))]
    (when (not ok?)
      (print (.. "dotfiles error: " val-or-err)))))

;; --- Package Management (for use-package! macro) ---

(local packages [])

(fn use-package [name opts]
  (tset opts 1 name)
  (table.insert packages opts))

(fn use-package-setup []
  (let [lazy (require :lazy)]
    (lazy.setup {:spec packages })))

;; --- Command Functions ---

(fn sh [cmd]
  (vim.fn.systemlist cmd))

(fn gh-open-pull-request []
  (sh "gh pr view --web")
  (print "Opened PR in browser."))

(fn gh-list-pull-requests []
  (local fzf-core (require :fzf-lua.core))
  (let [on-select (fn [choice _]
                    (let [pr-line (. choice 1)
                          pr-number-str (-> (split pr-line "#") (. 2) (split "%s") (. 1))]
                      (sh (.. "gh pr view " pr-number-str " --web"))))
        cmd "gh pr list --search sort:updated-desc --json author,title,number,isDraft --jq '.[] | [\"#\" + (.number|tostring), .author.login, .title + \" \" + (if .isDraft then \"[draft]\" else \"[open]\" end)] | join(\" - \")'"]
    (fzf-core.fzf_exec (sh cmd) {:prompt "PR: " :actions {"default" on-select}})))

;; (fn js-insert-i18n []
;;   (let [cmd (.. "jq -r '.messages | [leaf_paths as $path | { \"key\": $path | join(\".\"), \"value\": getpath($path)}] | map([(.key + \": \" + .value)]) | .[] | .[]' " (expand "src/packages/eh-locale/lang/en-AU.json"))
;;         on-select (fn [choice]
;;                     (let [i18n-key (-> (. choice 1) (split ":") (. 1) (string.gsub "^%s*(.-)%s*$" "%1")) ; trim whitespace
;;                           pos (nvim_win_get_cursor 0)
;;                           row (- (. pos 1) 1)
;;                           col (. pos 2)
;;                           text (.. "Intl.formatMessage({ id: '" i18n-key "' })")]
;;                       (nvim_buf_set_text (nvim_win_get_buf 0) row col row col [text])))]
;;     (coroutine.wrap (fn []
;;                       (let [choice (fzf-core.fzf cmd {:border false})]
;;                         (when choice (on-select choice)))))()))

(fn ci-open []
  (let [org-repo (-> (sh "git remote get-url origin") (. 1)
                     (split ":") (. 2)
                     (string.gsub ".git$" "")) ; anchor replacement to the end
        branch (-> (sh "git rev-parse --abbrev-ref HEAD") (. 1)
                   (string.gsub "/" "%%2F"))
        ci-url (.. "https://app.circleci.com/pipelines/github/" org-repo "?branch=" branch)]
    (sh (.. "open \"" ci-url "\""))))

(local org-roam-directory (expand "~/notes/roam"))

(fn string-trim-quote [s]
  (string.gsub s "\"(.+)\"" "%1"))

(fn org-roam-dailies-find-today []
  (let [file (.. org-roam-directory "/journals/" (os.date "%Y-%m-%d.org"))]
    (nvim_command (.. "edit " file))))

(fn org-roam-dailies-find-yesterday []
  (let [yesterday (os.date "%Y-%m-%d.org" (- (os.time) (* 24 60 60)))
        file (.. org-roam-directory "/journals/" yesterday)]
    (nvim_command (.. "edit " file))))

(fn org-roam-dailies-find-tomorrow []
  (let [tomorrow (os.date "%Y-%m-%d.org" (+ (os.time) (* 24 60 60)))
        file (.. org-roam-directory "/journals/" tomorrow)]
    (nvim_command (.. "edit " file))))

(fn org-roam-find-file []
  (let [db-path (expand "~/.config/org-roam.db")
        sqlite (require :sqlite)
        db (sqlite {:uri db-path :nodes {:objectives "luatable" :id true :title {:type "string"} :file {:type "string"}}})
        all-nodes (db.nodes:get)
        node-titles (icollect [_ node (ipairs all-nodes)] (string-trim-quote (. node :title)))
        get-node (fn [title]
                   (let [results (db.nodes:get {:where {:title (string.format "%q" title)}})]
                     (when (> (length results) 0)
                       (. results 1))))
        on-select (fn [choice _]
                    (let [title (. choice 1)
                          node (get-node title)]
                      (when node
                        (let [file (string-trim-quote (. node :file))]
                          (nvim_command (.. "edit " file))))))]
    (fzf-core.fzf_exec node-titles {:prompt "Node: " :actions {"default" on-select}})))


;; --- EXPORT PUBLIC FUNCTIONS ---
{
 : expand
 : glob
 : exists?
 : lua-file
 : config-path
 : filename
 : readonly
 : safe-require-plugin-config
 : use-package
 : use-package-setup
 : sh
 : gh-open-pull-request
 : gh-list-pull-requests
 : ci-open
 : org-roam-dailies-find-today
 : org-roam-dailies-find-yesterday
 : org-roam-dailies-find-tomorrow
 : org-roam-find-file
}
