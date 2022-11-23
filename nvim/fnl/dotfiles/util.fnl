(module dotfiles.util
  {autoload {nvim aniseed.nvim
             strings aniseed.string
             a aniseed.core
             packer packer
             fzf-core fzf-lua.core}
   require-macros [dotfiles.macros]})

(defn expand [path]
  (nvim.fn.expand path))

(defn glob [path]
  (nvim.fn.glob path true true true))

(defn exists? [path]
  (= (nvim.fn.filereadable path) 1))

(defn lua-file [path]
  (nvim.ex.luafile path))

(def config-path (nvim.fn.stdpath "config"))

(defn filename []
  (expand "%"))

(defn readonly []
  (if (and nvim.bo.readonly
           (not= nvim.bo.filetype "help"))
    "RO"
    ""))

(defn safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :dotfiles.plugin. name))]
    (when (not ok?)
      (print (.. "dotfiles error: " val-or-err)))))

(defn use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup
      (fn [use]
        (for [i 1 (a.count pkgs) 2]
          (let [name (. pkgs i)
                opts (. pkgs (+ i 1))]
            (-?> (. opts :mod) (safe-require-plugin-config))
            (use (a.assoc opts 1 name))))))))

(defn gh-open-pull-request []
  (sh "gh pr view --web")
  (nvim.input "<CR>"))

(defn gh-list-pull-requests []
  (let [on-select (fn [choice]
                    (let [pr-number (-> choice
                                        (. 1)
                                        (strings.split " - ") (. 1)
                                        (strings.split "#") (. 2))]
                      (sh (.. "gh pr view " pr-number " --web"))))
        cmd "gh pr list --search sort:updated-desc --json author,title,number,isDraft --jq '.[] | [\"#\" + (.number|tostring), .author.login, .title + \" \" + (if .isDraft then \"[draft]\" else \"[open]\" end)] | join(\" - \")'"]
    (coroutine.wrap (fn []
                      (let [choice (fzf-core.fzf cmd {:border false})]
                        (when choice (on-select choice)))))))

(defn js-insert-i18n []
  (let [cmd (.. "jq -r '.messages | [leaf_paths as $path | { \"key\": $path | join(\".\"), \"value\": getpath($path)}] | map([(.key + \": \" + .value)]) | .[] | .[]' " (expand "src/packages/eh-locale/lang/en-AU.json"))
        on-select (fn [choice]
                    (let [i18n-key (-> choice
                                       (. 1)
                                       (strings.split ":") (. 1)
                                       strings.trim)]
                      (let [pos (nvim.win_get_cursor 0)
                            row (-> (. pos 1) (- 1))
                            col (. pos 2)
                            text (.. "Intl.formatMessage({ id: '" i18n-key "' })")]
                        (nvim.buf_set_text (nvim.win_get_buf 0) row col row col [text]))))]
    ((coroutine.wrap (fn []
                       (let [choice (fzf-core.fzf cmd {:border false})]
                         (when choice (on-select choice))))))))

(defn ci-open []
  (let [org-repo (-> (sh "git remote get-url origin") (. 1)
                     (strings.split ":") (. 2)
                     (string.gsub ".git" ""))
        branch (-> (sh "git rev-parse --abbrev-ref HEAD") (. 1)
                   (string.gsub "/" "%%2F"))
        ci-url (.. "https://app.circleci.com/pipelines/github/" org-repo "?branch=" branch)]
    (sh (.. "open \"" ci-url "\""))))

(defn org-roam-dailies-find-today []
  (let [file (.. "~/org-modes/roam/journals/" (os.date "%Y-%m-%d.org"))]
    (nvim.ex.edit file)))
