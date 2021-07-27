(module dotfiles.util
  {autoload {nvim aniseed.nvim
             a aniseed.core
             packer packer}})

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

