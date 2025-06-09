;; [nfnl-macro]

(fn use-package! [name ...]
  (let [opts [...]
        new-opts {}]
    (for [i 1 (length opts) 2]
      (let [k (. opts i)
            v (. opts (+ i 1))]
        (tset new-opts k v)))
    (let [final-opts (if (> (length opts) 0) new-opts {})]
      `(util.use-package ,name ,final-opts))))

(fn use-package-setup! [] `(util.use-package-setup))

(fn defer [timeout func]
  `(vim.defer_fn ,func ,timeout))

(fn autocmd [event opts]
  `((. vim.api :nvim_create_autocmd) ,event ,opts))

(fn augroup [name ...]
  `(let [group# (vim.api.nvim_create_augroup ,(tostring name) {:clear true})]
     (tset vim.g :current_augroup group#)
     ,...
     (tset vim.g :current_augroup nil)))

(fn noremap [mode from to ?opts]
  (let [opts# (or ?opts {})]
    (tset opts# :noremap true)
    `(vim.keymap.set ,mode ,from ,to ,opts#)))

(fn inoremap [from to ?opts]
  `(noremap :i ,from ,to ,?opts))

(fn map [mode from to ?opts]
  (let [opts# (or ?opts {})]
    (tset opts# :noremap false)
    `(vim.keymap.set ,mode ,from ,to ,opts#)))

(fn nmap [from to ?opts]
  `(map :n ,from ,to ,?opts))

(fn imap [from to ?opts]
  `(map :i ,from ,to ,?opts))

(fn noremap-buffer [buffer mode from to ?opts]
  (let [opts# (or ?opts {})]
    (tset opts# :noremap true)
    (tset opts# :buffer buffer)
    `(vim.keymap.set ,mode ,from ,to ,opts#)))

(fn ex [...]
  `(vim.cmd ,...))

{
 : use-package!
 : use-package-setup!
 : defer
 : ex
 : autocmd
 : augroup
 : noremap
 : inoremap
 : map
 : nmap
 : imap
 : noremap-buffer
}
