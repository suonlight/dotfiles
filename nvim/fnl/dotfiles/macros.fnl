{
  :cmd
  (fn [name ...]
    `((. nvim.ex ,(tostring name)) ,...))

  :sh
  (fn [...]
    `(vim.fn.systemlist ,...))

  :autocmd
  (fn [...]
    `(nvim.ex.autocmd ,...))

  :augroup
  (fn [name ...]
    `(do
       (nvim.ex.augroup ,(tostring name))
       (nvim.ex.autocmd_)
       ,(list `do ...)
       (nvim.ex.augroup :END)))

  :noremap
  (fn [mode from to ?opts]
    (if ?opts
      (do (set ?opts.noremap true)
        `(nvim.set_keymap ,mode ,from ,to ,?opts))
      `(nvim.set_keymap ,mode ,from ,to {:noremap true})))

  :inoremap
  (fn [from to ?opts]
    `(noremap :i ,from ,to ,?opts))

  :map
  (fn [mode from to ?opts]
    (if ?opts
      (do (set ?opts.noremap false)
        `(nvim.set_keymap ,mode ,from ,to ,?opts))
      `(nvim.set_keymap ,mode ,from ,to {:noremap false})))

  :nmap
  (fn [from to ?opts]
    `(map :n ,from ,to ,?opts))

  :imap
  (fn [from to ?opts]
    `(map :i ,from ,to ,?opts))

  :noremap-buffer
  (fn [buffer mode from to ?opts]
    (if ?opts
      (do (set ?opts.noremap true)
        `(nvim.buf_set_keymap ,buffer ,mode ,from ,to ,?opts))
      `(nvim.buf_set_keymap ,buffer ,mode ,from ,to {:noremap true})))

  :defer
  (fn
    [timeout func]
    `(vim.defer_fn ,func ,timeout))

  :fn->viml
  (fn [mod from to]
    `(nvim-util.fn-bridge ,to ,mod ,from {:return true}))
}
