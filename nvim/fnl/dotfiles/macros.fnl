{
 :_:
 (fn [name ...]
   `((. nvim.ex ,(tostring name)) ,...))

 :autocmd
 (fn [...]
   `(nvim.ex.autocmd ,...))

 :noremap
 (fn [mode from to]
   `(nvim.set_keymap ,mode ,from ,to {:noremap true}))

 :nmap
 (fn [from to]
   `(noremap :n ,from ,to))

 :noremap-buffer
 (fn [buffer mode from to]
   `(nvim.buf_set_keymap ,buffer ,mode ,from ,to {:noremap true}))
 }
