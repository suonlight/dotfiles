{
  :_:
  (fn [name ...]
    `((. nvim.ex ,(tostring name)) ,...))

  :autocmd
  (fn [...]
    `(nvim.ex.autocmd ,...))
}
