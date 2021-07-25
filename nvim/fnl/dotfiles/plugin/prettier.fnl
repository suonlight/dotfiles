(module dotfiles.plugin.prettier
  {autoload {nvim aniseed.nvim}
   require-macros [dotfiles.macros]})

(set nvim.g.prettier#autoformat 1)

(autocmd
  :BufWritePre
  "*.js,*.jsx,*.mjs,*.ts,*.tsx,*.less,*.json,*.graphql,*.md,*.vue"
  "Prettier")

; (vim.schedule
;   (fn []
;     ))
