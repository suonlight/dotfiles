(module dotfiles.plugin.prettier
  {autoload {nvim aniseed.nvim}
   require-macros [dotfiles.macros]})

(vim.schedule
  (fn []
    (autocmd
      :BufWritePre
      "*.js,*.jsx,*.mjs,*.ts,*.tsx,*.less,*.json,*.graphql,*.md,*.vue"
      "Prettier")))
