{
 ;; Tells nfnl to find all .fnl files inside the 'fnl' directory.
 :source-file-patterns ["fnl/**/*.fnl"]

 ;; Tells nfnl to output a .lua file in the 'lua' directory
 ;; that matches the source file's path.
 ;; e.g., fnl/dotfiles/init.fnl -> lua/dotfiles/init.lua
 :target-file-patterns ["lua/?.lua"]

 ;; Tells nfnl where to look for your macros.fnl file.
 :macro-path "fnl/**/macros.fnl"
}
