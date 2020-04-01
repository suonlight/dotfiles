call plug#begin('~/.local/share/nvim/plugged')

" Core plugins
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'christoomey/vim-tmux-navigator'
Plug 'benmills/vimux'
Plug 'ludovicchabant/vim-gutentags'
Plug 'neoclide/coc-neco'
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'szw/vim-maximizer'
Plug 'easymotion/vim-easymotion'
Plug 'matze/vim-move'

Plug 'danro/rename.vim'

Plug 'mhinz/vim-startify'
Plug 'itchyny/lightline.vim'
" Plug 'ryanoasis/vim-devicons'
Plug 'Yggdroot/indentLine'
Plug 'joshdick/onedark.vim'

" Plug 'SirVer/ultisnips'
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'honza/vim-snippets'

Plug 'tpope/vim-surround'                                        " Easily change Surround characters
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-entire'
Plug 'gaving/vim-textobj-argument'
Plug 'coderifous/textobj-word-column.vim'

Plug 'ntpeters/vim-better-whitespace'
Plug 'alvan/vim-closetag'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
Plug 'dense-analysis/ale'
Plug 'brooth/far.vim'
Plug 'janko-m/vim-test'

" Plug 'xolox/vim-session'
Plug 'xolox/vim-misc'

"----------------------------------------------
" VCS git
"----------------------------------------------
Plug 'tpope/vim-fugitive'                                        " Easily manipulate Git(hub)
Plug 'tpope/vim-rhubarb'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-projectionist'
" Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary' }

"----------------------------------------------
" Language: Ruby
"----------------------------------------------
" Plug 'vim-ruby/vim-ruby'
" Plug 'tpope/vim-rails'
" Plug 'hackhowtofaq/vim-solargraph'

"----------------------------------------------
" Language: JavaScript
"----------------------------------------------
" Plug 'pangloss/vim-javascript', { 'for': 'javascript' }          " vim-jsx
" Plug 'mxw/vim-jsx', { 'for': 'javascript' }
" Plug 'ternjs/tern_for_vim', { 'for': 'javascript', 'do': 'npm install' }
" Plug 'epilande/vim-es2015-snippets', { 'for': 'javascript' }     " ES2015 code snippets (Optional)
" Plug 'epilande/vim-react-snippets', { 'for': 'javascript' }      " React code snippets
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'mattn/emmet-vim', { 'for': ['html', 'erb', 'javascript'] } " emmet-vim
Plug 'editorconfig/editorconfig-vim'
" Plug 'kchmck/vim-coffee-script'

call plug#end()

"----------------------------------------------
" General settings
"----------------------------------------------
filetype plugin indent on
set nocompatible
set autoindent                    " take indent for new line from previous line
set smartindent                   " enable smart indentation
set autoread
set clipboard=unnamed
set expandtab                     " expands tabs to spaces
set softtabstop=2
set tabstop=2
set shiftwidth=2
set ignorecase
set smartcase
set encoding=UTF-8
set nowrap

syntax enable

let g:strip_whitespace_on_save = 1
set ttimeout
set ttimeoutlen=0

"----------------------------------------------
" Layer: Themes
"----------------------------------------------

set background=dark
" colorscheme gruvbox
if (has("termguicolors"))
  set termguicolors
endif
colorscheme onedark

" Show status bar by default.
set laststatus=2
if !has('gui_running')
  set t_Co=256
endif

" " Set this. Airline will handle the rest.
" let g:airline_theme = 'luna'

" " Enable top tabline.
" let g:airline#extensions#tabline#enabled = 1

" " Disable showing tabs in the tabline. This will ensure that the buffers are
" " what is shown in the tabline at all times.
" let g:airline#extensions#tabline#show_tabs = 0

" " Show only file name in tabline
" let g:airline#extensions#tabline#fnamemod = ':t'

" " Show buffer index next to file name
" let g:airline#extensions#tabline#buffer_nr_show = 1

" " Enable powerline fonts.
" let g:airline_powerline_fonts = 1

" " Advanced separators (extra-powerline-symbols):
" let g:airline_left_sep = "\uE0B4"
" let g:airline_right_sep = "\uE0B6"

"----------------------------------------------
" Searching
"----------------------------------------------
set incsearch                     " move to match as you type the search query
set hlsearch                      " disable search result highlighting

if has('nvim')
  set inccommand=split          " enables interactive search and replace
endif

" Clear search highlights
nnoremap <Esc> :noh<CR><Esc>

" These mappings will make it so that going to the next one in a search will
" center on the line it's found in.
nnoremap n nzzzv
nnoremap N Nzzzv

"----------------------------------------------
" Navigation
"----------------------------------------------

" Disable arrow keys
" inoremap <Up> <nop>
" inoremap <Down> <nop>
" inoremap <Left> <nop>
" inoremap <Right> <nop>
" noremap <Up> <nop>
" noremap <Down> <nop>
" noremap <Left> <nop>
" noremap <Right> <nop>

"----------------------------------------------
" Plug 'kana/vim-textobj-entire'
"----------------------------------------------
call textobj#user#plugin('entire', {
\      '-': {
\        'select-a': 'ag',  'select-a-function': 'textobj#entire#select_a',
\        'select-i': 'ig',  'select-i-function': 'textobj#entire#select_i'
\      }
\    })

"----------------------------------------------
" Plug 'xolox/vim-session'
"----------------------------------------------
" let g:session_autoload = 'no'
" let g:session_autosave = 'no'

"----------------------------------------------
" Snippets
"----------------------------------------------

let g:neosnippet#enable_completed_snippet = 1
let g:neosnippet#enable_snipmate_compatibility = 1

" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-e>     <Plug>(neosnippet_expand_or_jump)
smap <C-e>     <Plug>(neosnippet_expand_or_jump)
xmap <C-e>     <Plug>(neosnippet_expand_target)

" cuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <expr><TAB>
      \ pumvisible() ? "\<C-n>" :
      \ neosnippet#expandable_or_jumpable() ?
      \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif

"----------------------------------------------
" Linter
"----------------------------------------------
" Enable completion where available.
let g:ale_completion_enabled = 1

" Set this variable to 1 to fix files when you save them.
let g:ale_lint_on_save = 1

let g:ale_fixers = {
      \   'javascript': ['prettier'],
      \   'json': ['jq'],
      \   'ruby': ['rubocop'],
      \}

let g:ale_linters = {
      \   'javascript': ['eslint'],
      \   'json': ['jsonlint'],
      \   'ruby': ['rubocop'],
      \}

let g:ale_lint_on_text_changed="never"
let g:ale_echo_cursor = 1
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_set_highlights = 0
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
highlight SignColumn guibg=255
"----------------------------------------------
" Plug 'Shougo/deoplete.nvim'
"----------------------------------------------
if has('nvim')
    " Enable deoplete on startup
    let g:deoplete#enable_at_startup = 1
endif
let g:python3_host_prog = "/usr/local/bin/python3"
let g:python2_host_prog = "/usr/local/bin/python2.7"

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"


"----------------------------------------------
" Plug 'Yggdroot/indentLine'
"----------------------------------------------
let g:indentLine_enabled = 0
let g:indentLine_concealcursor = 'inc'
let g:indentLine_conceallevel = 2
"----------------------------------------------
" Plug 'pangloss/vim-javascript'
"----------------------------------------------
let g:javascript_plugin_flow = 1

"----------------------------------------------
" Plug 'mxw/vim-jsx'
"----------------------------------------------
let g:jsx_ext_required = 0

"----------------------------------------------
" Plug 'prettier/vim-prettier'
"----------------------------------------------
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.less,*.json,*.graphql,*.md,*.vue Prettier

"----------------------------------------------
" Plug 'christoomey/vim-tmux-navigator'
"----------------------------------------------
" tmux will send xterm-style keys when its xterm-keys option is on.
if &term =~ '^screen'
  execute "set <xUp>=\e[1;*A"
  execute "set <xDown>=\e[1;*B"
  execute "set <xRight>=\e[1;*C"
  execute "set <xLeft>=\e[1;*D"
endif

" Tmux vim integration
let g:tmux_navigator_no_mappings = 1
let g:tmux_navigator_save_on_switch = 1

" only works on mac osx - Move between splits with ctrl+h,j,k,l
" set shell=/bin/bash\ -i
"----------------------------------------------
" Plug 'itchyny/lightline.vim'
"----------------------------------------------
function! LinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? '✓' : printf(
    \   'Linter %d🚸 %d⛔️',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

let g:lightline = {
      \ 'colorscheme': 'onedark',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified', 'alestatus'] ]
      \ },
      \ 'component_function': {
      \   'alestatus': 'LinterStatus'
      \ },
      \ }

"----------------------------------------------
" Plug 'scrooloose/nerdtree'
"----------------------------------------------
let g:NERDTreeSyntaxDisableDefaultExtensions = 1
let g:NERDTreeDisableExactMatchHighlight = 1
let g:NERDTreeDisablePatternMatchHighlight = 1
let g:NERDTreeSyntaxEnabledExtensions = ['rb', 'js', 'html', 'haml', 'css', 'erb', 'jsx', 'scss']
let g:NERDTreeLimitedSyntax = 1
let g:NERDTreeHighlightCursorline = 0

"----------------------------------------------
" Plug 'mhinz/vim-startify'
"----------------------------------------------
let g:startify_change_to_vcs_root = 1

"----------------------------------------------
" Plug 'ludovicchabant/vim-gutentags'
"----------------------------------------------
" let s:vim_tags = expand('~/.cache/tags')
" let g:gutentags_cache_dir = get(g:, 'gutentags_cache_dir', expand('~/.cache/tags'))
" let g:gutentags_enabled = 1

"----------------------------------------------
" Plug 'alvan/vim-closetag'
"----------------------------------------------
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.erb,*.jsx,*.js"
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.erb,*.js'
let g:closetag_emptyTags_caseSensitive = 1
" Add > at current position without closing the current tag, default is ''
let g:closetag_close_shortcut = '<leader>>'

let g:AutoPairsFlyMode = 0

"----------------------------------------------
" Plug 'mattn/emmet-vim'
"----------------------------------------------
" let g:user_emet_expandabbr_key='<Tab>'
let g:user_emmet_settings = {
      \  'javascript.jsx' : {
      \      'extends' : 'jsx',
      \      'block_all_childless' : 1,
      \      'quote_char': "'",
      \  },
      \  'html': {
      \      'block_all_childless' : 1,
      \      'quote_char': "'",
      \  },
      \}

" augroup Term
"   autocmd!
"   " Always start in terminal mode in term buffers
"   autocmd TermOpen * startinsert
"   autocmd BufEnter term://* startinsert
"   autocmd BufLeave term://* stopinsert
" augroup END

" escape from terminal mode to normal mode
tnoremap jk <C-\><C-n>

"----------------------------------------------
" Plug 'janko-m/vim-test'
"----------------------------------------------
let g:test#strategy = 'vimux'
let g:test#preserve_screen = 1

let test#enabled_runners = ["ruby#rspec"]
let test#ruby#minitest#file_pattern = '_spec\.rb'

"----------------------------------------------
" Plug 'junegunn/vim-easy-align'
"----------------------------------------------
let g:easy_align_delimiters = {
      \ '"': { 'pattern': '\s"', 'delimiter_align': 'l' }
      \ }

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
"----------------------------------------------
" Space Mapping
"----------------------------------------------
let mapleader = ","

" Help Mapping
nnoremap <silent> <Space>hdb :Maps<CR>
nnoremap <silent> <Space>hdf :Helptags<CR>  " Help > Describe > Tags Function
nnoremap <silent> <Space>qq :q<CR>          " Quit Program

" Windows
nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>

nnoremap <silent> <Space>0 :NERDTreeFind<CR>        " Tree > Select Window
nnoremap <silent> <Space>1 :exe 1 . 'wincmd w'<CR>  " Window 1
nnoremap <silent> <Space>2 :exe 2 . 'wincmd w'<CR>  " Window 2
nnoremap <silent> <Space>3 :exe 3 . 'wincmd w'<CR>  " Window 3
nnoremap <silent> <Space>4 :exe 4 . 'wincmd w'<CR>  " Window 4
nnoremap <silent> <Space>5 :exe 5 . 'wincmd w'<CR>  " Window 5
nnoremap <silent> <Space>6 :exe 6 . 'wincmd w'<CR>  " Window 6
nnoremap <silent> <Space>7 :exe 7 . 'wincmd w'<CR>  " Window 7
nnoremap <silent> <Space>8 :exe 8 . 'wincmd w'<CR>  " Window 8
nnoremap <silent> <Space>9 :exe 9 . 'wincmd w'<CR>  " Window 9
nnoremap <silent> <Space>wv :wincmd v<CR>           " Window Vertical
nnoremap <silent> <Space>ws :wincmd s<CR>           " Window Split
nnoremap <silent> <Space>w= :wincmd =<CR>           " Window Balance Area
nnoremap <silent> <Space>wl :wincmd l<CR>           " Window Left
nnoremap <silent> <Space>wh :wincmd h<CR>           " Window Right
nnoremap <silent> <Space>wj :wincmd j<CR>           " Window Down
nnoremap <silent> <Space>wk :wincmd k<CR>           " Window Up
nnoremap <silent> <Space>ww :wincmd w<CR>           " Other Window
nnoremap <silent> <Space>wr :wincmd r<CR>           " Rotate Window
nnoremap <silent> <Space>w<S-k> :wincmd <S-k><CR>   " Move Window Very Top
nnoremap <silent> <Space>w<S-j> :wincmd <S-j><CR>   " Move Window Very Bottom
nnoremap <silent> <Space>w<S-h> :wincmd <S-h><CR>   " Move Window Far Right
nnoremap <silent> <Space>w<S-l> :wincmd <S-l><CR>   " Move Window Far Left
nnoremap <silent> <Space>wc :close<CR>              " Window Close
nnoremap <silent> <Space><BS> :close<CR>              " Window Close
nnoremap <silent> <Space>wm :MaximizerToggle<CR>    " Window Maximize

" Files
nnoremap <Space>fs :update<CR>                              " File > Save
nnoremap <silent> <Space>ft :NERDTreeToggle<CR>                      " File > Tree
nnoremap <silent> <Space>fT :NERDTreeFind<CR>                        " File > Tree > Find
nnoremap <silent> <Space>fed :e ~/.config/nvim/init.vim<CR>          " File > Editor > Definition
nnoremap <silent> <Space>fez :e ~/.zshrc<CR>                         " File > Editor > ZSH
nnoremap <silent> <Space>fet :e ~/.tmux.conf<CR>                     " File > Editor > Tmux
nnoremap <Space>feR :source ~/.config/nvim/init.vim<CR>     " File > Editor > Reload
nnoremap <silent> <Space>ff :Files %:p:h<CR>|                        " File > Find File in Current Directory
nnoremap <silent> <Space>fr :History<CR>|                            " File > Find File in History
nnoremap <Space>fR :Rename<Space>|                                   " Rename File
nnoremap <Space>fc :saveas <C-R>=expand("%:p:h")<CR>/|               " Copy File
nnoremap <silent> <Space>fyY :let @*=expand("%") \| echo @*<CR>      " File > Copy Relative File Path
nnoremap <silent> <Space>fyy :let @*=expand("%:p") \| echo @*<CR>    " File > Copy Full File Path
nnoremap <silent> <Space>fyd :let @*=expand("%:p:h") \| echo @*<CR>  " File > Copy Directory Path

" Projects
nnoremap <silent> <Space>pf :FZF<CR>
nnoremap <silent> <Space>pa :A<CR>
nnoremap <Space>pG :GutentagsUpdate<CR>
nnoremap <silent> <Space>pg :Tags<CR>
nnoremap <silent> <C-p> :FZF<CR>

" Searching
nnoremap <silent> <Space>/ :Rg<CR>
nnoremap <silent> <Space>sp :Rg<CR>
nnoremap <silent> <Space>su :Rg<up><CR>
nnoremap <silent> <Space>ss :BLines<CR>
nnoremap <silent> <Space>* :Rg <C-R><C-W><CR>
nmap s <Plug>(easymotion-overwin-f)
map <silent> <Space>jj <Plug>(easymotion-s)
map <silent> <Space>jw <Plug>(easymotion-overwin-f)
" map <Space>jL <Plug>(easymotion-bd-jk)
nmap <Space>jl <Plug>(easymotion-overwin-line)
nnoremap <silent> <Space>ji :BTags<CR>
" nnoremap <silent> <Space>sh :call <SNR>62_Highlight("w") \| nohls<CR>
" nnoremap <silent> <Space>sc :call <SNR>62_Highlight("n")<CR>

" Buffers
nnoremap <silent> <Space>bb :Buffer<CR>|           " List Buffers
nnoremap <silent> <Space>bd :bdelete<CR>           " Delete Buffer
nnoremap <silent> <Space>bn :bnext<CR>             " Next Buffer
nnoremap <silent> <Space>bp :bprevious<CR>         " Previous Buffer
nnoremap <silent> <Space>bh :Startify<CR>          " Home Buffer
nnoremap <silent> <Space><tab>  <C-^>              " Last Buffer
nnoremap <silent> <Space>bx :e /tmp/scratch<CR>    " Scratch Buffer

" Errors
nnoremap <silent> ]e :ALENext<CR>      " Next Error
nnoremap <silent> [e :ALEPrevious<CR>      " Next Error
nnoremap <silent> <Space>ef :ALEFix<CR>       " Fix Errors

" Git
nnoremap <silent> <Space>gb :Gblame<CR>    " Git Blame
nnoremap <silent> <Space>gs :Gstatus<CR>   " Git Status
nnoremap <silent> <Space>goo :Gbrowse<CR>  " Git Browse

" Terminal
" nnoremap <silent> <Space>' :Ttoggle<CR>            " Open/Close shell
" tnoremap <silent> <Space>' <C-\><C-n>:Ttoggle<CR>  " Open/Close Shell

" Test
nnoremap <Leader>tb :TestFile<CR>:TmuxNavigateDown<CR>     " Run test in Buffer
nnoremap <Leader>tt :TestNearest<CR>:TmuxNavigateDown<CR>  " Run test at point
noremap <f5> :TestNearest<CR>:TmuxNavigateDown<CR>         " Run test at point

" Register
nnoremap <silent> <Space>re :registers<CR>|     " Registers

" Text
nmap <Space>xa: <Plug>(EasyAlign)ip:<CR>|  " Text > Align > :
nmap <Space>xa= <Plug>(EasyAlign)ip=<CR>|  " Text > Align > =
nmap <Space>xa" <Plug>(EasyAlign)ip"<CR>|  " Text > Align > "

" Toggle
nnoremap <silent> <Space>ti :IndentLinesToggle<CR>
nnoremap <silent> <Space>tl :set nu! rnu!<CR>

" Global
nmap gy yygccp                           " Copy and Comment Lines
nmap <silent> <Space>cl gcc              " Comment Lines
vmap <silent> <Space>cl gcc              " Comment Lines
vmap <silent> <S-k> <Plug>MoveBlockUp    " Move Block Up
vmap <silent> <S-j> <Plug>MoveBlockDown  " Move Block Down

nnoremap <silent> <M-b> :Buffer<CR>|     " List Buffers
nnoremap <M-s> :update<CR>               " File > Save
nnoremap <silent> <M-w> :close<CR>       " Close window

nnoremap <silent> gd :tag <C-R><C-W><CR>

autocmd FileType javascript nmap <silent> gd <Plug>(coc-definition)
autocmd FileType javascript nmap <silent> gr <Plug>(coc-references)
autocmd FileType javascript nmap <silent> [g <Plug>(coc-diagnostic-prev)
autocmd FileType javascript nmap <silent> ]g <Plug>(coc-diagnostic-next)
