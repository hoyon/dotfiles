set encoding=utf-8
scriptencoding utf-8

call plug#begin('~/.vim/plugged')

Plug 'morhetz/gruvbox'
Plug 'jnurmine/zenburn'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/tmuxline.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'godlygeek/tabular'
Plug 'PeterRincker/vim-argumentative'
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
Plug 'wellle/visual-split.vim'
Plug 'bkad/CamelCaseMotion'
Plug 'Raimondi/delimitMate'
Plug 'majutsushi/tagbar'
Plug 'sfiera/vim-emacsmodeline'
Plug 'qpkorr/vim-renamer'
Plug 'jceb/vim-orgmode'
Plug 'vim-scripts/utl.vim'
Plug 'vim-scripts/SyntaxRange'

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-speeddating'

Plug 'Shougo/deoplete.nvim'
Plug 'zchee/deoplete-clang'
Plug 'racer-rust/vim-racer'
Plug 'xaizek/vim-inccomplete'
Plug 'davidhalter/jedi-vim'
Plug 'w0rp/ale'

"Plug 'Valloric/YouCompleteMe'
"Plug 'benekastah/neomake'
Plug 'kana/vim-altr'
Plug 'sjl/tslime.vim'
Plug 'Chiel92/vim-autoformat'
Plug 'SirVer/ultisnips', { 'on': [] }
Plug 'honza/vim-snippets'

Plug 'dag/vim2hs' , { 'for': 'haskell' }
Plug 'bitc/lushtags', { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc'

Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'wting/rust.vim', { 'for': 'rust' }
"Plug 'racer-rust/vim-racer', { 'for': 'rust' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'elzr/vim-json', { 'for': 'json' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'digitaltoad/vim-jade', { 'for': 'jade' }
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
Plug 'Slava/vim-spacebars'
Plug 'tikhomirov/vim-glsl', { 'for': 'glsl' }
Plug 'elixir-lang/vim-elixir'
Plug 'slashmili/alchemist.vim'
Plug 'ElmCast/elm-vim', { 'for': 'elm' }
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'plasticboy/vim-markdown', { 'for': 'md' }
Plug 'dag/vim-fish', { 'for': 'fish' }
Plug 'zah/nim.vim', { 'for': 'nim' }
Plug 'igankevich/mesonic'
Plug 'ron-rs/ron.vim', { 'for': 'ron' }
Plug 'reasonml-editor/vim-reason-plus', { 'for': 're' }

Plug 'junegunn/goyo.vim'

call plug#end()

set linebreak
set number
set hidden
set ignorecase
set smartcase
set showmatch
set shortmess+=I
set mouse=a
set showcmd
set shell=sh

set background=dark
colorscheme zenburn

set noswapfile
set nobackup
set autoread
set nolazyredraw
set hlsearch

"Indent settings
set expandtab
set shiftwidth=4
set softtabstop=4
set cindent
"Use shift-tab to insert a literal tab character
inoremap <S-Tab> <C-V><Tab>

set ttimeoutlen=0

"Use I-beam when in insert mode
if &term ==# 'xterm-256color' || &term ==# 'screen-256color' || &term ==# 'xterm-termite'
    let &t_SI = "\<Esc>[5 q"
    let &t_EI = "\<Esc>[1 q"
endif

if exists('$TMUX')
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
endif

augroup vimrc
    autocmd! 
augroup END

set title
autocmd vimrc BufEnter * let &titlestring = 'vim ' . expand("%:p")

"Move by screen lines instead of actual lines
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk

nnoremap <C-J> :wincmd w<cr>
nnoremap <C-K> :wincmd W<cr>

"Space as an additional leader
map <space> <leader>
map <Enter> o<ESC>
map <S-Enter> O<ESC>
inoremap jk <ESC>
nmap <silent> <leader><space> :nohlsearch<cr>

"Prevent searches being highlighed when vimrc reloads
nohlsearch

"Fold settings
nmap <silent> z<space> za
set foldcolumn=1
set foldlevel=99

"use %% to get current dir in ex
cnoremap %% <C-R>=expand('%:h').'/'<cr>

"quickly edit this file
nmap <leader>ev :e ~/.vimrc<cr>
nmap <leader>sv :so ~/.vimrc<cr>

"Quit vim with Q as well as q
command! Q q
command! W w

"Mappings for running make
nmap <leader>mm :Make -j4<cr>
nmap <leader>mr :make run<cr>

"open a file where I left off last time
if has('autocmd')
    autocmd vimrc BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') && &filetype !=# 'gitcommit'
                \| exe "normal! '\"" | endif
endif

"Wild mode
set wildmenu
set wildmode=list:longest,full

"Make background transparent
hi Normal ctermbg=none
hi NonText ctermbg=none

"Highlight current line number
hi clear CursorLine
hi CursorLineNR cterm=bold
set cursorline

"make quickfix stop creating in file included.. files
let &errorformat = '%E%f: line %l\, col %c\, Error - %m'

" Prevent <cr> being remapped in quickfix and location
autocmd vimrc BufReadPost quickfix nnoremap <buffer> <cr> <cr>
autocmd vimrc BufReadPost location nnoremap <buffer> <cr> <cr>

"save with root pemissions
cmap w!! w !sudo tee > /dev/null %

"Undo tree
"Persistent undo
if has('persistent_undo')
    set undodir=~/.vim/undodir/
    set undofile
endif
nmap <F12> :UndotreeToggle<cr>

"Switch to last buffer
nmap gb :b#<cr>

"NERDTree
"let NERDTreeQuitOnOpen = 1
nmap <F2> :NERDTreeToggle<cr>
let g:NERDTreeMapJumpNextSibling='☻'
let g:NERDTreeMapJumpPrevSibling='☺'

" fzf
nmap <C-p> :Files<cr>
nmap <leader>b :Buffers<cr>
nmap <leader>/ :Ag<space>

"vim airline
let g:airline_powerline_fonts = 1
let g:airline_theme= 'base16_eighties'
"if !exists('g:airline_symbols')
"let g:airline_symbols = {}
"endif
"let g:airline_symbols.space = "\ua0"
let g:airline#extensions#whitespace#enabled = 0

"altr -- switch between header and source file with f4
nmap <F4> <Plug>(altr-forward)
nmap <S-F4> <Plug>(altr_back)

"Ale
nnoremap <silent> <C-n> :ALEPreviousWrap<cr>zz
nnoremap <silent> <C-m> :ALENextWrap<cr>zz
"let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {
\   'haskell': ['hlint', 'stack-build']
\}

nmap <leader>lo :lopen<cr>
nmap <leader>lc :lclose<cr>
nmap <leader>ln :lnext<cr>
nmap <leader>lp :lprevious<cr>

"YouCompeleteMe
let g:ycm_global_ycm_extra_conf = '~/.vim/.ycm_extra_conf.py'
let g:ycm_key_invoke_completion = '<NUL>'
let g:ycm_confirm_extra_conf = 0
let g:ycm_autoclose_preview_window_after_insertion = 0
let g:ycm_rust_src_path = '/usr/src/rust/src'
let g:ycm_semantic_triggers = {'haskell' : ['.']}
nnoremap <leader>kd :YcmCompleter GoToDeclaration<cr>
nnoremap <leader>kf :YcmCompleter GoToDefinition<cr>
nnoremap <leader>kk :YcmCompleter GoTo<cr>
nnoremap <leader>kt :YcmCompleter GetType<cr>
nnoremap <leader>kp :YcmCompleter GetParent<cr>
nnoremap <leader>kx :YcmCompleter FixIt<cr>

"Ultisnips
let g:UltiSnipsExpandTrigger='<C-j>'
let g:UltiSnipsJumpForwardTrigger='<C-j>'
let g:UltiSnipsJumpBackwardTrigger='<C-k>'
" Only load ultisnips when first used
inoremap <silent> <C-j> <C-r>=LoadUltiSnips()<cr>
function! LoadUltiSnips()
    let l:curpos = getcurpos()
    execute plug#load('ultisnips')
    call cursor(l:curpos[1], l:curpos[2])
    call UltiSnips#ExpandSnippet()
    return ''
endfunction

"Deoplete
if has('nvim')
    let g:deoplete#enable_at_startup = 1
endif

"Deoplete-clang
let g:deoplete#sources#clang#libclang_path='/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header='/usr/lib/clang'
let g:deoplete#sources#clang#sort_algo='priority'

"Inccomplete
let g:clang_user_options=' /usr/include/c++/8.2.1/ '
let g:inccomplete_addclosebracket='never'

"Autoformat
nmap <f5> :Autoformat<cr>

"Tagbar
nmap <F3> :TagbarToggle<cr>

"delimitMate
let g:delimitMate_expand_cr = 2
let g:delimitMate_expand_space = 1

"tslime
let g:tslime_ensure_trailing_newlines = 1

"Haskell
let g:haskell_conceal = 0
let g:haskellmode_completion_ghc = 0
autocmd vimrc FileType haskell setlocal omnifunc=necoghc#omnifunc shiftwidth=2 softtabstop=2 expandtab

"Rust
let g:racer_cmd = '/home/hoyon/.cargo/bin/racer'
let g:racer_experimental_completer = 1
let $RUST_SRC_PATH='/usr/src/rust/src'

"json
let g:vim_json_syntax_conceal = 0

"Ron
autocmd vimrc FileType ron setlocal shiftwidth=2 softtabstop=2 expandtab
