set encoding=utf-8
scriptencoding utf-8

"Install vim-plug if not already installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'morhetz/gruvbox'
Plug 'NLKNguyen/papercolor-theme'
Plug 'chriskempson/base16-vim'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Raimondi/delimitMate'
Plug 'sfiera/vim-emacsmodeline'

" Use :w suda://% to save current file as sudo
Plug 'lambdalisue/suda.vim'

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-commentary'

Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'joshdick/onedark.vim'

let g:polyglot_disabled = ['org']
Plug 'sheerun/vim-polyglot'

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

set termguicolors
set background=light
"colorscheme PaperColor

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
if &term ==# 'xterm-256color' || &term ==# 'screen-256color' || &term ==# 'xterm-termite' || &term ==# 'xterm-kitty'
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

"Space as an additional leader
map <space> <leader>
inoremap jk <ESC>
nmap <silent> <leader><space> :nohlsearch<cr>

"Prevent searches being highlighed when vimrc reloads
nohlsearch

"use %% to get current dir in ex
cnoremap %% <C-R>=expand('%:h').'/'<cr>

"quickly edit this file
nmap <leader>ev :e ~/.vimrc<cr>
nmap <leader>sv :so ~/.vimrc<cr>

"Quit vim with Q as well as q
command! Q q
command! W w

"open a file where I left off last time
if has('autocmd')
    autocmd vimrc BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') && &filetype !=# 'gitcommit'
                \| exe "normal! '\"" | endif
endif

"Wild mode
set wildmenu
set wildmode=list:longest,full

"Make background transparent
highlight! Normal ctermbg=none
highlight! NonText ctermbg=none

if has('nvim')
    highlight! Normal guibg=none
    highlight! NonText guibg=none
endif

"Highlight current line numbe
hi clear CursorLine
hi CursorLineNR cterm=bold
set cursorline

"Undo tree
"Persistent undo
if has('persistent_undo')
    set undodir=~/.vim/undodir/
    set undofile

    " neovim undo files are incompatible with regular vim
    if has('nvim')
        set undodir=~/.vim/undodir/neovim
    endif
endif

"Switch to last buffer
nmap gb <C-^>

"vim airline
let g:airline_powerline_fonts = 1
let g:airline_theme = 'sol'
let g:airline#extensions#whitespace#enabled = 0

" fzf
nmap <C-p> :Files<cr>
nmap <leader>b :Buffers<cr>
nmap <leader>/ :Rg<space>

"delimitMate
let g:delimitMate_expand_cr = 2
let g:delimitMate_expand_space = 1
