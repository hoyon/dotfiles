set encoding=utf-8
scriptencoding utf-8

"Install vim-plug if not already installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

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

let g:polyglot_disabled = ['org', 'sensible']
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
set clipboard^=unnamed
set showcmd
set shell=sh

set termguicolors
set background=light

set noswapfile
set nobackup
set autoread
set hlsearch

"Indent settings
set expandtab
set shiftwidth=4
set softtabstop=4
set autoindent
"Use shift-tab to insert a literal tab character
inoremap <S-Tab> <C-V><Tab>

set ttimeoutlen=0

"Use I-beam when in insert mode
let &t_SI = "\<Esc>[5 q"
let &t_EI = "\<Esc>[1 q"

augroup vimrc
    autocmd! 
augroup END

set title
autocmd vimrc BufEnter * let &titlestring = v:progname . ' ' . expand("%:p")

"vim-polyglot replaces filetype.vim with a stale snapshot, so newer built-in filetypes need declaring
autocmd vimrc BufRead,BufNewFile *.kdl setf kdl

"Move by screen lines instead of actual lines
nnoremap <silent> j gj
nnoremap <silent> k gk
vnoremap <silent> j gj
vnoremap <silent> k gk

"Space as an additional leader
map <space> <leader>
nnoremap <silent> <leader><space> :nohlsearch<cr>

"Prevent searches being highlighed when vimrc reloads
nohlsearch

"use %% to get current dir in ex
cnoremap %% <C-R>=expand('%:h').'/'<cr>

"quickly edit this file
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>sv :so $MYVIMRC<cr>

"Quit vim with Q as well as q
command! Q q
command! W w

"open a file where I left off last time
autocmd vimrc BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') && &filetype !=# 'gitcommit'
            \| exe "normal! '\"" | endif

"Wild mode
set wildmenu
set wildmode=list:longest,full

"Make background transparent
highlight! Normal ctermbg=NONE guibg=NONE
highlight! NonText ctermbg=NONE guibg=NONE

"Highlight current line numbe
hi clear CursorLine
hi CursorLineNR cterm=bold
set cursorline

"Persistent undo
set undodir=~/.vim/undodir/
set undofile

" neovim undo files are incompatible with regular vim
if has('nvim')
    set undodir=~/.vim/undodir/neovim
endif

"Switch to last buffer
nnoremap gb <C-^>

"vim airline
let g:airline_powerline_fonts = 1
let g:airline_theme = 'sol'
let g:airline#extensions#whitespace#enabled = 0

"fzf
nnoremap <C-p> :Files<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>/ :Rg<space>

"delimitMate
let g:delimitMate_expand_cr = 2
let g:delimitMate_expand_space = 1
