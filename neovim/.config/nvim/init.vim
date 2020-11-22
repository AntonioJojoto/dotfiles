call plug#begin()
" Vim functionality
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
""Simplemente latex bro
Plug 'lervag/vimtex'
""Snippeds, basicamente sintaxis
Plug 'vimwiki/vimwiki'

" Vim GUI
Plug 'joshdick/onedark.vim'
Plug 'itchyny/lightline.vim'
Plug 'morhetz/gruvbox'
call plug#end()

"" General
set number	
set relativenumber
set showbreak=+++
set showmatch	
set visualbell
set enc=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc
set undofile
set undodir=~/.vim/undodir
set cursorline
set mouse=a  " scroll with mouse
set linebreak
set scrolloff=3

setlocal spell
set spelllang=en_gb
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u

"" My custom mappings, two times hh in insert mode is equivalent to escape
imap hh <Esc> 

" swap
set directory=$HOME/.vim/swap//

augroup numbertoggle
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
    autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END 

set hlsearch
set smartcase	
set ignorecase
set incsearch	

" ident options
set autoindent
set tabstop=8
set shiftwidth=8
set cindent
"set smartindent
filetype plugin indent on " smart autoindentaton
set wrap
set showbreak=>\ 
set smarttab
set softtabstop=8

" search options
set incsearch
set hlsearch
set ignorecase
set smartcase

" Disable beep and flash
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

"" Advanced
set ruler	 
set undolevels=1000
set backspace=indent,eol,start
set fillchars+=stl:\ ,stlnc:\

let mapleader = "\<Space>"

" Normal mode mappings
"nmap <leader>w :w!<cr>
nmap <leader>t :terminal<cr>

" Mover cursor a otras ventanas ventanas
nmap <leader>wj <C-W>j
nmap <leader>wh <C-W>h
nmap <leader>wk <C-W>k
nmap <leader>wl <C-W>l

" tabs
nmap <leader>tn :tabnew<cr>
nmap <leader>tc :tabclose<cr>

" Tab movement

" Move tab to
nmap <leader>tk :tabmove -<cr> 
nmap <leader>tj :tabmove +<cr>

" Go to next/previous tab
nmap <leader>. :tabnext +<cr>
nmap <leader>, :tabnext -<cr>

" buffer 
nmap <leader>bn :bnext<CR>
nmap <leader>bp :bprevious<CR>

" Move visual selection
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" NERDTree 
"autocmd VimEnter * NERDTree
noremap <leader>nn :NERDTreeToggle<cr>
noremap <leader>nb :NERDTreeFromBookmark
noremap <leader>nf :NERDTreeFind<cr>
noremap <leader>nc :NERDTreeCWD<cr>

" Vimwiki
set nocompatible
let g:vimwiki_list = [{'path': '~/apuntes/vimwiki/', 'syntax': 'default', 'ext': '.wiki'}]
let g:vimwiki_global_ext = 0
let g:vimwiki_table_mappings = 0

syntax on
"colorscheme onedark

" Transparent background 
"hi Normal ctermbg=NONEmap <leader>z :Goyo<cr>

" Vimtex
let g:polyglot_disabled = ['latex']
let g:tex_flavor = 'latex'
let g:vimtex_view_method = 'zathura'
set conceallevel=1
let g:tex_conceal='abdmg'
autocmd Filetype tex,latex inoremap <C-s> <Esc>:silent exec ".!scrot -s -e 'latexscrot $f && mv $f ./media/'" <CR><CR>:w<CR>

"gruvbox
"set termguicolors
let g:gruvbox_italic=1
colorscheme gruvbox
set background=dark

hi Normal ctermbg=NONE


