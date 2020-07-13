call plug#begin()
" Vim functionality
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
""Mobility
Plug 'justinmk/vim-sneak'
""Simplemente latex bro
Plug 'lervag/vimtex'
""Te descarga syntax y autocompletado de muchos lenguajes
Plug 'sheerun/vim-polyglot' 
""Snippeds, basicamente sintaxis
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'
Plug 'vimwiki/vimwiki'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf.vim'

" Vim GUI
Plug 'joshdick/onedark.vim'
Plug 'itchyny/lightline.vim'
Plug 'morhetz/gruvbox'
call plug#end()

"let g:ale_completion_enabled = 1
"let g:ale_enabled = 0
"let g:ale_c_parse_compile_commands = 0
"let g:ale_c_parse_makefile = 1

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
set nowrap
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

" MAKE
" Command Make will call make and then cwindow which
" opens a 3 line error window if any errors are found.
" If no errors, it closes any open cwindow.
:command -nargs=* Make make <args> | cwindow 3
:map <Leader>m :Make<CR>

" Omnicomplete
"filetype plugin on
"set omnifunc=syntaxcomplete#Complete

" Goyo mappings
" Lightline
function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction
let g:lightline = {
	    \   'colorscheme': 'gruvbox',
	    \   'active': {
	    \     'left':[ [ 'mode', 'paste' ],
	    \              [ 'gitbranch', 'readonly', 'filename', 'modified', 'cocstatus' ]
	    \     ]
	    \   },
	    \   'component': {
	    \     'lineinfo': ' %3l:%-2v',
	    \   },
	    \  'component_function': {
	    \   'cocstatus': 'coc#status',
	    \   'currentfunction': 'CocCurrentFunction'
	    \ },
	    \ }
let g:lightline.separator = {
	    \   'left': '', 'right': ''
	    \}
let g:lightline.subseparator = {
	    \   'left': '', 'right': '' 
	    \}
let g:lightline.tabline = {
	    \   'left': [ ['tabs'] ],
	    \   'right': [ ['close'] ]
	    \ }
"let g:lightline.component_expand = {
      "\  'linter_checking': 'lightline#ale#checking',
      "\  'linter_warnings': 'lightline#ale#warnings',
      "\  'linter_errors': 'lightline#ale#errors',
      "\  'linter_ok': 'lightline#ale#ok',
      "\ }
let g:lightline.component_type = {
      \     'linter_checking': 'left',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'left',
      \ }
let g:lightline.active = { 'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ]] }
set showtabline=2  " Show tabline
set guioptions-=e  " Don't use GUI tabline

" Omnicomplete
filetype plugin on
set omnifunc=syntaxcomplete#Complete

" Goyo mappings
"map <leader>z :Goyo<cr>

" NERDTree 
"autocmd VimEnter * NERDTree
noremap <leader>nn :NERDTreeToggle<cr>
noremap <leader>nb :NERDTreeFromBookmark
noremap <leader>nf :NERDTreeFind<cr>
noremap <leader>nc :NERDTreeCWD<cr>

" Vim-session
noremap <leader>ss :SaveSession<Space>
noremap <leader>so :OpenSession!<Space>
noremap <leader>sc :CloseSession
noremap <leader>sd :DeleteSession!<Space>
let g:session_autoload = 'no'

" Ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetDirectories=["/home/tony/.config/nvim/plugged/vim-snippets/UltiSnips"]


" Vimwiki
set nocompatible
let g:vimwiki_list = [{'path': '~/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]
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

" YouCompleteMe
"let g:ycm_key_list_select_completion=[]
"let g:ycm_key_list_previous_completion=[]

"gruvbox
"set termguicolors
let g:gruvbox_italic=1
colorscheme gruvbox
set background=dark

hi Normal ctermbg=NONE

"" coc.vim stuff
set hidden
"set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes
inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')
nmap <leader>rn <Plug>(coc-rename)
