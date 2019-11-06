"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PLUGINS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" If vundle is not installed, do it first
if (!isdirectory(expand('$HOME/.vim/repos/github.com/Shougo/dein.vim')))
	call system(expand('mkdir -p $HOME/.vim/repos/github.com'))
	call system(expand('git clone https://github.com/Shougo/dein.vim $HOME/.vim/repos/github.com/Shougo/dein.vim'))
endif

set runtimepath+=~/.vim/repos/github.com/Shougo/dein.vim/

call dein#begin(expand('~/.vim'))
call dein#add('Shougo/dein.vim')
call dein#add('tpope/vim-fugitive')
call dein#add('tpope/vim-commentary')
call dein#add('tpope/vim-sleuth')  " automatic buffer variables (indent,...)
call dein#add('ctrlpvim/ctrlp.vim')
call dein#add('airblade/vim-gitgutter')
call dein#add('liuchengxu/vim-which-key')
if dein#check_install()
	call dein#install()
endif
call dein#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CONFIGURATION
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" General {{{
let mapleader = ";"
filetype plugin indent on
syntax on
set t_Co=256
set background=light
set scrolloff=5
set sidescrolloff=5
set title
set autoread
set nobackup
set nowb
set noswapfile
set history=1000
set undolevels=1000
set viminfo='500,\"500,f1
set iskeyword+=-
set hidden
set listchars=tab:>-,trail:·
set updatetime=1000
hi CursorLine     cterm=NONE ctermbg=NONE ctermfg=NONE guibg=NONE guifg=NONE
hi Underlined     cterm=reverse ctermbg=NONE ctermfg=NONE gui=reverse guibg=NONE guifg=NONE  " hl under cursor
hi LineNr         cterm=NONE ctermfg=white ctermbg=darkgrey gui=NONE guifg=white guibg=darkgrey
hi CursorLineNr   cterm=bold,reverse gui=bold,reverse guibg=NONE
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END

hi SpecialKey   cterm=NONE ctermfg=DarkGray guifg=DarkGray
nnoremap <silent> <Leader>l
	\ :if exists('w:long_line_match') <Bar>
	\   silent! call matchdelete(w:long_line_match) <Bar>
	\   unlet w:long_line_match <Bar>
	\ elseif &textwidth > 0 <Bar>
	\   let w:long_line_match = matchadd('ErrorMsg', '\%>'.&tw.'v.\+', -1) <Bar>
	\ else <Bar>
	\   let w:long_line_match = matchadd('ErrorMsg', '\%>80v.\+', -1) <Bar>
	\ endif<CR>
" }}}

if (&termencoding ==# 'utf-8' || &encoding ==# 'utf-8') && v:version >= 700
  let &g:listchars = "tab:\u21e5\u00b7,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u00b7"
  let &g:fillchars = "vert:\u250b,fold:\u00b7"
else
  setglobal listchars=tab:>\ ,trail:-,extends:>,precedes:<
endif

" Other {{{
set tags=tags;/
" }}}

" Search {{{
set incsearch
set hlsearch " check out C-l
set ignorecase
set smartcase
" }}}


" move by rows, not lines
nnoremap k gk
nnoremap j gj
nnoremap gk k
nnoremap gj j

" indentation
set backspace=indent,eol,start
set complete-=i
"set noexpandtab
"set softtabstop=0
"set tabstop=4
"set shiftwidth=4
set cindent
set cinoptions=(0,u0,U0
" }}}


" Statusbar {{{
set laststatus=1
set showtabline=1
set ruler
set rulerformat=%50(%=%f%1*%m%r%*%w\ \:b%n\ 0x%B\ %l,%c%V\ %P%) " a ruler on steroids
set showcmd
set wildmenu
set wildignore=*.o,*.obj,*.bak,*.exe,*.py[co],*.swp,*~,*.pyc,.svn
" }}}

" Clever {{{
" for C-A and C-X, do not consider 0<number> an octal
set nrformats-=octal

if has("autocmd")
	au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

""" colourful word under the cursor
let g:toggleHighlight = 0
function! HighlightUnderCursor(...)
  if a:0 == 1 "toggle behaviour
    let g:toggleHighlight = 1 - g:toggleHighlight
  endif

  if g:toggleHighlight == 0 "normal action, do the hi
    silent! exe printf('match Underlined /\V\<%s\>/', escape(expand('<cword>'), '/\'))
  else
    "do whatever you need to clear the matches, e.g. 'call clearmatches()'
    "or nothing at all, since you are not printing the matches
  endif
endfunction
autocmd CursorMoved * call HighlightUnderCursor()

""" clear hl search
if maparg('<C-L>', 'n') ==# ''
	nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

""" set/unset paste
set pastetoggle=<leader>p

""" tabs movement
nnoremap <silent> <F2> :tabprev<CR>
nnoremap <silent> <leader><F2> :tabe<CR>
nnoremap <silent> <F3> :tabnext<CR>

""" windows movement
nnoremap <silent> <F4> <C-W><C-W>
nnoremap <silent> <leader><F4> :vsplit<CR>

""" toggle listchars
nnoremap <silent> <leader><space> :set list!<CR>

""" run last shell command
nnoremap <silent> <leader>r :!!<CR>
