""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins {{{
"

" Auto-install vim-plug plugin
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

" List plugins you want to be installed by vim-plug {{{
call plug#begin('~/.vim/plugged')
Plug 'junegunn/rainbow_parentheses.vim' " depth-based parentheses coloring
Plug 'airblade/vim-gitgutter' " leftmost column showing git diff signs
Plug 'junegunn/vim-easy-align' " powerful text alignment
Plug 'sjl/gundo.vim' " tree-like undo
Plug 'mtth/scratch.vim' " scratch buffer?
Plug 'kien/ctrlp.vim' " file finder
Plug 'tacahiroy/ctrlp-funky'  "requires ctrlp.vim
Plug 'ompugao/ctrlp-history'  "requires ctrlp.vim
Plug 'jgdavey/tslime.vim'
Plug 'tpope/vim-fugitive' " git manipulation
Plug 'tpope/vim-dispatch' " makes sending text to tmux buffer easy
Plug 'majutsushi/tagbar' " shows a list of ctags, on right
Plug 'kshenoy/vim-signature'
Plug 'xolox/vim-misc' "required by vim-easytags
Plug 'xolox/vim-easytags'
Plug 'utags'
Plug 'spec.vim'
"Plug 'patch.vim' """does not work???
Plug 'junkblocker/patchreview-vim'
Plug 'jceb/vim-orgmode' " emacs orgmode notetaking
Plug 'tpope/vim-speeddating'
Plug 'Smart-Tabs' " makes indenting with tabs and alignment with spaces nice
Plug 'terryma/vim-expand-region' " intelligent selecting :)
Plug 'actionshrimp/vim-xpath'  " live xpath filter
Plug 'sjbach/lusty'  " LustyExplorer and LustyJuggler, for files/buffers live search
call plug#end()
" }}}

" Plugin-specific settings below {{{

" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)
let g:easy_align_delimiters = {
	\ 't': {
	\       'pattern': "\<tab>", 'left_margin': 0, 'right_margin': 0 },
	\ '/': {
	\       'pattern':         '//\+\|/\*\|\*/',
	\       'delimiter_align': 'l',
	\       'ignore_groups':   ['!Comment']
	\       }
	\}

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" TSlime
vmap <C-c><C-c> <Plug>SendSelectionToTmux
nmap <C-c><C-c> <Plug>NormalModeSendToTmux
nmap <C-c>r <Plug>SetTmuxVars

" Tagbar
map <leader>t :TagbarToggle<CR>

let g:tagbar_type_spec = {
    \ 'ctagstype' : 'spec',
    \ 'kinds'     : [
        \ 's:section',
        \ 'g:global'
    \ ]
    \ }

let g:tagbar_type_patch = {
	\ 'ctagstype' : 'patch',
	\ 'kinds'     : [
		\ 'f:file'
	\ ]
	\ }

" CScope
"nnoremap <leader>fa :call CscopeFindInteractive(expand('<cword>'))<CR>
"nnoremap <leader>l :call ToggleLocationList()<CR>

" FlagShip
let g:tablabel =
      \ "%N%{flagship#tabmodified()} %{flagship#tabcwds('shorten',',')}"
autocmd User Flags call Hoist("buffer", "fugitive#statusline")


" GitGutter
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1
nmap <Leader>hu <Plug>GitGutterRevertHunk

" Easy Tags
let g:easytags_async = 1

" vim-expand-region



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Personal settings {{{
"

" General {{{
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
set number
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
set noexpandtab
set softtabstop=0
set tabstop=4
set shiftwidth=4
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
autocmd CursorMoved * exe printf('match Underlined /\V\<%s\>/', escape(expand('<cword>'), '/\'))

""" clear hl search
if maparg('<C-L>', 'n') ==# ''
	nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

""" set/unset paste
set pastetoggle=<leader>p

""" tabs movement
nnoremap <silent> <F2> :tabprev<CR>
nnoremap <silent> <F3> :tabnext<CR>
nnoremap <silent> <F4> :tabe<CR>

""" toggle listchars
nnoremap <silent> <leader><space> :set list!<CR>

""" run last shell command
nnoremap <silent> <F1> :!!<CR>

