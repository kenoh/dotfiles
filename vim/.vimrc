" PATHOGEN
execute pathogen#infect()


set wildmode=longest,list,full
set wildmenu


syntax on
set background=dark
set t_Co=256
"colorscheme molokai
"colors zenburn
filetype indent plugin on

set modeline
set background=dark
set tabstop=4
set shiftwidth=4
set noexpandtab
autocmd Filetype python setlocal noexpandtab
set smarttab
set autoindent
"set smartindent -- deprecated
set cindent
set wrap

" treat long lines as breaklines
map j gj
map <Down> g<Down>
map k gk
map <Up> g<Up>

" tabs movement
nnoremap <F2> :tabp<CR>
nnoremap <F3> :tabn<CR>
nnoremap <F4> :tabnew<CR>
inoremap <F2> <Esc>:tabp<CR>i
inoremap <F3> <Esc>:tabn<CR>i
inoremap <F4> <Esc>:tabnew<CR>i

filetype on
filetype plugin on
filetype indent on

" home to first non-blank character

set viminfo='100,\"100,:20,%,n~/.viminfo
"set list
"set list listchars=tab:\ \ ,trail:@
set number
highlight LineNr ctermfg=darkgrey

set hlsearch
set incsearch

set laststatus=2
set statusline=%<%f%h%m%r%=%b\ 0x%B\ \ %l,%c\ %P

set scrolloff=10

set showmatch
set matchtime=2
set matchpairs+=<:>
set matchpairs+=(:)
set matchpairs+={:}
set matchpairs+=[:]

set wildmenu
set wildignore=*.o,*.~,*.pyc

set nobackup
set nowb
set noswapfile

set lazyredraw " performance for macros exec

set magic " magic for regexps


map <F7> :!./%<CR>
"nmap <F8> :TagbarToggle<CR>
map <F8> :w<bar>!!<CR>
map <f9> :make<CR>
set pastetoggle=<f10>

" Python AutoCompletion
"autocmd FileType python set omnifunc=pythoncomplete#Complete
"

function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colorize statusbar depending on mode
"
function! InsertStatuslineColor(mode)
  if a:mode == 'i'
    hi statusline guibg=Green ctermfg=darkred guifg=Black ctermbg=white
  elseif a:mode == 'r'
    hi statusline guibg=Yellow ctermfg=yellow guifg=Black ctermbg=black
  else
    hi statusline guibg=Black ctermfg=black guifg=White ctermbg=white
  endif
endfunction

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi statusline guibg=DarkGrey ctermfg=8 guifg=White ctermbg=15

" default the statusline to green when entering Vim
hi statusline guibg=DarkGrey ctermfg=8 guifg=White ctermbg=15

" NERDTree
map <C-n> :NERDTreeToggle<CR>


""" Niji parentheses highlignting
"let g:niji_match_all_filetypes = 1


" .h and .c switching
source ~/.vim/autoload/a.vim


""" spacehi.vim
source ~/.vim/autoload/spacehi.vim
map <silent> <unique> <F5> :ToggleSpaceHi<CR>


""" sudo write
cnoremap sudow w !sudo tee % >/dev/null
