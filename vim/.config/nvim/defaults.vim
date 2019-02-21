filetype plugin indent on
filetype plugin on

set tabstop=2       " Width of tab is 2
set expandtab       " Convert tabs to spaces
set shiftwidth=2    " Indents have width 4
set softtabstop=2   " Set the number of columns for a tab
set nu
set rnu
set noshowmode
set autowrite
set cmdheight=2
set shiftround
set showcmd         " Show (partial) command in status line.
set showmatch       " Show matching brackets.
set sidescrolloff=5
set scrolloff=10
set smartcase       " Case insensitive searches become sensitive with capitals
set smarttab        " sw at the start of the line, sts everywhere else
" set lazyredraw
set eol
set hidden
set hls
set colorcolumn=80
set cursorline
set isk+=- " Treat “-” like a word separator (for auto-completion!) 
set t_Co=256 
set fillchars=vert:\│  " Unicode split char

" Deprecated from #ShaDa
" set viminfo='50,<1000,s100,h,n~/.config/nvim-local/.viminfo " Viminfo settings 
set backupdir=/home/alex/.config/nvim/tmp/
set directory=/home/alex/.config/nvim/tmp/
" GUNDO
set undofile
set undodir=~/.config/nvim/undo

set path+=/home/alex/.config/nvim/

let g:rehash256 = 1

syntax on
set background=dark
" highlight ColorColumn ctermbg=8

let base16colorspace=256
" colorscheme base16-twilight

set laststatus=2
set backspace=2

nnoremap <CR> :

" nnoremap <SPACE> <Nop>
imap jk <esc>
let mapleader=" "

nnoremap <silent> <C-J> :wincmd j<CR>
nnoremap <silent> <C-K> :wincmd k<CR>
nnoremap <silent> <C-L> :wincmd l<CR>
nnoremap <silent> <C-H> :wincmd h<CR>
if has('nvim')
  nmap <BS> <C-W>h
endif

" map 1 :Unite -select=0 buffer<CR><CR>
" map 2 :Unite -select=1 buffer<CR><CR>
" map 3 :Unite -select=2 buffer<CR><CR>
" map 4 :Unite -select=3 buffer<CR><CR>
" map 5 :Unite -select=4 buffer<CR><CR>
" map 6 :Unite -select=5 buffer<CR><CR>
" map 7 :Unite -select=6 buffer<CR><CR>
" map 8 :Unite -select=7 buffer<CR><CR>
" map 9 :Unite -select=8 buffer<CR><CR>
" map 0 :Unite -select=9 buffer<CR><CR>

map <A-1> :Unite -select=0 buffer<CR><CR>
map <A-2> :Unite -select=1 buffer<CR><CR>
map <A-3> :Unite -select=2 buffer<CR><CR>
map <A-4> :Unite -select=3 buffer<CR><CR>
map <A-5> :Unite -select=4 buffer<CR><CR>
map <A-6> :Unite -select=5 buffer<CR><CR>
map <A-7> :Unite -select=6 buffer<CR><CR>
map <A-8> :Unite -select=7 buffer<CR><CR>
map <A-9> :Unite -select=8 buffer<CR><CR>
map <A-0> :Unite -select=9 buffer<CR><CR>

nnoremap <silent> gp :b#<CR>

" cnoremap Q q
" cnoremap W w
" cabbrev ew wq
" cabbrev qw wq
" cabbrev W w
" cabbrev Wq wq
" cabbrev Q q

" cnoremap sw SudoWrite
cmap sw SudoWrite

" nnoremap <Leader>t :T parallel_test -n 3 --serialize-stdout -e 'rspec'<CR>

map <C-Space> <C-X><C-O>

nnoremap <silent> <C-S> :<C-u>Update<CR>

nnoremap <leader>s :sort<CR>
nnoremap <silent> \[ :set nowrap<CR>
nnoremap <silent> \] :set wrap<CR>
nnoremap <silent> \ :nohls<CR><esc>
nnoremap <silent> <esc> :nohls<return><esc>

" set pastetoggle=<F10>
" inoremap <C-v> <F10><C-r>+<F10>
inoremap <C-v> <ESC>"+p`]a 
imap <C-k> <C-y>,

function! Incr()
  let a = line('.') - line("'<")
  let c = virtcol("'<")
  if a > 0
    execute 'normal! '.c.'|'.a."\<C-a>"
  endif
  normal `<
endfunction
vnoremap <C-a> :call Incr()<CR>

function! PDCIncr()
  execute ".s/^\\(#\\+\\)/#\\1/"
  nohls
endfunction

function! PDCDecr()
  execute ".s/^\\(#\\+\\)#/\\1/"
  nohls
endfunction

nnoremap <silent> <Leader>a :call PDCIncr()<CR>
nnoremap <silent> <Leader>x :call PDCDecr()<CR>

hi clear Conceal


if has('nvim')
  tnoremap <a-J> <c-\><c-n><c-w>j
  tnoremap <a-K> <c-\><c-n><c-w>k
  tnoremap <a-H> <c-\><c-n><c-w>h
  tnoremap <a-L> <c-\><c-n><c-w>l
  au WinEnter *term:* call feedkeys('i')
  au WinEnter *term:* :setlocal nonumber
  " tnoremap 
endif

nnoremap <silent> <Leader>= :exe "resize +5"<CR>
nnoremap <silent> <Leader>- :exe "resize -5"<CR>

nnoremap <silent> <Leader>, :exe "vertical resize +5"<CR>
nnoremap <silent> <Leader>. :exe "vertical resize -5"<CR>


" nnoremap <silent> <Leader>+ :exe "resize " . (winheight(0) * 3/2)<CR>
" nnoremap <silent> <Leader>- :exe "resize " . (winheight(0) * 2/3)<CR>

" nnoremap <silent> <Leader>. :exe "vertical resize " . (winwidth(0) * 3/2)<CR>
" nnoremap <silent> <Leader>, :exe "vertical resize " . (winwidth(0) * 2/3)<CR>

nnoremap <silent> <Leader>rt :w:call RunCurrentTest('!ts be rspec')<CR>
nnoremap <silent> <Leader>rl :w:call RunCurrentLineInTest('!ts be rspec')<CR>

" Ruby is an oddball in the family, use special spacing/rules
if v:version >= 703
  " Note: Relative number is quite slow with Ruby, so is cursorline
  au BufEnter *.rb setlocal norelativenumber foldmethod=manual re=1 colorcolumn= nocursorline
endif


" Tag jumping shit

nnoremap gt <C-]>
nnoremap gb <C-t>

nmap <silent> <leader>x :%FormatXML<CR>
vmap <silent> <leader>x :FormatXML<CR> 

map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR> 

command! FormatJSON %!python -m json.tool 

command! GenTags !ripper-tags -R --exclude=vendor
