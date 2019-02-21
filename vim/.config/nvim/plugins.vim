" Set runtimes for Neobundle {{{
set runtimepath+=~/.vim/bundle/vimproc.vim
set runtimepath+=~/.vim/bundle/neobundle.vim
set runtimepath+=~/.vim/bundle/unite.vim
" }}}

call plug#begin('~/.vim/bundle')
" Load plugins {{{

" Shuogo's shit {{{
" PlugFetch 'Shougo/neobundle.vim' " Let Neobundle manage itself
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/unite.vim'
" Plug 'Shougo/neocomplete'
" Plug 'Shougo/neosnippet' | Plug 'Shougo/neosnippet-snippets'
" }}}

" Unite plugins {{{
Plug 'tsukkee/unite-tag'
Plug 'Shougo/unite-outline'
" }}}

" Ui {{{
Plug 'ervandew/supertab'
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/Nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'bling/vim-airline'
Plug 'majutsushi/tagbar'
" Plug 'taglist.vim'
" Plug 'roman/golden-ratio'
Plug 'jonathanfilip/vim-lucius'
Plug 'chriskempson/base16-vim'
Plug 'ryanoasis/vim-devicons'
" }}}

" TPOPE {{{
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-commentary'
" Plug 'tpope/vim-surround'
Plug 'tpope/vim-jdaddy', { 'for': 'json'}
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-ragtag'
Plug 'tpope/vim-characterize'
" }}}

" Language Specifics lazy load all of these {{{
Plug 'derekwyatt/vim-scala', { 'for': 'scala'}
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee'}

Plug 'wookiehangover/jshint.vim', { 'for': 'js'}

Plug 'fatih/vim-go', { 'for': 'go'}

Plug 'mattn/emmet-vim' ", { 'for': ['html', 'css', 'js', 'eruby'] }

Plug 'rust-lang/rust.vim', { 'for': 'rust' }

Plug 'keith/rspec.vim'

Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }

Plug 'noprompt/vim-yardoc', { 'for': 'ruby' }
" }}}


" Utils/QoL {{{
" Plug 'chrisbra/NrrwRgn'
" Plug 'deris/vim-shot-f'
" Plug 'nathanaelkane/vim-indent-guides'

" Plug 'Yggdroot/indentLine'
" Plug 'Raimondi/delimitMate'
Plug 'jiangmiao/auto-pairs'  " Replace delimate
" Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align' " Replace tabular
Plug 'easymotion/vim-easymotion'
Plug 'matze/vim-move'
" Plug 'simnalamburt/mundo.vim'
Plug 'rhysd/clever-f.vim'

" Plug 'spreadsheet.vim'
" Plug 'dhruvasagar/vim-table-mode'

Plug 'terryma/vim-multiple-cursors'

" Plug 'kassio/neoterm'

Plug 'airblade/vim-gitgutter'

" Plug 'terryma/vim-expand-region'

Plug 'machakann/vim-sandwich' 

" Plug 'KabbAmine/vCoolor.vim'
" Plug 'guicolorscheme.vim'
" Plug 'CSApprox'

" Plug 'KevinGoodsell/vim-csexact'
" }}}


" Pandoc {{{
Plug 'vim-pandoc/vim-pandoc', { 'for': 'pandoc' }
Plug 'vim-pandoc/vim-pandoc-after', { 'for': 'pandoc' }
Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': 'pandoc' }

Plug 'plasticboy/vim-markdown', { 'for': 'mkd' }
" }}}

Plug 'vimwiki/vimwiki'
Plug 'ScrollColors', {'on': 'SCROLLCOLOR'}
" Plug 'ntpeters/vim-better-whitespace'
Plug 'kopischke/vim-fetch' 
Plug 'gregsexton/gitv'
Plug 'szw/vim-ctrlspace'
Plug 'morhetz/gruvbox'
Plug 'junegunn/seoul256.vim'
" }}}
call plug#end()

