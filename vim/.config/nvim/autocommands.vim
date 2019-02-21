" Colorscheme {{{
au VimEnter,ColorScheme * :hi IndentGuidesOdd ctermbg=238
au VimEnter,ColorScheme * :hi IndentGuidesEven ctermbg=240
au VimEnter,ColorScheme * :hi CursorLine cterm=bold ctermfg=none
" }}}

" Fix issue with artefacts
au BufWritePost * :redraw!

" Sourcing {{{
au BufWritePost *.vim,.vimrc source %
" }}}

" Neocompl omnicomplete {{{
au FileType css setlocal omnifunc=csscomplete#CompleteCSS
au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
" }}}

" Go mappings {{{
au FileType go nmap <Leader>s <Plug>(go-implements)
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
" au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>u <Plug>(go-build)
au FileType go nmap <leader>e <Plug>(go-test)
" }}}

" Use spaces for tabs unless filetype {{{
au Filetype ruby set expandtab
" au FileType go,javascript set noexpandtab
" }}}

" Foldmethods {{{
au FileType vim setlocal foldmethod=marker
" }}}

" Filerun vars {{{
au FileType ruby let filerun = "ruby"
au FileType go let filerun = "go run"
au FileType c++ let filerun = "g++"

au FileType ruby let b:dispatch = 'bundle exec rspec %' 
" }}}

" Rspec {{{
au BufEnter *_spec.rb setlocal syntax=rspec
" }}}

" Strip trailing
fun! StripTrailingWhitespace()
    " Don't strip on these filetypes
    if &ft =~ 'vim'
        return
    endif
    %s/\s\+$//e
endfun

au BufWritePre * call StripTrailingWhitespace() 

au TermOpen * setlocal nonumber
" au WinEnter * if &buftype != 'nofile' && &buftype != 'terminal' | setlocal number |endif
" au WinLeave * :setlocal nonumber 

" au InsertLeave * if expand('%') != '' | update | endif
