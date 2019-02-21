" Unite Mappings {{{
call unite#custom#profile('default', 'context', {
\   'winheight': 10,
\ })

let g:unite_source_grep_command = 'ag'
let g:unite_source_grep_default_opts =
      \ '--line-numbers --nocolor --nogroup --hidden --ignore ' .
      \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr''' .
      \  ' --ignore ''log'''
let g:unite_source_grep_recursive_opt = '' 

call unite#custom#source('buffer', 'converters', 'converter_buf_num')
call unite#custom#source('file_rec,file_rec/async,file_rec/git',
      \ 'ignore_pattern', join([
      \ '\.git/',
      \ 'tmp/',
      \ 'node_modules/',
      \ 'vendor/',
      \ '\.png',
      \ '\.gif',
      \ '\.jpg',
      \ ], '\|')) 

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
" call unite#custom_source('buffer', 'sorters', 'sorter_word')
" call unite#custom_source('buffer', 'matcher', 'matcher_context')

let g:unite_quick_match_table = {'1': 0, '2': 1, '3': 2, '4': 3, '5': 4, '6': 5, '7': 6, '8': 7, '9': 8, '0': 9}
let g:unite_source_history_yank_enable = 1

nnoremap [unite] <Nop>
nmap , [unite]

" Fuzzy
nmap <c-p> [unite]p

" Better '/'
nmap <c-s><c-f> [unite]l

nnoremap <Leader>b :Unite buffer<cr>

nnoremap <c-s><c-a> :UniteResume -buffer-name=grep<CR>
nnoremap <Leader>f :Unite -buffer-name=grep grep:.<cr>
nnoremap <c-s><c-s> :Unite -buffer-name=grep grep:.::<C-r><C-w><CR>
nnoremap \d :Unite -start-insert -buffer-name=migrations  file_rec/async:db/migrate<cr>
nnoremap \m :Unite -start-insert -buffer-name=models      file_rec/async:app/models<cr>
nnoremap \c :Unite -start-insert -buffer-name=controllers file_rec/async:app/controllers<cr>
nnoremap \v :Unite -start-insert -buffer-name=views       file_rec/async:app/views<cr>
nnoremap \s :Unite -start-insert -buffer-name=specs       file_rec/async:spec<cr>

" Quick fuzzy
nnoremap <silent> [unite]p :<C-U>Unite -start-insert -buffer-name=files -hide-source-names file_rec/git<CR>

" Quick outline
nnoremap <silent> [unite]o :<C-u>Unite -buffer-name=outline -vertical outline<CR> 

" Unite tags
nnoremap <silent> [unite]t :<C-u>Unite -start-insert -buffer-name=tags tag<CR>

" Quick file search
nnoremap <silent> [unite]f :<C-u>Unite -start-insert -buffer-name=files file_rec/async file/new<CR> 

" Quick snippet
nnoremap <silent> [unite]s :<C-u>Unite -buffer-name=snippets ultisnips<CR> 

" Quick commands
nnoremap <silent> [unite]c :<C-u>Unite -buffer-name=commands command<CR> 

" Quick line
nnoremap <silent> [unite]l :<C-u>Unite -buffer-name=search_file line<CR> 

nnoremap <leader>y :<C-u>Unite history/yank<CR>


" Ctrl-sr: Easier (s)earch and (r)eplace
nnoremap <c-s><c-r> :%s/<c-r><c-w>//gc<left><left><left> 

let g:unite_source_menu_menus = {}

let g:unite_source_menu_menus.filters = {
\'description' : 'Text filters',
\'command_candidates' : [
\  ["Remove empty lines"           , 'v/./d'],
\  ["Remove empty lines  [v]"      , "'<,'>v/./d"],
\  ['Condense empty lines'         , '%s/\n\{3,}/\r\r/e'],
\  ['Remove trailing white space'  , '%s/\s\+$//' ],
\  ['',''],
\]}

let g:unite_source_menu_menus.git = {
\ 'description' : 'Git commands (Fugitive)',
\ 'command_candidates' : [
\  ['git status       (Fugitive)                               '  , 'Gstatus'],
\  ['git diff         (Fugitive)                               '  , 'Gdiff'],
\  ['git commit       (Fugitive)                               '  , 'Gcommit'],
\  ['git log          (Fugitive)                               '  , 'exe "silent Glog | Unite quickfix"'],
\  ['git blame        (Fugitive)                               '  , 'Gblame'],
\  ['git stage        (Fugitive)                               '  , 'Gwrite'],
\  ['git checkout     (Fugitive)                               '  , 'Gread'],
\  ['git rm           (Fugitive)                               '  , 'Gremove'],
\  ['git mv           (Fugitive)                               '  , 'exe "Gmove " input("destino: ")'],
\  ['git push         (Fugitive)                               '  , 'Git! push'],
\  ['git pull         (Fugitive)                               '  , 'Git! pull'],
\  ['git prompt       (Fugitive)                               '  , 'exe "Git! " input("comando git: ")'],
\  ['git cd           (Fugitive)                               '  , 'Gcd'],
\]
\} 

nnoremap <silent> <Leader>g :Unite -start-insert -winheight=10 -buffer-name=menu menu:git<CR>


" }}}


" Airline {{{
" let g:airline_left_sep=''
" let g:airline_right_sep=''
let g:airline_theme='tomorrow'
let g:airline_powerline_fonts = 1


" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#left_sep = ' '
" let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_exclude_preview = 1
" }}}


" " Neocompl {{{
" let g:acp_enableAtStartup = 0
" let g:neocomplete_enable_at_startup = 1
" let g:neocomplete_enable_smart_case = 1
" let g:neocomplete_min_syntax_length = 3

" " Recommended key-mappings.
" " <CR>: close popup and save indent.
" inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
" function! s:my_cr_function()
"   return neocomplete#close_popup() . "\<CR>"
"   " For no inserting <CR> key.
"   "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
" endfunction
" " <TAB>: completion.
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" " <C-h>, <BS>: close popup and delete backword char.
" inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
" inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
" inoremap <expr><C-y>  neocomplete#close_popup()
" inoremap <expr><C-e>  neocomplete#cancel_popup() 

" " SuperTab like snippets behavior.
" imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
" \ "\<Plug>(neosnippet_expand_or_jump)"
" \: pumvisible() ? "\<C-n>" : "\<TAB>"
" smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
" \ "\<Plug>(neosnippet_expand_or_jump)"
" \: "\<TAB>"


" " Plugin key-mappings.
" imap <C-k>     <Plug>(neosnippet_expand_or_jump)
" smap <C-k>     <Plug>(neosnippet_expand_or_jump)
" xmap <C-k>     <Plug>(neosnippet_expand_target)

" " SuperTab like snippets behavior.
" imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
" \ "\<Plug>(neosnippet_expand_or_jump)"
" \: pumvisible() ? "\<C-n>" : "\<TAB>"
" smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
" \ "\<Plug>(neosnippet_expand_or_jump)"
" \: "\<TAB>" 

" " Enable heavy omni completion.
" if !exists('g:neocomplete#sources#omni#input_patterns')
"   let g:neocomplete#sources#omni#input_patterns = {}
" endif 

" " For snippet_complete marker.
" if has('conceal')
" set conceallevel=2 concealcursor=i
" endif
" " }}}

" UltiSnips/YCM {{{
" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>" 
" }}}

" Syntastic {{{
let g:syntastic_quiet_messages = { "level" : "warnings" }
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_scss_checkers = ['scss_lint', 'sass']
" }}}

" Nerdtree {{{
map <leader>l :NERDTreeToggle<CR>
" }}}

" Sandwhich {{{
nmap sc sr
let g:sandwich#recipes = g:sandwich#default_recipes
let g:sandwich#recipes += [
      \   {
      \     'buns'    : ['TagInput(1)', 'TagInput(0)'],
      \     'expr'    : 1,
      \     'kind'    : ['add', 'replace'],
      \     'action'  : ['add'],
      \     'input'   : ['t'],
      \   },
      \   {
      \     'buns'        : ['{ ', ' }'],
      \     'input'       : ['}'],
      \   }, 
      \   {
      \     'buns'        : ['( ', ' )'],
      \     'input'       : [')'],
      \   }, 
      \   {
      \     'buns'    : ['<% ', ' %>'],
      \     'input'   : ['ee'],
      \   },
      \   {
      \     'buns'    : ['<%= ', ' %>'],
      \     'input'   : ['e='],
      \   },
      \ ]

function! TagInput(is_head) abort
  if a:is_head
    let s:TagLast = input('Tag: ')
    if s:TagLast !=# ''
      let tag = printf('<%s>', s:TagLast)
    else
      throw 'OperatorSandwichCancel'
    endif
  else
    let tag = printf('</%s>', matchstr(s:TagLast, '^\a[^[:blank:]>/]*'))
  endif
  return tag
endfunction 

let g:sandwich#recipes += [
      \ ]
" }}}

" Taglist {{{
nnoremap \t :TagbarToggle<CR>
" nnoremap \t :TlistOpen<CR>
let Tlist_Use_Right_Window = 1
let Tlist_Process_File_Always = 1
let Tlist_File_Fold_Auto_Close = 1
let Tlist_Exit_OnlyWindow = 1
" }}}

" Table Mode {{{
nnoremap <silent> <leader>e :TableEvalFormulaLine<CR>
" }}}

" vim-rails {{{
nnoremap <silent> <Leader>c :Econtroller<CR>
nnoremap <silent> <Leader>v :Eview<CR>
nnoremap <silent> <Leader>m :Emodel<CR>
nnoremap <silent> <Leader>t :A<CR>
" }}}

" Fugitive {{{
autocmd BufReadCmd index{,.lock} nmap <buffer> <silent> u -
" }}}

" CTRL SPACE {{{
if executable("ag") 
  let g:ctrlspace_glob_command = 'ag -l --nocolor -g ""'
endif
" let g:CtrlSpaceLoadLastWorkspaceOnStart = 1 
" let g:CtrlSpaceSaveWorkspaceOnExit = 1
" let g:CtrlSpaceSaveWorkspaceOnSwitch = 1
" }}}

" Vim-EasyAlign {{{

" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign) 
" }}}

let g:abolish_save_file = expand('<sfile>')

if !exists(":Abolish")
  finish
endif 

" Abolish {{{
Abolish o{dr,rd}{re,e} order
Abolish ship{p,i}{p,i}ng_co{m,i}{m,i}itm{e,n}{e,n}t shipping_commitment 
" }}}

" Misc {{{
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

nnoremap <silent> <Leader>n :NR<CR>

" let JSHintUpdateWriteOnly=1

let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0

" let g:hybrid_use_Xresources = 1

let g:rehash256 = 1

command! -nargs=0 -bar Update if &modified 
                          \|    if empty(bufname('%'))
                          \|        browse confirm write
                          \|    else
                          \|        confirm write
                          \|    endif
                          \|endif

let g:csexact_term_override = 'tmux.gnome'


let g:webdevicons_enable = 1 

let g:indentLine_char = '│'

nnoremap <F9> :Dispatch<CR>

let g:clever_f_across_no_line = 1
" }}}
