" vim: fdm=marker sw=2 sts=2 foldenable
"
" Setup {{{

" Check if this is the first time sourcing the file
let g:is_first = exists("g:is_first") ? 0 : 1

if g:is_first
  " Cancel if this is not being sourced by NeoVim
  if !has("nvim")
    echoerr "You are not using NeoVim; this configuration doesn't work properly with Vim."
    finish
  endif

  " Set config locations
  let g:config_root = resolve(expand("<sfile>:p:h"))
  let $VIM_INIT = g:config_root . "/init.vim"
  let $GVIM_INIT = g:config_root . "/ginit.vim"

  " Variables for platforms. This one was pretty stolen.
  let g:is_win = has("win32") || has("win64")
  let g:is_linux = has("unix") && !has("macunix")
  let g:is_mac = has("macunix")
  let g:is_android = isdirectory("/sdcard")
  let g:is_tty = $DISPLAY == "" && !g:is_android
  let g:is_embedded = $NVIM_EMBEDDED_MODE == "1"
endif

" }}}
" Plugins {{{
"
lua require("cfg.plugins").load()

function! _InsertWikiFileRef(input, after_cursor)
  normal! m`
  exec 'normal! ' .. (a:after_cursor ? 'a' : 'i') .. '@ref(' .. split(a:input)[0] .. ')'
  normal! ``
  if a:after_cursor
    normal! l
  endif
endfunction

" Make it so <Esc> cancels clap even while on insert mode
" augroup clap_esc_fix
"   au!
"   au User ClapOnEnter inoremap <silent> <buffer> <Esc> <Esc>:<c-u>call clap#handler#exit()<CR>
"   au User ClapOnExit silent! iunmap <buffer> <Esc>
" augroup end

" let g:clap_open_preview = "never"
" let g:clap_layout = { "relative": "editor" }

if 0
  " So, pear-tree kept undloading the keybindings, so I forced it to reload
  " everytime I switch or load buffers.
  "
  " I fear this might be CPU expensive but since I'm too lazy to figure out
  " what is the bug and pear-tree is one of the best paren match plugins
  " out there I'll just do this.
  " {{{
  function! PearTreeUpdate()
    if get(b:, "pear_tree_enabled", 0)
      imap <buffer> <BS> <Plug>(PearTreeBackspace)
      imap <buffer> <CR> <Plug>(PearTreeExpand)
    endif
  endfunction

  " Buffer autocommands
  augroup pear_tree_reload_on_buffer
    au!
    au BufRead,BufEnter,BufWinEnter * call PearTreeUpdate()
    au User BufSwitch call PearTreeUpdate()
  augroup end

  " Key mappings
  nnoremap <silent> <C-w>h <C-w>h:call PearTreeUpdate()<CR>
  nnoremap <silent> <C-w>j <C-w>j:call PearTreeUpdate()<CR>
  nnoremap <silent> <C-w>k <C-w>k:call PearTreeUpdate()<CR>
  nnoremap <silent> <C-w>l <C-w>l:call PearTreeUpdate()<CR>
  " }}}
endif

" }}}
" Functions {{{

function! MyFoldText() " {{{
  let l:foldmarker = split(&foldmarker, ',')
  let l:tab_char = strpart(' ', shiftwidth())
  let l:line_contents = substitute(getline(v:foldstart), '\t', l:tab_char, 'g')
  let l:line_contents = substitute(l:line_contents, ' *'.l:foldmarker[0].'\d* *', '', 'g')

  let l:numbers_width = &foldcolumn + &number * &numberwidth
  let l:window_width = winwidth(0) - numbers_width - 1
  let l:folded_lines_number = v:foldend - v:foldstart

  let l:line_contents = strpart(l:line_contents, 0, l:window_width - 2 - len(l:folded_lines_number))
  let l:void_size = l:window_width - len(l:line_contents) - len(folded_lines_number)
  let l:void_char = '·'

  return l:line_contents . repeat(l:void_char, l:void_size) . l:folded_lines_number . 'l   '
endfunction " }}}
function! AddBookmark(letter, path) " {{{
  execute 'nnoremap <silent> <Leader>e'.a:letter.' :e '.a:path.'<CR>'
endfunction " }}}
function! AddSnippet(key, data) " {{{
  execute 'nnoremap <silent> <buffer> <Leader>i'.a:key.' i'.a:data.'<Esc>'
endfunction " }}}
function! PagerMode(...) " {{{
  if len(a:000) >= 1
    let &ft = a:1
  endif
  setlocal ts=8 nomod nolist noma timeoutlen=0 nocursorline norelativenumber
  setlocal noshowcmd
  nnoremap <buffer> <silent> d <C-d>
  nnoremap <buffer> <silent> u <C-u>
  nnoremap <buffer> <silent> f <C-f>
  nnoremap <buffer> <silent> b <C-b>
  nnoremap <buffer> <silent> q :q<CR>
  nnoremap <buffer> <silent> j <C-e>
  nnoremap <buffer> <silent> k <C-y>
  normal M
endfunction " }}}
function! PathAppend(...) " {{{
  " Appends to Vim's PATH.
  " Good for subshells, specially on Windows.
  for dir in a:000
    if stridx($PATH, dir) == -1
      let $PATH .= (g:is_win ? ";" : ":")
      let $PATH .= dir
    endif
  endfor
endfunction " }}}
function! ReverseRSearch(basedir, query) " {{{
  let l:current_dir = a:basedir
  while 1
    let l:current_glob = glob(l:current_dir . "/*", 0, 1)
    let l:list_match = ListMatch(l:current_glob, '.*/' .. a:query .. '$') || ListMatch(l:current_glob, '.*\\' .. a:query .. '$')
    if (l:current_dir == "/") || (l:current_dir =~ '^\w:\\$') " *NIX or Windows root directories
      return l:list_match
    else
      if l:list_match
        return 1
      else
        let l:current_dir = fnamemodify(l:current_dir, ":h")
      endif
    endif
  endwhile
endfunction " }}}
function! InList(element, list) " {{{
  for e in a:list
    if e == a:element
      return 1
    endif
  endfor
  return 0
endfunction " }}}
function! ListMatch(list, pattern) " {{{
  for e in a:list
    if e =~ a:pattern
      return 1
    endif
  endfor
  return 0
endfunction " }}}
function! SetupMakefileRifle() " {{{
  if ReverseRSearch(expand("%:p:h"), "Makefile")
    let b:rifle_ft = "@make"
  endif
endfunction " }}}
function! FormatBuffer() " {{{
  if !exists("b:format_command")
    echo "No format command found (should be at b:format_command)."
    return
  endif

  let l:current_buffer_lines = getline(1, "$")
  let l:current_buffer = join(l:current_buffer_lines, "\n")

  let l:infile = tempname()
  call writefile(l:current_buffer_lines, l:infile)

  let l:output = systemlist(b:format_command . "<" . l:infile)

  if v:shell_error != 0
    let g:last_format_err = l:output

    echo "Formatting failed - see buffer for more info"

    if bufname("*Format Errors*") == ""
      split
      wincmd j
      enew
      file *Format Errors*
      setlocal buftype=nofile
      setlocal bufhidden=delete
      setlocal noswapfile
      setlocal nobuflisted
    else
      let l:bufwindow = bufwinid("*Format Errors*")

      if l:bufwindow != -1
        call win_gotoid(l:bufwindow)
      else
        split
        wincmd j
        buffer *Format Errors*
      endif
    endif

    setlocal modifiable
    normal ggdG
    call append("$", "Formatting failed:")
    for line in l:output
      call append("$", "  " . line)
    endfor
    call append("$", "Note - use :ShowFormatErr to show the error again.")
    normal ggdd
    setlocal nomodifiable
  else
    let l:bufwindow = bufwinid("*Format Errors*")

    if l:bufwindow != -1
      call nvim_win_close(l:bufwindow, v:false)
    endif

    if l:output == l:current_buffer_lines
      echo "Buffer already formatted"
    else
      let window_top = line("w0")
      let cline = line(".")
      let ccol = col(".")
      normal ggdG
      call setline(1, l:output)

      exec "normal " . window_top . "Gzt"
      exec "normal " . cline . "G" . ccol "|"
    endif
  endif

  call delete(l:infile)
endfunction " }}}
function! ShowFormatErr() " {{{
  if !exists("g:last_format_err")
    echo "No format errors."
    return
  endif

  for line in g:last_format_err
    echo line
  endfor
endfunction " }}}
function! AddToRecFile() " {{{
  let l:path = expand("%:p")

  if l:path == ""
    return
  else
    let l:pid = jobstart(["filehist", "add", l:path])

    if l:pid == -1
      " `filehist` probably doesn't exist - let's ignore this then
      return
    endif
  endif
endfunction " }}}
function! ApcReenable() " {{{
  if get(b:, "apc_enable", 0) == 1
    ApcEnable
  endif
endfunction " }}}
function! NextBuffer() " {{{
  bnext
  silent doautocmd User BufSwitch
endfunction " }}}
function! PrevBuffer() " {{{
  bprevious
  silent doautocmd User BufSwitch
endfunction " }}}
function! GetCurrentChar() " {{{
  " TODO: refactor into a char utilities file
  return strcharpart(getline('.')[col('.') - 1:], 0, 1)
endfunction! " }}}
function! GetLineToEnd() " {{{
  return getline('.')[col('.') - 1:]
endfunction! " }}}
function! GetCharAt(line, col) " {{{
  return strcharpart(getline(a:line)[a:col - 1:], 0, 1)
endfunction! " }}}
function! GetURL(string) " {{{
  return matchstr(a:string, '\v(https?|www\.)://[a-zA-Z0-9/\-\.%_?#=&+~:]+')
endfunction " }}}
function! GetFile(string) " {{{
  return matchstr(a:string, '\v[a-zA-Z0-9_\-\./]+')
endfunction " }}}
function! ReadLine(prompt) " {{{
  call inputsave()
  let line = input(a:prompt)
  call inputrestore()

  echo ""
  return line
endfunction " }}}
function! OpenSelected() " {{{
  let current_word = expand("<cWORD>")

  let url = GetURL(current_word)
  if len(url) != 0
    let browser = $BROWSER

    echo "Found URL: " .. url
    if browser == ""
      echoerr "Could not find a suitable browser to open (set it via $BROWSER)"
    endif

    if confirm("Open using a browser?", "&Yes\n&No") == 1
      call jobstart([browser, url])
    endif

    return
  endif

  let file = GetFile(current_word)
  if len(file) != 0
    echo "Found file: " .. file

    if confirm("Open in a new buffer", "&Yes\n&No") == 1
      exec 'edit ' .. file
    endif

    return
  endif

  echo "Could not find a suitable file or url in the current WORD"
endfunction " }}}
function! WikiGenTitle() " {{{
  return strftime("%Y%m%d%H%M-") .. trim(system("hexdump -n 3 -e '4/4 \"%08X\" 1 \"\\n\"' /dev/random | cut -c 3-"))
endfunction " }}}
function! Item_ToggleTodo() " {{{
  let l:Func = exists("b:item_toggletodo_func") ? b:item_toggletodo_func : funcref("Item_Default_ToggleTodo")
  call l:Func()
endfunction " }}}
function! Item_Default_ToggleTodo() " {{{
  let current_line = getline('.')

  let preferred_done = exists("b:item_toggletodo_preferred_done") 
        \ ? b:item_toggletodo_preferred_done
        \ : "x"

  let open_square_patt = '\v^(\s*)([*-]*)(\s*)\[ \]'
  if current_line =~ open_square_patt
    exec 's/' . open_square_patt . '/' . '\1\2\3[' . preferred_done . ']'
    nohlsearch
    normal ``
    return
  endif

  let open_round_patt = '\v^(\s*)([*-]*)(\s*)\( \)'
  if current_line =~ open_round_patt
    exec 's/' . open_round_patt . '/' . '\1\2\3(' . preferred_done . ')'
    nohlsearch
    normal ``
    return
  endif

  let closed_square_patt = '\v^(\s*)([*-]*)(\s*)\[[Xx]\]'
  if current_line =~ closed_square_patt
    exec 's/' . closed_square_patt . '/' . '\1\2\3[ ]'
    nohlsearch
    normal ``
    return
  endif

  let closed_round_patt = '\v^(\s*)([*-]*)(\s*)\([Xx]\)'
  if current_line =~ closed_round_patt
    exec 's/' . closed_round_patt . '/' . '\1\2\3( )'
    nohlsearch
    normal ``
    return
  endif

  echo "No to-do detected on the current line"
endfunction " }}}
if g:is_first | function! SourceIf(...) " {{{
  for path in a:000
    if filereadable(path)
      exec "source ".path
    endif
  endfor
endfunction | endif
" }}}

" }}}
" General {{{

if g:is_first
  call AddBookmark("v", '$VIM_INIT') " I can put the environment variable quoted because it's evaluated at key press
endif

" }}}
" Commands {{{

command! -nargs=0 Reload call SourceIf($VIM_INIT, $GVIM_INIT)
command! -nargs=* PagerMode call PagerMode(<f-args>)
command! -nargs=0 FormatBuffer call FormatBuffer()
command! -nargs=0 ShowFormatErr call ShowFormatErr()

" Abbreviations
cnoreabbrev rl Reload

" }}}
" Settings {{{

" This wrap might make reloading with :Reload less CPU-intensive.
" Put settings that are unlikely to change here.
if g:is_first
  call SourceIf($VIMRUNTIME."/delmenu.vim", $VIMRUNTIME."/menu.vim")

  " TODO: actually do this on the apps/windows.lua or something
  if g:is_win
    set background=light
    colorscheme gruvbox
  else
    colorscheme base16
  endif

  filetype plugin indent on
  set foldtext=MyFoldText()

  " TO-DO Highlighting (and more)
  " Partially stolen from https://github.com/sakshamgupta05/vim-todo-highlight
  if v:true
    let g:annotations_to_highlight = ['TODO', 'FIXME', 'XXX', 'NOTE']

    " creates annotation group and highlight it according to the config
    for name in g:annotations_to_highlight
      let group_name = 'annotation_' . tolower(name)

      " make group for annotation where its pattern matches and is inside comment
      " TODO: make work with strings too
      execute 'augroup ' . group_name
      autocmd!
      execute 'autocmd Syntax * syntax match ' . group_name .
            \ ' /\v\_.<' . name . '>:?/hs=s+1 contained containedin=.*Comment.*'
      execute 'augroup end'

      " highlight the group according to the config
      execute 'hi link ' . group_name . ' Todo'
    endfor
  endif
endif

" }}}
" Mappings {{{

" goyo
nnoremap <silent> <C-x>j :Goyo 120<CR>
au User GoyoEnter nested nnoremap <silent> <buffer> <C-x>j :Goyo!<CR>
au User GoyoLeave nested nnoremap <silent> <buffer> <C-x>j :Goyo 120<CR>

" Rifle commands
" {{{
silent call hydra#hydras#register({
      \ "name":           "rifle",
      \ "title":          "Rifle Commands",
      \ "show":           "popup",
      \ "exit_key":       "q",
      \ "feed_key":       v:false,
      \ "foreign_key":    v:true,
      \ "position":       "s:bottom_right",
      \ "single_command": v:true,
      \ "keymap": [
      \   {
      \     "name": "Commands to run",
      \     "keys": [
      \       ["r", "Rifle 'run'", "run"],
      \       ["b", "Rifle 'build'", "build"],
      \       ["c", "Rifle 'check'", "check"],
      \       ["t", "Rifle 'test'", "test"],
      \     ]
      \   },
      \ ]
      \ }
      \ )

nnoremap <silent> <Leader>r :Hydra rifle<CR>

" nnoremap <silent> <Leader>rr :Rifle "run"<CR>
" nnoremap <silent> <Leader>rc :Rifle "check"<CR>
" nnoremap <silent> <Leader>rb :Rifle "build"<CR>
" nnoremap <silent> <Leader>rt :Rifle "test"<CR>
" }}}

" Soft wrap mappings
" {{{
function! SetSoftWrapBinds(enable)
  for char in ['j', 'k', '0', '$']
    if a:enable
      exec printf("nnoremap <expr> %s (v:count == 0 ? 'g%s' : '%s')", char, char, char)
      exec printf("vnoremap <expr> %s (v:count == 0 ? 'g%s' : '%s')", char, char, char)
      exec printf("nnoremap <expr> g%s (v:count == 0 ? '%s' : 'g%s')", char, char, char)
      exec printf("vnoremap <expr> g%s (v:count == 0 ? '%s' : 'g%s')", char, char, char)
    else
      exec printf("nunmap %s", char)
    endif
  endfor

  let g:soft_wrap_binds_state = a:enable
endfunction

" Initially call the function to enable wrap bindings by default
call SetSoftWrapBinds(v:true)

" Related Commands
command! -nargs=0 SWBindOn call SetSoftWrapBinds(v:true)
command! -nargs=0 SWBindOff call SetSoftWrapBinds(v:false)
command! -nargs=0 SWBindToggle call SetSoftWrapBinds(!get(g:, "soft_wrap_binds_state", v:false))

cnoreabbrev sbt SWBindToggle

" }}}

" Find TO-DO's
" {{{
function! FindTodos()
  let l:queries = ['<TODO>', '<FIXME>', '<XXX>']
  if exists("b:todo_queries")
    let l:queries += b:todo_queries
  endif

  let l:query = '\v(' . l:queries->join("|") . ')'
  let g:_foo = l:query
  exec 'normal! /' . l:query . 'nN'
  call histadd("/", l:query)
endfunction

function! FindSections()
  let l:query = '\v(\[SECTION\])'
  exec 'normal! /' . l:query . 'nN'
  call histadd("/", l:query)
endfunction

nnoremap <silent> <Leader>f :Hydra extrafind<CR>
silent call hydra#hydras#register({
      \ "name":           "extrafind",
      \ "title":          "Extra Find",
      \ "show":           "popup",
      \ "exit_key":       "q",
      \ "feed_key":       v:false,
      \ "foreign_key":    v:true,
      \ "position":       "s:bottom_right",
      \ "single_command": v:true,
      \ "keymap": [
      \   {
      \     "name": "In buffer",
      \     "keys": [
      \       ["t", 'call FindTodos()', "TODOs (universal)"],
      \       ["s", 'call FindSections()', "find [SECTION]s"],
      \     ],
      \   },
      \   {
      \     "name": "Others",
      \     "keys": [
      \       ["b", "lua require('telescope.builtin').buffers()", "buffers"],
      \       ["h", "lua require('telescope.builtin').help_tags()", "help tags"],
      \     ],
      \   },
      \ ]
      \ })
" }}}

" Vimwiki hydra
" {{{
" Wiki - create file, insert it inline and go to the new file
function! Vimwiki_NewFileAddRef(after_cursor)
  let LIMIT = 20
  let i = 0
  while 1
    let title = WikiGenTitle()
    let file_path = g:wiki_dir .. "/" .. title .. ".acr"

    if !filereadable(file_path)
      call _InsertWikiFileRef(title, a:after_cursor)
      exec 'edit ' .. file_path
      return 1
    endif

    let i += 1
    if i > LIMIT
      echo "EXCEEDED LIMIT"
      return -1
    endif
  endwhile
endfunction

lua <<EOF
vim.fn["hydra#hydras#register"] {
  name = "vimwiki",
  title = "Vimwiki",
  show = "popup",
  exit_key = "q",
  feed_key = false,
  foreign_key = true,
  single_command = true,
  position = "s:bottom_right",
  keymap = {
    {
        name = "General",
        keys = {
          {"w", "e ~/wiki/vimwiki/index.acr", "open index"},
          {"s", "e ~/wiki/vimwiki/202105021825-E80938.acr", "open scratchpad"},
          {"p", "e ~/wiki/vimwiki/202212311207-AFDA90.acr", "open week plan (2023)"},
          {"o", "lua dummy.open_wiki_file({})", "select a wiki file"},
          {"O", "lua dummy.open_wiki_file({}, {'acw-get-projects'})", "select a project"},
          {"H", "Vimwiki2HTMLBrowse", "compile current & browse"},
          {"h", "Vimwiki2HTML", "compile current"},
          {"A", "VimwikiAll2HTML", "compile all"},
        },
    },

    {
        name = "References",
        keys = {
          {"R", "lua dummy.insert_wiki_file{after_cursor = false}", "add reference ←"},
          {"r", "lua dummy.insert_wiki_file{after_cursor = true}", "add reference →"},
          {"N", "call Vimwiki_NewFileAddRef(v:false)", "new note + add reference ←"},
          {"n", "call Vimwiki_NewFileAddRef(v:true)", "new note + add reference →"},
        }
    }
  }
}
EOF

" silent call hydra#hydras#register({
"       \ "name":           "vimwiki",
"       \ "title":          "Vimwiki",
"       \ "show":           "popup",
"       \ "exit_key":       "q",
"       \ "feed_key":       v:false,
"       \ "foreign_key":    v:true,
"       \ "single_command": v:true,
"       \ "position":       "s:bottom_right",
"       \ "keymap": [
"       \   {
"       \     "name": "General",
"       \     "keys": [
"       \       ["w", "e ~/wiki/vimwiki/index.wiki", "open index"],
"       \       ["s", "e ~/wiki/vimwiki/202105021825-E80938.wiki", "open scratchpad"],
"       \       ["p", "e ~/wiki/vimwiki/202212311207-AFDA90.wiki", "open week plan (2023)"],
"       \       ["j", "lua dummy.open_today_journal()", "open today's journal"],
"       \       ["o", "lua dummy.open_wiki_file({})", "select a wiki file"],
"       \       ["H", "Vimwiki2HTMLBrowse", "compile current & browse"],
"       \       ["h", "Vimwiki2HTML", "compile current"],
"       \       ["A", "VimwikiAll2HTML", "compile all"],
"       \     ]
"       \   },
"       \   {
"       \     "name": "References",
"       \     "keys": [
"       \       ["R", "lua dummy.insert_wiki_file{after_cursor = false}", "add reference ←"],
"       \       ["r", "lua dummy.insert_wiki_file{after_cursor = true}", "add reference →"],
"       \       ["N", "call Vimwiki_NewFileAddRef(v:false)", "new note + add reference ←"],
"       \       ["n", "call Vimwiki_NewFileAddRef(v:true)", "new note + add reference →"],
"       \     ]
"       \   },
"       \ ]
"       \ }
"       \ )

nnoremap <silent> <Leader>w :Hydra vimwiki<CR>
" }}}

function! ToggleVirtualEdit()
  if &virtualedit == ""
    setlocal ve=all
    echom "Virtual Edit ON"
  else
    setlocal ve=
    echom "Virtual Edit OFF"
  endif
endfunction

map <C-m> <CR>

" }}}
" Experimental Stuff {{{

au BufRead,BufNewFile *.wiki set ft=acrylic
au BufRead,BufNewFile *.lang set ft=lang

" Telescope builtins
nnoremap <leader>. <cmd>lua require('telescope.builtin').fd()<CR>
nnoremap <leader>o <cmd>lua dummy.open_recent()<CR>

nnoremap <leader>g <cmd>Goyo 130<CR>
nnoremap <leader>G <cmd>Goyo!<CR>
nnoremap <leader>W <cmd>MatchupWhereAmI?<CR>

nnoremap <Leader>L <cmd>set cursorline!<CR>
nnoremap <Leader>C <cmd>set cursorcolumn!<CR>

nnoremap n /<Up><CR>
nnoremap N ?<Up><CR>

" }}}
" Lua Tunnel {{{

lua require("cfg.initrc")

" }}}
