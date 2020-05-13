" vim: fdm=marker sw=2 sts=2
" YohananDiamond's main neovim config file

" Setup {{{

" Check if this is the first time sourcing the file
let g:is_first = exists("g:is_first") ? 0 : 1

if g:is_first
  " Cancel if this is not being sourced by NeoVim
  if !has("nvim")
    echoerr "You are not using NeoVim. Please source this init file with it or remove this bit on the code."
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

  " If I'm at home
  let g:is_home = isdirectory(expand("~/projects"))
endif

" }}}
" Functions {{{

function! SpawnTerminal(command) " {{{
  let l:command_string = "terminal " . a:command
  split | exec "normal! \<C-w>j"
  exec l:command_string
  normal! i
endfunction " }}}
function! MyFoldText() " {{{
  let l:foldmarker = split(&foldmarker, ',')
  let l:tab_char = strpart(' ', shiftwidth())
  let l:line_contents = substitute(getline(v:foldstart), '\t', l:tab_char, 'g')
  let l:line_contents = substitute(l:line_contents, l:foldmarker[0].'\d*', '', 'g')

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
function! TabOrComplete(mode) " {{{
  """ Used when no completion plugin is available.
  """ When pressing the tab key, decide if it's needed to complete the current word, or else simply insert the tab key.
  """ There is a mapping in the Mappings section for this.
  if (col(".") > 1) && !(strcharpart(getline("."), col(".") - 2, 1) =~ '\v[ \t]')
    if (a:mode == 0)
      return "\<C-P>"
    elseif (a:mode == 1)
      return "\<C-N>"
    endif
  else 
    return "\<Tab>"
  endif
endfunction " }}}
function! PagerMode(...) " {{{
  if len(a:000) >= 1
    let &ft = a:1
  endif
  setlocal ts=8 nomod nolist noma timeoutlen=0 nocursorline
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
function! TryCD(...) " {{{
  for dir in a:000
    if isdirectory(dir)
      call LogMessage("Found directory: ".dir)
      exec "cd ".dir
      break
    endif
  endfor
endfunction " }}}
function! TryCustomShell(...) " {{{
  for shell in a:000
    if executable(shell)
      call LogMessage("Found shell: ".shell)
      let g:custom_shell = shell
      break
    endif
  endfor
endfunction " }}}
function! SpawnCustomShell(args) " {{{
  if !exists("g:custom_shell")
    echo "[SpawnCustomShell()]: `g:custom_shell` not defined."
  else
    call SpawnTerminal(g:custom_shell." ".a:args)
  endif
endfunction " }}}}
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
function! LogMessage(message) " {{{
  if !exists("g:messages")
    let g:messages = []
  endif
  call add(g:messages, a:message)
endfunction " }}}
function! ListMessages() " {{{
  if !exists("g:messages") || len(g:messages) == 0
    echo "You have no messages."
    return
  endif
  for message in g:messages
    echo message
  endfor
endfunction " }}}
function! SetupMakefileRifle() " {{{
  if ReverseRSearch(expand("%:p:h"), "Makefile")
    let b:rifle = {}
    let b:rifle.run = "rrsrun 2 Makefile make run"
    let b:rifle.build = "rrsrun 2 Makefile make"
  endif
endfunction " }}}
function! Surround(...) " {{{
  let l:ls = a:000
  let l:len = len(l:ls)

  if l:len == 0
    echoerr "Specify at least one argument"
  elseif l:len == 1
    " Surround l:ls[0] with double quotes
    return '"' . l:ls[0] . '"'
  elseif l:len == 2
    " Surround l:ls[0] with l:ls[1]
    return l:ls[1] . l:ls[0] . l:ls[1]
  elseif l:len == 3
    " Prepend l:ls[1] and append l:ls[2] to l:ls[0]
    return l:ls[1] . l:ls[0] . l:ls[2]
  endif
endfunction " }}}
function! FormatFile() " {{{
  if exists("b:format_command")
    normal mz
    exec "%!" . b:format_command
    normal `z
    normal zz
  else
    echo "Could not find b:format_command."
  endif
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
" General Initialization {{{

if g:is_first
  call AddBookmark("v", '$VIM_INIT') " I can put the environment variable quoted because it'll then be evaluated at key press.

  if g:is_win
    call TryCustomShell($HOME.'\AppData\Local\Programs\Git\bin\sh', 'powershell')
    call TryCD('E:\home', $HOME)
    call PathAppend('C:\Program Files (x86)\CodeBlocks\MinGW\bin')
  endif

endif

" }}}
" Commands {{{

command! -nargs=0 Reload call SourceIf($VIM_INIT, $GVIM_INIT)
command! -nargs=* PagerMode call PagerMode(<f-args>)
command! -nargs=0 CleanWhitespace if &filetype != "markdown" | %s/\v +$//g | else | echo "That wouldn't be a good idea." | endif
command! -nargs=0 FormatFile call FormatFile()

" Abbreviations
cnoreabbrev rl Reload

" }}}
" Plugin Config {{{

if g:is_first
  " Pathogen Config
  let g:pathogen_disabled = []
  call add(g:pathogen_disabled, executable("nim") ? "" : "nvim-nim")
  call pathogen#infect()
endif

" Vim AutoPopMenu
let g:apc_enable_ft = {'*': 1}

" Emmet
let g:user_emmet_leader_key='<C-c>'

" CtrlP
let g:ctrlp_map = '<C-o>'
let g:ctrlp_cmd = 'CtrlP'

" Lightline
let g:lightline = {}
let g:lightline.active = {}
let g:lightline.colorscheme = "gruvbox"
let g:lightline.active.left = [["mode", "paste"], ["readonly", "filename"]]
let g:lightline.active.right = [["lineinfo"], ["percent"], ["fileformat", "fileencoding", "filetype"]]

" Markdown
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_folding_style_pythonic = 0
let g:vim_markdown_override_foldtext = 0
let g:vim_markdown_no_extensions_in_markdown = 1
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0

" Netrw
" let g:netrw_banner = 0
" let g:netrw_winsize = 25
" let g:netrw_browse_split = 4
" let g:netrw_liststyle = 3
" let g:netrw_altv = 1

" Gruvbox
let g:gruvbox_bold = 1
let g:gruvbox_italics = 1

" Buftabline
let g:buftabline_indicators = 1

" QuickScope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

" Rifle
let g:rifle_use_termup = 1

" }}}
" Settings {{{

" This wrap might make reloading with :Reload less CPU-intensive.
" Put settings that are unlikely to change here.
if g:is_first
  let &encoding = "utf-8"
  let &langmenu = "en_US"
  let $LANG = "en_US"
  call SourceIf($VIMRUNTIME."/delmenu.vim", $VIMRUNTIME."/menu.vim")

  set hidden
  set title
  set backspace=indent,eol,start
  set laststatus=2
  set number relativenumber
  set wildmode=longest,list
  set autoindent
  set hlsearch incsearch
  set linebreak wrap
  set cursorline
  set showcmd
  set shortmess+=atcI
  set belloff+=ctrlg
  set mouse=a
  set display+=lastline
  set complete=.,w,b,u,t
  set completeopt-=preview
  set completeopt+=menuone,noselect
  set noshowmode
  set list

  " Try to make italics work
  let &t_ZH = "\<Esc>[3m"
  let &t_ZR = "\<Esc>[23m"

  let &autochdir = !g:is_win

  syntax on
  silent! colorscheme gruvbox
  let &background = g:is_win ? "light" : "dark" " I like to use light backgrounds on windows.
  if g:is_home
    " Disable background if I'm using vim at home, since my terminals
    " are transparent
    hi Normal guibg=NONE ctermbg=NONE
    hi BufTabLineFill guibg=NONE ctermbg=NONE
    hi CursorLine guibg=NONE ctermbg=NONE
    hi CursorLineNr guibg=NONE ctermbg=NONE
  endif

  filetype plugin indent on
  set foldtext=MyFoldText()

  " Indentation
  set tabstop=8 " For tab characters, I guess
  set shiftwidth=4 softtabstop=4
  set expandtab smarttab

  " set listchars=tab:»\ ,trail:¬
  set listchars=tab:\ \ ,trail:¬
endif

" }}}
" Autocommands {{{

" Augroups {{{

augroup extras
  au!
  au FileType xdefaults setlocal commentstring=\!%s
  au TermOpen * setlocal norelativenumber nonumber nocursorline
augroup end

augroup buffer_load
  au!
  au FileType * if exists("*Ft_".&ft) | exec 'call Ft_'.&ft.'()' | endif
  au FileType * call SetupMakefileRifle()
  " au BufNewFile,BufRead,BufEnter * if line('$') > 5000 | syntax off | endif " Not really working well
  au BufNewFile,BufRead,BufEnter *.fx set filetype=c
  au BufNewFile,BufRead,BufEnter *.clj set filetype=clojure
  au BufNewFile,BufRead,BufEnter *.alg set filetype=visualg
  au BufNewFile,BufRead,BufEnter *.jl set filetype=julia
  au BufNewFile,BufRead,BufEnter *.scrbl set filetype=scribble
  au BufNewFile,BufRead,BufEnter *.h set filetype=c
  au BufNewFile,BufRead,BufEnter calcurse-note.* set filetype=markdown
augroup end

" }}}
" Filetypes {{{

function! Ft_c() " {{{
  let b:rifle = {}
  let b:rifle.run = "gcc '%f' -o '%o' && { '%o'; rm '%o'; }"
  setlocal noet sw=8 ts=8
  setlocal fdm=syntax
endfunction " }}}
function! Ft_cpp() " {{{
  let b:rifle = {}
  let b:rifle.run = "g++ '%f' -o '%o' && { '%o'; rm '%o'; }"
  setlocal fdm=syntax
endfunction " }}}
function! Ft_clojure() " {{{
  let b:rifle = {}
  let b:rifle.run = "clojure '%f'"
endfunction " }}}
function! Ft_markdown() " {{{
  " Thanks to https://gist.github.com/olmokramer/feadbf14a055efd46a8e1bf1e4be4447
  let s:bullet = '^\s*\%(\d\+\.\|[-+*]\)'
  function! MarkdownCheckboxToggle(...) abort " {{{
    let c = a:0 ? a:1 : toupper(escape(nr2char(getchar()), '\.*'))

    if c !~ '\p'
      return
    endif

    call search(s:bullet, 'bcW')
    for i in range(v:count1)
      try
        execute 'keeppatterns s/' . s:bullet . '\s\+\[\zs.\ze\]/\=submatch(0) == c ? " " : c/'
      catch /E486/
        execute 'keeppatterns s/' . s:bullet . '\s\zs/[' . c . '] /'
      endtry

      if i < v:count1 - 1 && !search(s:bullet, 'W')
        break
      endif
    endfor
  endfunction " }}}
  function! MarkdownCheckboxRemove() abort " {{{
    call search(s:bullet, 'bcW')
    try
      for i in range(v:count1)
        execute 'keeppatterns s/' . s:bullet . '\s\zs\s*\[.\] //'

        if i < v:count1 - 1 && !search(s:bullet, 'W')
          break
        endif
      endfor
    catch /E486/
      " No checkbox found.
    endtry
  endfunction " }}}

  " Folding code from https://stackoverflow.com/questions/3828606/vim-markdown-folding
  function! MarkdownFoldExpr(lnum)
    for level in range(1, 6)
      if getline(a:lnum) =~ '^'.repeat('#', level).' .*$'
        return '>1'
        " return '>'.level
      endif
    endfo
    return "="
  endfunction

  let b:rifle_use_termup = 0
  let b:rifle = {}
  let b:rifle.run = "runread md-preview '%f'"
  let b:rifle.build = "md-compile '%f' > ~/".expand("%:t:r").".".strftime("%Y-%m-%d").".html"

  setlocal fdm=expr foldexpr=MarkdownFoldExpr(v:lnum)
  setlocal textwidth=72 noautoindent
  setlocal tabstop=2 shiftwidth=2

  command! -buffer MakeHeader exec "normal ggO---\<CR>created: ".strftime("%Y-%m-%d")."\<CR>---\<CR>\<Esc>2k:Tabularize /:\\zs\<Esc>3j"

  nnoremap <Leader>df :TableFormat<CR>
  nnoremap <Leader>d: vap:Tabularize /:\zs<CR>
  vnoremap <Leader>d: :Tabularize /:\zs<CR>
  nnoremap <Leader>dm :MakeHeader<CR>
  nnoremap <silent> <buffer> <Leader>o :call MarkdownCheckboxToggle("x")<CR>
  nnoremap <silent> <buffer> <Leader>O :call MarkdownCheckboxRemove()<CR>
  vnoremap <silent> <buffer> <Leader>o :call MarkdownCheckboxToggle("x")<CR>
  vnoremap <silent> <buffer> <Leader>O :call MarkdownCheckboxRemove()<CR>
endfunction " }}}
function! Ft_sh() " {{{
  let b:rifle = {}
  let b:rifle.run = "if [ -x '%f' ]; then '%f'; else bash '%f'; fi"
  " let g:is_bash = 1
  " let g:sh_fold_enabled = 0
  setlocal tabstop=2 shiftwidth=2 fdm=syntax

  " 1 (001): fold functions
  " let g:sh_fold_enabled += 1

  " 2 (010): fold heredoc
  " let g:sh_fold_enabled += 2

  " 4 (100): fold if/for/case/...
  " let g:sh_fold_enabled += 4
endfunction " }}}
function! Ft_zsh() " {{{
  let b:rifle = {}
  let b:rifle.run = "if [ -x '%f' ]; then '%f'; else zsh '%f'; fi"
  setlocal tabstop=2 shiftwidth=2 fdm=syntax
endfunction " }}}
function! Ft_visualg() " {{{
  setlocal tabstop=4 shiftwidth=4 syntax=c
endfunction " }}}
function! Ft_julia() " {{{
  let b:rifle = {}
  let b:rifle.run = "julia '%f'"
endfunction " }}}
function! Ft_vim() " {{{
  let b:rifle = {}
  let b:rifle.run = "nvim -u '%f'"
  setlocal fdm=marker tw=72
endfunction " }}}
function! Ft_python() " {{{
  let b:rifle = {}
  let b:rifle.run = "python3 '%f'"
endfunction " }}}
function! Ft_hy() " {{{
  let b:rifle = {}
  let b:rifle.run = "hy '%f'"
  setlocal tabstop=2 shiftwidth=2
endfunction " }}}
function! Ft_fsharp() " {{{
  " To be honest I wanted to compile to the .exe and run it... sadly I can't
  " without writing some substitutions script and I'm too lazy.
  let b:rifle = {}
  let b:rifle.run = "fsharpi '%f'"
endfunction " }}}
function! Ft_lua() " {{{
  let b:rifle = {}
  let b:rifle.run = "lua '%f'"
endfunction " }}}
function! Ft_nim() " {{{
  setlocal shiftwidth=2 softtabstop=2
  let b:rifle = {}
  let b:rifle.run = "nim -c -r '%f'"
  let b:rifle.build = "nim -c '%f'"
endfunction " }}}
function! Ft_ruby() " {{{
  let b:rifle = {}
  let b:rifle.run = "ruby '%f'"
  setlocal fdm=syntax
endfunction " }}}
function! Ft_haskell() " {{{
  setlocal ts=2 sw=2
endfunction " }}}
function! Ft_racket() " {{{
  let b:rifle = {}
  let b:rifle.run = "racket '%f'"
endfunction " }}}
function! Ft_scribble() " {{{
  " Nothing lol
endfunction " }}}
function! Ft_html() " {{{
  let b:rifle_use_termup = 0
  let b:rifle = {}
  if g:is_win
    let b:rifle.run = "start %f"
  else
    let b:rifle.run = "OPEN_GUI=1 runread openfork '%f'"
  endif
endfunction " }}}
function! Ft_rust() " {{{
  let b:rifle = {}
  if ReverseRSearch(expand("%:p:h"), "Cargo.toml")
    let b:rifle.run = "cd '".expand("%:p:h")."' && cargo run"
    let b:rifle.build = "cd '".expand("%:p:h")."' && cargo build"
  else
    let b:rifle.run = "rustc '%f' -o '%o' && { '%o'; rm '%o'; }"
    let b:rifle.build = "rustc '%f' -o '%o'"
  endif

  let b:format_command = "rustfmt"
  setlocal fdm=syntax textwidth=100
endfunction " }}}
function! Ft_java() " {{{
  if ReverseRSearch(expand("%:p:h"), "gradlew")
    let b:rifle = {}
    let b:rifle.run = "rrsrun 1 gradlew run"
    let b:rifle.build = "rrsrun 1 gradlew build"
  endif

  setlocal fdm=syntax
endfunction " }}}
function! Ft_make() " {{{
  setlocal sw=8 ts=8 noet
endfunction " }}}
function! Ft_tex() " {{{
  " let b:rifle = {}
  " let l:subst_command = printf('$(basename $(echo %s | sed %s))', Surround("%f", "'"), Surround('s/\.tex$//g', "'"))
  " let b:rifle.run = "pdflatex '%f' -output_directory '%t' && openfork '%t'/" . l:subst_command . ".pdf"
endfunction
function! Ft_plaintex()
  call Ft_tex()
endfunction " }}}

" }}}

" }}}
" Mappings {{{

nmap <Space> <Leader>
vmap <Space> <Leader>

" Key input delays
set timeoutlen=1000 ttimeoutlen=10

" Mouse wheel scrolling
if !g:is_android
  nnoremap <ScrollWheelUp> <C-u>
  nnoremap <ScrollWheelDown> <C-d>
  nnoremap <RightMouse> <nop>
  nnoremap <LeftMouse> <nop>
  inoremap <ScrollWheelUp> <C-u>
  inoremap <ScrollWheelDown> <C-d>
  inoremap <RightMouse> <nop>
  inoremap <LeftMouse> <nop>
endif

" Clipboard versions of keymappings
for mapmode in ['n', 'v']
  for mapkey in split('y Y p P d D x X')
    exec mapmode."noremap <silent> <Leader>".mapkey.' "+'.mapkey
  endfor
endfor

" <Esc> to clear search query
nnoremap <silent> <Esc> :noh<CR>

" Use ccedilla for entering into command modes
nnoremap ç :
vnoremap ç :
nnoremap Ç q:A
vnoremap Ç q:A

" Folding Commands
nnoremap <silent> <Tab> za
nnoremap <silent> <S-Tab> zm

" Use Tab to Complete or insert spaces
inoremap <silent> <Tab> <C-r>=TabOrComplete(1)<CR>
inoremap <silent> <S-Tab> <C-r>=TabOrComplete(0)<CR>

" Navigate the completion menu with <C-k>, <C-j> and <C-m>
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "<C-k>"
inoremap <expr> <C-m> pumvisible() ? "\<C-y>" : "<C-m>"

" Rifle Commands
nnoremap <silent> <Leader>r :Rifle "run"<CR>
nnoremap <silent> <Leader>R :Rifle "build"<CR>
nnoremap <Leader><C-r> :Rifle ""<Left>

" Formatting Commands
nnoremap <Leader>f :FormatFile<CR>

" Escape terminal in nvim
tnoremap <silent> <Esc> <C-\><C-n>
tnoremap <silent> <C-w><Esc> <Esc>
tnoremap <silent> <C-w>h <C-\><C-n><C-w>h
tnoremap <silent> <C-w>j <C-\><C-n><C-w>j
tnoremap <silent> <C-w>k <C-\><C-n><C-w>k
tnoremap <silent> <C-w>l <C-\><C-n><C-w>l

" Use perl-ish regexes (I guess)
nnoremap / /\v
vnoremap / /\v

" Quick character inserts
inoremap <silent> <C-l>d <C-r>=strftime("20%y-%m-%d")<CR>
inoremap <silent> <C-l>o ∘
inoremap <silent> <C-l>l λ
inoremap <silent> <C-l>c ```<CR>```<Up><End><CR>

" Terminal Spawner
nnoremap <leader>K :call SpawnCustomShell("")<CR>

" View messages history
nnoremap <leader>m :call ListMessages()<CR>

" Adjust indentation on the entire file
nnoremap <silent> <leader>= mzgg=G`zzz

" Replace everything in screen with... something
nnoremap <Leader>s :%s/\v/g<Left><Left>
vnoremap <Leader>s :s/\v/g<Left><Left>

" Buffer navigation mappings
nnoremap <silent> <C-j> :bn<CR>
nnoremap <silent> <C-k> :bp<CR>

" }}}
" Finishing {{{

if g:is_first
  if exists("g:messages") && len("g:messages") != 0
    echo "You have unread startup messages."
  endif

  if g:is_tty
    hi Comment ctermfg=2
    hi Folded ctermfg=2
    hi LineNr ctermfg=2
  else
    hi Comment cterm=italic
    hi Folded cterm=italic
    hi LineNr cterm=italic
    hi CursorLineNr cterm=italic
  endif

  hi link SpecialComment Comment
endif

" }}}
