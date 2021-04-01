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
endif

" }}}
" Plugins {{{

if g:is_first
  silent! call plug#begin(g:config_root . '/plugged')

  " Editing enhancement
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-commentary'
  Plug 'mattn/emmet-vim'
  Plug 'godlygeek/tabular'
  Plug 'ap/vim-buftabline'
  Plug 'tpope/vim-rsi'

  " :Clap install-binary[!] will always try to compile the binary locally,
  " if you do care about the disk used for the compilation, try using the force download way,
  " which will download the prebuilt binary even you have installed cargo.
  Plug 'liuchengxu/vim-clap', { 'do': { -> clap#installer#force_download() } }

  " Editing enhancement: electric pairs
  Plug 'tmsvg/pear-tree'
  " Plug 'vim-scripts/AutoClose'
  " Plug 'jiangmiao/auto-pairs'

  " Filetypes
  Plug 'Clavelito/indent-sh.vim'
  Plug 'ziglang/zig.vim'
  Plug 'JuliaEditorSupport/julia-vim'
  Plug 'cespare/vim-toml'
  Plug 'neoclide/jsonc.vim'
  Plug 'HerringtonDarkholme/yats.vim'
  Plug 'plasticboy/vim-markdown'
  Plug 'wlangstroth/vim-racket'
  Plug 'vim-scripts/scribble.vim'
  Plug 'neovimhaskell/haskell-vim'
  Plug 'leafo/moonscript-vim'
  Plug 'rust-lang/rust.vim'
  Plug 'raymond-w-ko/vim-lua-indent'
  Plug 'justinmk/vim-syntax-extra'
  Plug 'Vimjas/vim-python-pep8-indent'
  Plug 'vim-python/python-syntax'
  Plug 'https://gitlab.com/HiPhish/guile.vim'
  " Plug 'tbastos/vim-lua'
  " Plug 'hylang/vim-hy'
  " Plug 'fsharp/vim-fsharp'
  " Plug 'xolox/vim-lua-ftplugin'

  " Filetypes - nim
  if executable("nim")
    Plug 'baabelfish/nvim-nim'
  endif

  " Themes
  Plug 'morhetz/gruvbox' " for windows
  " Plug 'dracula/vim'
  " Plug 'chriskempson/base16-vim'

  " Forks
  Plug 'https://gitlab.redox-os.org/YohananDiamond/ion-vim' " fork of redox-os/ion-vim
  Plug 'YohananDiamond/vim-auto-popmenu' " fork of skywind3000/vim-auto-popmenu

  " Misc.
  Plug 'tpope/vim-vinegar'
  " Plug 'itchyny/lightline.vim'

  call plug#end()
endif

" }}}
" Plugin Config {{{

" Emmet
let g:user_emmet_leader_key='<C-y>'

" Markdown
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_folding_style_pythonic = 0
let g:vim_markdown_override_foldtext = 0
let g:vim_markdown_no_extensions_in_markdown = 1
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0

" Gruvbox
let g:gruvbox_bold = 1
let g:gruvbox_italics = 1

" Buftabline
let g:buftabline_indicators = 1

" Rifle
if is_android
  let g:rifle_mode = "buffer"
else
  let g:rifle_mode = "popup"
endif

" vim-auto-popmenu x Clap
let g:apc_default_state = 1
let g:apc_custom_states = {
      \ "clap_input": 0,
      \ }

" Clap recent files command
let g:clap_provider_recent = {
      \ "source": "filehist list | tac",
      \ "sink": "e",
      \ "description": "Load a file from the recent list",
      \ }

" Make it so <Esc> cancels clap even while on insert mode
augroup clap_esc_fix
  au!
  au User ClapOnEnter inoremap <silent> <buffer> <Esc> <Esc>:<c-u>call clap#handler#exit()<CR>
  au User ClapOnExit silent! iunmap <buffer> <Esc>
augroup end

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

" Zig.vim
let g:zig_fmt_autosave = 0

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
  execute 'nnoremap <silent> <Leader>i'.a:key.' i'.a:data.'<Esc>'
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

    set modifiable
    normal ggdG
    call append("$", "Formatting failed:")
    for line in l:output
      call append("$", "  " . line)
    endfor
    call append("$", "Note - use :ShowFormatErr to show the error again.")
    normal ggdd
    " set nomodifiable
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
  set nocursorline
  set showcmd
  set shortmess+=atcI
  set belloff+=ctrlg
  set mouse=a
  set display+=lastline
  set complete=.,w,b,u,t,k
  set completeopt-=preview
  set completeopt+=menuone,noselect
  set noshowmode
  set list
  set nofoldenable
  set cinoptions+=g0
  set cinoptions+=:0

  " That's how the italics work (or not)
  let &t_ZH = "\<Esc>[3m"
  let &t_ZR = "\<Esc>[23m"

  let &autochdir = !g:is_win

  syntax on

  if g:is_win
    set background=light
    colorscheme gruvbox
  else
    colorscheme base16
  endif

  filetype plugin indent on
  set foldtext=MyFoldText()

  " Indentation
  set tabstop=8 " For tab characters, I guess
  set shiftwidth=4 softtabstop=4
  set expandtab smarttab

  " set listchars=tab:»\ ,trail:¬
  set listchars=tab:\ \ ,trail:¬

  " TO-DO Highlighting (and more)
  " Partially stolen from https://github.com/sakshamgupta05/vim-todo-highlight
  if v:true
    let g:annotations_to_highlight = ['TODO', 'FIXME', 'XXX', 'NOTE']

    " creates annotation group and highlight it according to the config
    for name in g:annotations_to_highlight
      let group_name = 'annotation_' . tolower(name)

      " make group for annotation where its pattern matches and is inside comment
      execute 'augroup ' . group_name
      autocmd!
      execute 'autocmd Syntax * syntax match ' . group_name .
            \ ' /\v\_.<' . name . ':?/hs=s+1 containedin=.*Comment.*'
      execute 'augroup end'

      " highlight the group according to the config
      execute 'hi link ' . group_name . ' Todo'
    endfor
  endif
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
  au BufNewFile,BufRead * call AddToRecFile()
  au BufEnter * call ApcReenable()
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
  setlocal noet sw=8 ts=8
  setlocal fdm=syntax
  let b:format_command = "clang-multicfg-format c"

  call AddSnippet("s", '#include <stdio.h>')
  call AddSnippet("m", 'int main(void) {<CR><CR>}<Up>')
endfunction " }}}
function! Ft_cpp() " {{{
  setlocal fdm=syntax
  let b:format_command = "clang-multicfg-format cpp"

  call AddSnippet("s", '#include <iostream>')
  call AddSnippet("m", 'int main() {<CR><CR>}<Up>')
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

  let b:rifle_mode = "silent"
  let b:rifle_ft = "markdown"

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
  setlocal tabstop=2 shiftwidth=2 fdm=syntax
  " let g:is_bash = 1
  " let g:sh_fold_enabled = 0

  " 1 (001): fold functions
  " let g:sh_fold_enabled += 1

  " 2 (010): fold heredoc
  " let g:sh_fold_enabled += 2

  " 4 (100): fold if/for/case/...
  " let g:sh_fold_enabled += 4
endfunction " }}}
function! Ft_zsh() " {{{
  setlocal tabstop=2 shiftwidth=2 fdm=syntax
endfunction " }}}
function! Ft_vim() " {{{
  setlocal fdm=marker tw=72
endfunction " }}}
function! Ft_hy() " {{{
  setlocal tabstop=2 shiftwidth=2
endfunction " }}}
function! Ft_nim() " {{{
  setlocal sw=2 ts=2 expandtab

  if ReverseRSearch(expand("%:p:h"), '*.nimble')
    let b:rifle_ft = "@nimble"
  else
    let b:rifle_ft = "nim"
  endif
endfunction " }}}
function! Ft_nims() " {{{
  call Ft_nim()
endfunction " }}}
function! Ft_ruby() " {{{
  setlocal fdm=syntax
endfunction " }}}
function! Ft_haskell() " {{{
  setlocal ts=2 sw=2
endfunction " }}}
function! Ft_html() " {{{
  let b:rifle_mode = "silent"
endfunction " }}}
function! Ft_rust() " {{{
  if ReverseRSearch(expand("%:p:h"), "Cargo.toml")
    let b:rifle_ft = "@cargo"
  else
    let b:rifle_ft = "rust"
  endif

  let b:format_command = "rustfmt"
  setlocal fdm=syntax textwidth=100
endfunction " }}}
function! Ft_java() " {{{
  if ReverseRSearch(expand("%:p:h"), "gradlew")
    let b:rifle_ft = "@gradlew"
  endif

  setlocal fdm=syntax
endfunction " }}}
function! Ft_make() " {{{
  setlocal sw=8 ts=8 noet
endfunction " }}}
function! Ft_tex() " {{{
  let b:rifle_ft = "tex"
  let b:rifle_mode = "buffer"
  setlocal textwidth=72
endfunction
function! Ft_plaintex()
  call Ft_tex()
endfunction " }}}
function! Ft_zig() " {{{
  if ReverseRSearch(expand("%:p:h"), "build.zig")
    let b:rifle_ft = "@zig-build"
  else
    let b:rifle_ft = "zig"
  endif

  let b:format_command = "zig fmt --stdin"
  setlocal textwidth=120

  call AddSnippet("s", 'const std = @import("std");')
  call AddSnippet("m", 'pub fn main() anyerror!void {<CR><CR>}<Up>')
endfunction " }}}
function! Ft_python() " {{{
  " let b:format_command = "python3 -m black -"

  syn keyword Boolean True
  syn keyword Boolean False
  syn keyword Boolean None

  call AddSnippet("c", 'from dataclasses import dataclass')
  call AddSnippet("a", 'from abc import abstractmethod')
endfunction " }}}
function! Ft_javascript() " {{{
  let b:format_command = "prettier-stdin"
endfunction " }}}
function! Ft_yaml() " {{{
  setlocal sw=2
endfunction " }}}

" }}}

" }}}
" Status Line {{{

let g:mode_map = {
      \ 'n'      : 'NORMAL',
      \ 'no'     : 'NORMAL (OP)',
      \ 'v'      : 'VISUAL',
      \ 'V'      : 'VISUAL LINE',
      \ "\<C-V>" : 'VISUAL BLOCK',
      \ 's'      : 'SELECT',
      \ 'S'      : 'SELECTION LINE',
      \ "\<C-S>" : 'SELECTION BLOCK',
      \ 'i'      : 'INSERT',
      \ 'R'      : 'REPLACE',
      \ 'Rv'     : 'VISUAL REPLACE',
      \ 'c'      : 'COMMAND',
      \ 'cv'     : 'VIM EX',
      \ 'ce'     : 'EX',
      \ 'r'      : 'PROMPT',
      \ 'rm'     : 'MORE',
      \ 'r?'     : 'CONFIRM',
      \ '!'      : 'SHELL',
      \ 't'      : 'TERMINAL',
      \ }

function! SLFiletype()
  return &filetype == "" ? "no ft" : &filetype
endfunction

function! SLGetMode()
  if has_key(g:mode_map, mode())
    return g:mode_map[mode()]
  else
    return "{" . mode() . "}"
  endif
endfunction

let &statusline = ""
      \ . "%#TabLineSel# %{SLGetMode()} "
      \ . "%#Normal# %r "
      \ . "%#Normal# %="
      \ . "%#Normal# %{SLFiletype()} (%{&fileformat}) "
      \ . "%#TabLine# %p%% "
      \ . "%#TabLineSel# %3l:%-3c "

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
    exec mapmode."noremap <silent> <Space>".mapkey.' "+'.mapkey
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
" nnoremap <silent> <Tab> za
" nnoremap <silent> <S-Tab> zm

" Use Tab to Complete or insert spaces
inoremap <silent> <Tab> <C-r>=TabOrComplete(1)<CR>
inoremap <silent> <S-Tab> <C-r>=TabOrComplete(0)<CR>

" Navigate the completion menu with <C-k>, <C-j> and <C-m>
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "<C-k>"
inoremap <expr> <Down> pumvisible() ? "\<C-n>" : "<Down>"
inoremap <expr> <Up> pumvisible() ? "\<C-p>" : "<Up>"
inoremap <expr> <C-m> pumvisible() ? "\<C-y>" : "<C-m>"

" Rifle Commands
nnoremap <silent> <Leader>rr :Rifle "run"<CR>
nnoremap <silent> <Leader>rc :Rifle "check"<CR>
nnoremap <silent> <Leader>rb :Rifle "build"<CR>
nnoremap <silent> <Leader>rt :Rifle "test"<CR>

" Formatting Commands
nnoremap <Leader>bf :FormatBuffer<CR>

" Keybindings to escape the terminal
" tnoremap <silent> <Esc> <C-\><C-n>
tnoremap <silent> <C-w><Esc> <Esc>
tnoremap <silent> <C-w>h <C-\><C-n><C-w>h
tnoremap <silent> <C-w>j <C-\><C-n><C-w>j
tnoremap <silent> <C-w>k <C-\><C-n><C-w>k
tnoremap <silent> <C-w>l <C-\><C-n><C-w>l

" Use perl-ish regexes (I guess)
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v

" Open a prompt to replace everything in the screen
nnoremap <Leader>s :%s/\v/g<Left><Left>
vnoremap <Leader>s :s/\v/g<Left><Left>

" Buffer navigation mappings
nnoremap <silent> <C-j> :call NextBuffer()<CR>
nnoremap <silent> <C-k> :call PrevBuffer()<CR>

" Open a file
nnoremap <Leader>o :Clap recent<CR>

" }}}
