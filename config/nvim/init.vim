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

if g:is_first
  silent! call plug#begin(g:config_root . '/plugged')

  " " Editing enhancement
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-commentary'
  Plug 'mattn/emmet-vim'
  Plug 'godlygeek/tabular'
  Plug 'ap/vim-buftabline'
  Plug 'tpope/vim-rsi'
  " " Plug 'andymass/vim-matchup'
  Plug 'luochen1990/rainbow'

  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'

  " " :Clap install-binary[!] will always try to compile the binary locally,
  " " if you do care about the disk used for the compilation, try using the force download way,
  " " which will download the prebuilt binary even you have installed cargo.
  " " Plug 'liuchengxu/vim-clap', { 'do': { -> clap#installer#force_download() } }

  " " Editing enhancement: electric pairs
  Plug 'windwp/nvim-autopairs'
  " " Plug 'tmsvg/pear-tree'
  " " Plug 'vim-scripts/AutoClose'
  " " Plug 'jiangmiao/auto-pairs'

  " " Filetypes
  Plug 'Clavelito/indent-sh.vim'
  Plug 'YohananDiamond/zig.vim' " Plug 'ziglang/zig.vim'
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
  Plug 'vim-crystal/vim-crystal'
  Plug 'justinmk/vim-syntax-extra'
  Plug 'Vimjas/vim-python-pep8-indent'
  Plug 'vim-python/python-syntax'
  Plug 'https://gitlab.com/HiPhish/guile.vim'
  Plug 'YohananDiamond/fennel.vim' " fork of 'bakpakin/fennel.vim'
  Plug 'udalov/kotlin-vim'
  Plug 'ollykel/v-vim'
  Plug 'Tetralux/odin.vim'
  Plug 'junegunn/goyo.vim'
  Plug 'YohananDiamond/danmakufu-ph3.vim'
  Plug 'hellerve/carp-vim'
  Plug 'habamax/vim-godot'
  Plug 'janet-lang/janet.vim'
  Plug 'jdonaldson/vaxe'
  Plug 'karolbelina/uxntal.vim'
  " Plug 'stefanos82/nelua.vim'
  " if isdirectory($HOME .. "/pj/code/nelua.vim")
  "   " This repository doesn't actually exist on my GitHub. It's
  "   " currently local and in very early stages (testing `nelua`).
  "   Plug 'YohananDiamond/nelua.vim'
  " endif

  " Plug 'tbastos/vim-lua'
  " Plug 'hylang/vim-hy'
  " Plug 'fsharp/vim-fsharp'
  " Plug 'xolox/vim-lua-ftplugin'
  " Plug 'teal-language/vim-teal'

  " Filetypes - nim
  if executable("nim") && executable("nimsuggest")
    Plug 'YohananDiamond/nvim-nim'
  endif

  " Themes
  if g:is_win
    Plug 'morhetz/gruvbox' " for windows
  endif
  " Plug 'dracula/vim'
  " Plug 'chriskempson/base16-vim'

  " fork of redox-os/ion-vim
  Plug 'https://gitlab.redox-os.org/YohananDiamond/ion-vim'

  " fork of skywind3000/vim-auto-popmenu
  Plug 'YohananDiamond/vim-auto-popmenu'

  " Misc.
  Plug 'tpope/vim-vinegar'
  Plug 'vimwiki/vimwiki' " NOTE: Slowdown candidate
  " " Plug 'itchyny/lightline.vim'

  Plug 'YohananDiamond/vim-hydra'
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'nvim-lua/popup.nvim'
  Plug 'nvim-lua/plenary.nvim'
  " Plug 'nvim-lua/completion-nvim'
  " Plug 'RRethy/vim-illuminate'
  " Plug 'slakkenhuis/vim-margin'

  Plug 'airblade/vim-gitgutter'

  call plug#end()
endif

" }}}
" Plugin Config {{{

" Rainbow
let g:rainbow_active = 1
let g:rainbow_conf = {
    \ "separately": {
        \ "vimwiki": 0,
        \ },
    \ }

" Emmet
let g:user_emmet_leader_key = '<C-x>e'

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
let g:apc_map_enter_backspace = 0
let g:apc_custom_states = {
      \ "clap_input": 0,
      \ }

" Clap command: recent files
" TODO: remove
let g:clap_provider_recent = {
      \ "source": "filehist list | tac",
      \ "sink": "e",
      \ "description": "Load a file from the recent list",
      \ }

" TODO: remove
" Clap command: search on wiki
" {{{
function! _OpenWikiFile(file)
  let file_name = $WIKI .. "/vimwiki/" .. split(a:file)[0] .. ".wiki"
  exec 'e ' .. file_name
endfunction

let g:clap_provider_wiki = {
      \ "source": "acw-list-titles",
      \ "sink": { sel -> _OpenWikiFile(sel) },
      \ "description": "Search on my wiki",
      \ }
" }}}

" TODO: remove
" Clap command: insert from file
" {{{
function! _InsertWikiFileRef(input, after_cursor)
  normal! m`
  exec 'normal! ' .. (a:after_cursor ? 'a' : 'i') .. '[[' .. split(a:input)[0] .. ']]'
  normal! ``
  if a:after_cursor
    normal! l
  endif
endfunction

let g:clap_provider_wiki_iref = {
      \ "source": "acw-list-titles",
      \ "sink": { sel -> _InsertWikiFileRef(sel, v:false) },
      \ "description": "...",
      \ }

let g:clap_provider_wiki_aref = {
      \ "source": "acw-list-titles",
      \ "sink": { sel -> _InsertWikiFileRef(sel, v:true) },
      \ "description": "...",
      \ }
" }}}

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

" Zig.vim
let g:zig_fmt_autosave = 0

" VimWiki
let g:wiki_dir = $HOME .. "/wiki/vimwiki"
let g:vimwiki_list = [{ "path": g:wiki_dir,
                      \ "path_html": "~/.cache/output/vimwiki_html",
                      \ "syntax": "default",
                      \ "ext": ".wiki"}]

let g:vimwiki_map_prefix = "<NOP>"
let g:vimwiki_global_ext = 0
let g:vimwiki_conceallevel = 0
let g:vimwiki_url_maxsave = 0

" Illuminate - delay to highlight words (in millisceconds)
let g:Illuminate_delay = 250

let g:matchup_matchparen_offscreen = {'method': 'popup'}

" :help php-indent
let g:PHP_outdentphpescape = 0
let g:PHP_default_indenting = 0

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
function! VimwikiXToggleItem() " {{{
  let current_line = getline('.')

  let open_square_patt = '\v^(\s*)([*-]\s+)?\[ \]'
  if current_line =~ open_square_patt
    exec 's/' . open_square_patt . '/' . '\1\2[X]'
    nohlsearch
    normal ``
    return
  endif

  let open_round_patt = '\v^(\s*)([*-]\s+)?\( \)'
  if current_line =~ open_round_patt
    exec 's/' . open_round_patt . '/' . '\1\2(X)'
    nohlsearch
    normal ``
    return
  endif

  let closed_square_patt = '\v^(\s*)([*-]\s+)?\[X\]'
  if current_line =~ closed_square_patt
    exec 's/' . closed_square_patt . '/' . '\1\2[ ]'
    nohlsearch
    normal ``
    return
  endif

  let closed_round_patt = '\v^(\s*)([*-]\s+)?\(X\)'
  if current_line =~ closed_round_patt
    exec 's/' . closed_round_patt . '/' . '\1\2( )'
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

  set hidden
  set title
  set backspace=indent,eol,start
  set laststatus=2
  set number relativenumber
  set wildmode=longest,list
  set autoindent
  set hlsearch incsearch
  set linebreak wrap
  set cursorline " line highlighting
  set showcmd
  set shortmess+=atcI
  set belloff+=ctrlg
  set mouse=a
  set display+=lastline
  set complete=.,w,b,u,k,kspell " 'i' was interesting too but it seems too expensive; 't' for no tags
  set completeopt-=preview
  set completeopt+=menuone,noselect
  set noshowmode
  set list
  set nofoldenable
  set cinoptions+=g0
  set cinoptions+=:0
  set scrolloff=3 " scroll ahead :)

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
" Autocommands {{{

" Augroups {{{

augroup extras
  au!
  au TermOpen * setlocal norelativenumber nonumber nocursorline
augroup end

augroup buffer_load
  au!
  au FileType * if has_key(g:, "ft") && has_key(g:ft, &filetype) | call g:ft[&filetype]() | endif
  au FileType * call SetupMakefileRifle()
  au BufNewFile,BufRead * call AddToRecFile()
  au BufEnter * call ApcReenable()
  au BufNewFile,BufRead,BufEnter *.wk set filetype=wk
  au BufNewFile,BufRead,BufEnter *.fx set filetype=c
  au BufNewFile,BufRead,BufEnter *.clj set filetype=clojure
  au BufNewFile,BufRead,BufEnter *.alg set filetype=visualg
  au BufNewFile,BufRead,BufEnter *.jl set filetype=julia
  au BufNewFile,BufRead,BufEnter *.scrbl set filetype=scribble
  au BufNewFile,BufRead,BufEnter *.h set filetype=c
  au BufNewFile,BufRead,BufEnter *.mpp set filetype=cpp
  au BufNewFile,BufRead,BufEnter *.tsx if getline(1) =~ '^<?xml' | set filetype=xml | endif
  au BufNewFile,BufRead,BufEnter calcurse-note.* set filetype=vimwiki
  au BufNewFile,BufRead,BufEnter *.PKGBUILD set filetype=PKGBUILD
augroup end

" }}}
" Filetypes {{{

let g:ft = {}

function! ft.asm() " {{{
  setlocal noet sw=8 ts=8
endfunction! " }}}
function! ft.xdefaults() " {{{
  setlocal commentstring=\!%s
endfunction " }}}
function! ft.c() " {{{
  setlocal noet sw=8 ts=8
  setlocal fdm=syntax
  setlocal commentstring=/*\ %s\ */
  let b:format_command = "clang-multicfg-format c"

  call AddSnippet("s", '#include <stdio.h>')
  call AddSnippet("m", 'int main(void) {<CR><CR>}<Up>')
endfunction " }}}
function! ft.cpp() " {{{
  setlocal fdm=syntax
  setlocal commentstring=//\ %s
  let b:format_command = "clang-multicfg-format cpp"

  call AddSnippet("s", '#include <iostream>')
  call AddSnippet("v", '#include <vector>')
  call AddSnippet("M", '#include <memory>')
  call AddSnippet("m", 'int main() {<CR><CR>}<Up>')
endfunction " }}}
function! ft.markdown() " {{{
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
  setlocal commentstring=<!--\ %s\ -->

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
function! ft.sh() " {{{
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
function! ft.zsh() " {{{
  setlocal tabstop=2 shiftwidth=2 fdm=syntax
endfunction " }}}
function! ft.vim() " {{{
  setlocal fdm=marker tw=72
endfunction " }}}
function! ft.hy() " {{{
  setlocal tabstop=2 shiftwidth=2
endfunction " }}}
function! ft.nim() " {{{
  setlocal sw=2 ts=2 expandtab

  if ReverseRSearch(expand("%:p:h"), '*.nimble')
    let b:rifle_ft = "@nimble"
  else
    let b:rifle_ft = "nim"
  endif
endfunction " }}}
function! ft.nims() " {{{
  call g:ft.nim()
endfunction " }}}
function! ft.ruby() " {{{
  setlocal fdm=syntax
endfunction " }}}
function! ft.haskell() " {{{
  setlocal ts=2 sw=2
endfunction " }}}
function! ft.html() " {{{
  let b:rifle_mode = "silent"
  call AddSnippet("m", "<!DOCTYPE html><CR><html><CR><head><CR><title>Title</title><CR><meta charset=\"UTF-8\"/><CR><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/><CR><link rel=\"stylesheet\" href=\"style.css\"/><CR></head><CR><CR><body><CR><p>Hello, World!</p><CR></body><CR></html><Esc>gg")
endfunction " }}}
function! ft.rust() " {{{
  if ReverseRSearch(expand("%:p:h"), "Cargo.toml")
    let b:rifle_ft = "@cargo"
  else
    let b:rifle_ft = "rust"
  endif

  let b:format_command = "rustfmt"
  setlocal fdm=syntax textwidth=100
endfunction " }}}
function! ft.java() " {{{
  if ReverseRSearch(expand("%:p:h"), "gradlew")
    let b:rifle_ft = "@gradlew"
  endif

  setlocal fdm=syntax
  let b:format_command = "google-java-format --aosp - 2>/dev/null"

  call AddSnippet("m", 'public class Main {<CR>public static void main(String[] args) {<CR>System.out.println("Hello, World!");<CR>}<CR>}<Up><Up><C-o>_')
endfunction " }}}
function! ft.make() " {{{
  setlocal sw=8 ts=8 noet
endfunction " }}}
function! ft.tex() " {{{
  let b:rifle_ft = "tex"
  let b:rifle_mode = "buffer"
  setlocal textwidth=72
endfunction
function! ft.plaintex()
  call g:ft.tex()
endfunction " }}}
function! ft.zig() " {{{
  if ReverseRSearch(expand("%:p:h"), "build.zig")
    let b:rifle_ft = "@zig-build"
  else
    let b:rifle_ft = "zig"
  endif

  let b:format_command = "zig fmt --stdin"
  setlocal textwidth=120

  hi link zigBuiltinFn Special

  call AddSnippet("s", 'const std = @import("std");')
  call AddSnippet("m", 'pub fn main() anyerror!void {<CR><CR>}<Up>')
  call AddSnippet("t", 'test {<CR><CR>}<Up>')
endfunction " }}}
function! ft.python() " {{{
  let b:format_command = "python3 -m black - 2>/dev/null"

  syn keyword Boolean True
  syn keyword Boolean False
  syn keyword Boolean None

  call AddSnippet("m", 'def main():<CR>pass<CR><CR>if __name__ == "__main__":<CR>main()')
  call AddSnippet("c", 'from dataclasses import dataclass')
  call AddSnippet("a", 'from abc import abstractmethod')
endfunction " }}}
function! ft.javascript() " {{{
  let b:format_command = "prettier-stdin"
endfunction " }}}
function! ft.yaml() " {{{
  setlocal sw=2
endfunction " }}}
function! ft.wk() " {{{
  setlocal sw=2
endfunction " }}}
function! ft.moon() " {{{
  setlocal sw=2
endfunction " }}}
function! ft.vimwiki() " {{{
  setlocal sw=2

  syn match VimwikiXNodeAttr /\v[A-Za-z0-9_.]+\{/
  hi link VimwikiXNodeAttr String

  syn match VimwikiXTag /\v\#([A-Za-z0-9_]+)(\.[A-Za-z0-9_]+)*/
  hi link VimwikiXTag Function

  " syn match VimwikiXListItem /\v^\s*[*-]\s+/
  " hi link VimwikiXListItem VimwikiXDone

  syn match VimwikiXTodo /\v^\s*([*-]\s+)?\[ \]/
  syn match VimwikiXTodo /\v^\s*([*-]\s+)?\( \)/

  syn match VimwikiXDone /\v^\s*([*-]\s+)?\[X\]/
  syn match VimwikiXDone /\v^\s*([*-]\s+)?\(X\)/

  syn match VimwikiDatetime /\v\d{1,2}(:\d{2}){1,2}(AM|PM)?/
  syn match VimwikiDatetime /\v\d{4}\/\d{2}\/\d{2}/
  hi link VimwikiDatetime Special

  syn match VimwikiXHeaderAttr /\v^\s*\%:(custom\.)?[A-Za-z_][A-Za-z0-9_]*/
  hi link VimwikiXHeaderAttr Function

  syn match VimwikiXFuncCall /\v\@[A-Za-z_][A-Za-z0-9_]*/
  hi link VimwikiXFuncCall Function

  syn match VimwikiXFuncSpread /\v\@[A-Za-z_][A-Za-z0-9_]*-\>/
  hi link VimwikiXFuncSpread Function

  silent! nunmap <buffer> o
  silent! nunmap <buffer> O
  silent! nmap <buffer> <C-h> <BS>

  call AddSnippet("j", '%:title Journal for <C-r>=strftime("%Y/%m/%d")<CR>')
  call AddSnippet("t", '%:title ')
endfunction " }}}
function! ft.vlang() " {{{
  setlocal sw=4 ts=4 noet

  let b:format_command = "fmt-wrapper-v"
endfunction " }}}
function! ft.fennel() " {{{
  hi link FennelKeyword String
endfunction " }}}
function! ft.visualg() " {{{
endfunction " }}}
function! ft.gdscript() " {{{
  setlocal noet sw=4 ts=4
endfunction " }}}
function! ft.d() " {{{
  setlocal et sw=4 ts=4
  let b:format_command = "dfmt"
endfunction " }}}
function! ft.json() " {{{
  let b:format_command = "jq ."
endfunction " }}}
function! ft.php() " {{{
  set commentstring=//\ %s
endfunction " }}}
function! ft.apache() " {{{
  setlocal commentstring=#\ %s
endfunction " }}}

" }}}

" }}}
" Mappings {{{

" Define leader keys
nmap <Space> <Leader>
vmap <Space> <Leader>
nmap , <Leader>
vmap , <Leader>

" Key input delays
set timeoutlen=1000 ttimeoutlen=10

" Mouse wheel scrolling
if !g:is_android
  nnoremap <ScrollWheelUp> <C-u>
  nnoremap <ScrollWheelDown> <C-d>
  inoremap <ScrollWheelUp> <Esc><C-u>a
  inoremap <ScrollWheelDown> <Esc><C-d>a

  " TODO: disable visual mode in insert mode
endif

" TODO: visual mode drag when off normal mode
nnoremap <RightMouse> <nop>
nnoremap <LeftMouse> <nop>
inoremap <RightMouse> <nop>
inoremap <LeftMouse> <nop>
nnoremap <C-RightMouse> <RightMouse>
nnoremap <C-LeftMouse> <LeftMouse>
inoremap <C-RightMouse> <RightMouse>
inoremap <C-LeftMouse> <LeftMouse>

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

" Formatting Commands
nnoremap <Leader>bf :FormatBuffer<CR>

" Keybindings to escape the terminal
" tnoremap <silent> <Esc> <C-\><C-n>
tnoremap <silent> <C-w><Esc> <Esc>
tnoremap <silent> <C-w>h <C-\><C-n><C-w>h
tnoremap <silent> <C-w>j <C-\><C-n><C-w>j
tnoremap <silent> <C-w>k <C-\><C-n><C-w>k
tnoremap <silent> <C-w>l <C-\><C-n><C-w>l

" Insert date
inoremap <silent> <C-u> <Nop>
inoremap <silent> <expr> <C-u>d strftime("%Y/%m/%d")

" Use perl-ish regexes (I guess)
nnoremap / /\v\c()<Left>
vnoremap / /\v\c()<Left>
nnoremap ? ?\v\c()<Left>
vnoremap ? ?\v\c()<Left>
nnoremap <Leader>/ /\v()<Left>
vnoremap <Leader>/ /\v()<Left>
nnoremap <Leader>? ?\v()<Left>
vnoremap <Leader>? ?\v()<Left>

" Open a prompt to replace everything in the screen
nnoremap <Leader>s :%s/\v/g<Left><Left>
vnoremap <Leader>s :s/\v/g<Left><Left>
nnoremap <Leader>S :%s/<C-r>///g<Left><Left>
vnoremap <Leader>S :s/<C-r>///g<Left><Left>

" Buffer navigation mappings
nnoremap <silent> <C-j> :call NextBuffer()<CR>
nnoremap <silent> <C-k> :call PrevBuffer()<CR>

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
  let l:query = '\v<(TODO|FIXME|XXX)>'
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


" A join command similar to the one in emacs (or evil-mode, idk)
" {{{
function! TheBetterJoin()
  " Go to the end of the current line and set a mark there
  normal! $
  normal! m`

  " Remove trailing whitespace on the current line
  normal! V:s/\s\+$//e\<CR>

  " Actually join the lines
  normal! J

  " Remove trailing whitespace, again...
  normal! V:s/\s\+$//e\<CR>

  " Go to that mark we just set, and move one character to the right, if
  " possible
  normal! ``
  normal! l

  " Remove extra whitespace that gets generated for some reason, but
  " only if what's after the whitespace is a delimiter or the end of the
  " line.
  call SpecialRemoveWhitespace()
endfunction

function! SpecialRemoveWhitespace()
  if (GetLineToEnd() =~ '\v^\s+[()\[\]{}]') || (GetLineToEnd() =~ '\v^\s+$') || (GetCharAt('.', col('.') - 1) =~ '\v[(\[{<]')
    normal dw
  elseif (GetLineToEnd() =~ '\v\s{2,}')
    exec 'normal dwi '
  endif
endfunction

function! TheBetterVisualJoin()
  let line_start = getpos("'<")[1]
  let line_end = getpos("'>")[1]
  let line_diff = line_end - line_start

  exec 'normal ' .. "\<Esc>" .. line_start .. 'G'

  for _ in range(line_diff)
    call TheBetterJoin()
  endfor

  exec 'normal ' .. line_start .. 'G'
endfunction

nnoremap <silent> J :call TheBetterJoin()<CR>
vnoremap <silent> J :call TheBetterVisualJoin()<CR>
" }}}

" Wiki - Toggle bullet items
nnoremap <Leader>, :call VimwikiXToggleItem()<CR>

" Vimwiki hydra
" {{{
" Wiki - create file, insert it inline and go to the new file
function! Vimwiki_NewFileAddRef(after_cursor)
  while v:true
    let title = WikiGenTitle()
    let file_path = g:wiki_dir .. "/" .. title .. ".wiki"

    if filereadable(file_path)
      continue
    endif

    call _InsertWikiFileRef(title, a:after_cursor)
    exec 'edit ' .. file_path
    break
  endwhile
endfunction

silent call hydra#hydras#register({
      \ "name":           "vimwiki",
      \ "title":          "Vimwiki",
      \ "show":           "popup",
      \ "exit_key":       "q",
      \ "feed_key":       v:false,
      \ "foreign_key":    v:true,
      \ "single_command": v:true,
      \ "position":       "s:bottom_right",
      \ "keymap": [
      \   {
      \     "name": "General",
      \     "keys": [
      \       ["w", "e ~/wiki/vimwiki/index.wiki", "open index"],
      \       ["s", "e ~/wiki/vimwiki/202105021825-E80938.wiki", "open scratchpad"],
      \       ["p", "e ~/wiki/vimwiki/202112271411-5A4961.wiki", "open week plan (2022)"],
      \       ["j", "lua dummy.open_today_journal()", "open today's journal"],
      \       ["o", "lua dummy.open_wiki_file({})", "select a wiki file"],
      \       ["H", "Vimwiki2HTMLBrowse", "compile current & browse"],
      \       ["h", "Vimwiki2HTML", "compile current"],
      \       ["A", "VimwikiAll2HTML", "compile all"],
      \     ]
      \   },
      \   {
      \     "name": "References",
      \     "keys": [
      \       ["R", "lua dummy.insert_wiki_file{after_cursor = false}", "add reference ←"],
      \       ["r", "lua dummy.insert_wiki_file{after_cursor = true}", "add reference →"],
      \       ["N", "call Vimwiki_NewFileAddRef(v:false)", "new note + add reference ←"],
      \       ["n", "call Vimwiki_NewFileAddRef(v:true)", "new note + add reference →"],
      \     ]
      \   },
      \ ]
      \ }
      \ )

nnoremap <silent> <Leader>w :Hydra vimwiki<CR>
" }}}

" Improved file opener
nnoremap gf :call OpenSelected()<CR>

" Toggle virtualedit
" {{{
function! ToggleVirtualEdit()
  if &virtualedit == ""
    setlocal ve=all
    echom "Virtual Edit ON"
  else
    setlocal ve=
    echom "Virtual Edit OFF"
  endif
endfunction

nnoremap <Leader>tv :call ToggleVirtualEdit()<CR>
" }}}

" Okay
nnoremap K <Nop>
vnoremap K <Nop>
nnoremap gK K
vnoremap gK K

" }}}
" Experimental Stuff {{{

au BufRead,BufNewFile *.acw set ft=acw
au BufRead,BufNewFile *.lang set ft=lang

" Telescope builtins
nnoremap <leader>. <cmd>lua require('telescope.builtin').fd()<CR>
nnoremap <leader>o <cmd>lua dummy.open_recent()<CR>

nnoremap <leader>g <cmd>Goyo 130<CR>
nnoremap <leader>G <cmd>Goyo!<CR>
nnoremap <leader>W <cmd>MatchupWhereAmI?<CR>

let g:neovide_transparency = 0.8
let g:neovide_cursor_vfx_mode = "ripple"

nnoremap <Leader>L <cmd>set cursorline!<CR>
nnoremap <Leader>C <cmd>set cursorcolumn!<CR>

nnoremap n /<Up><CR>
nnoremap N ?<Up><CR>

" }}}
" Lua Tunnel {{{

lua require("cfg.initrc")

" }}}
