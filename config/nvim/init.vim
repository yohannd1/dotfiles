" NeoVim Configuration File
" Author: YohananDiamond
"
" Note that this config file does not work properly with Vim, due to having
" NeoVim-specific things.

" Functions ---------------------------------- {{{

function! SpawnTerminal(command) " {{{
    let l:command_string = "terminal " . a:command
    split | exec "normal! \<C-w>j"
    exec l:command_string
    normal! i
endfunction " }}}
function! MyFoldText() " {{{
    let l:tab_char = strpart(' ', shiftwidth())
    let l:line_contents = substitute(getline(v:foldstart), '\t', l:tab_char, 'g')
    let l:line_contents = substitute(l:line_contents, '{{{', '', 'g') " Remove fold marker }}}

    let l:numbers_width = &foldcolumn + &number * &numberwidth
    let l:window_width = winwidth(0) - numbers_width - 1
    let l:folded_lines_number = v:foldend - v:foldstart

    let l:line_contents = strpart(l:line_contents, 0, l:window_width - 2 - len(l:folded_lines_number))
    let l:void_size = l:window_width - len(l:line_contents) - len(folded_lines_number)
    let l:void_char = '·'

    return l:line_contents . repeat(l:void_char, l:void_size) . l:folded_lines_number . 'l   '
endfunction " }}}
function! AddBookmark(letter, path) " {{{
    execute 'nnoremap <silent> <Leader>e' . a:letter . ' :e ' . a:path . '<CR>'
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
function! PagerMode() " {{{
    setlocal ft=man ts=8 nomod nolist noma timeoutlen=0 nocursorline
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
            echom "Found directory: ".dir
            exec "cd ".dir
            break
        endif
    endfor
endfunction " }}}
function! TryCustomShell(...) " {{{
    for shell in a:000
        if executable(shell)
            echom "Found shell: ".shell
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
            let $PATH .= (g:is_windows ? ";" : ":")
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

" }}}
" Prelude ------------------------------------ {{{

let g:VIM_CONFIG = resolve(expand("<sfile>:p:h"))
let $MYVIMRC = g:VIM_CONFIG . "/init.vim"
let $MYGVIMRC = g:VIM_CONFIG . "/ginit.vim"
let g:is_windows = isdirectory('C:\')
let g:is_android = isdirectory('/sdcard')
let g:at_home = isdirectory(expand('~/projects/dotfiles')) || $DOTFILES != ""
let g:first_time = exists("g:first_time") ? 0 : 1

if is_windows
    call TryCustomShell($HOME.'\AppData\Local\Programs\Git\bin\sh', 'powershell')
    call TryCD('E:\usb-station\home', '..\..\..\home', $HOME)
    call PathAppend('C:\Program Files (x86)\CodeBlocks\MinGW\bin')
endif

" }}}
" Plugin Config ------------------------------ {{{

" Pathogen {{{

let g:pathogen_disabled = []
" call add(g:pathogen_disabled, has("python3") ? "vim-auto-popmenu" : "deoplete.nvim")
call add(g:pathogen_disabled, "deoplete.nvim")
call add(g:pathogen_disabled, executable("nim") ? "" : "nvim-nim")
call pathogen#infect()

" }}}
" Deoplete {{{

let g:deoplete#enable_at_startup = 1

" }}}
" Vim Auto Popmenu {{{

let g:apc_enable_ft = {'*': 1}

" }}}
" Lightline {{{

let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ 'active': {
      \   'left': [[ 'mode', 'paste' ], [ 'readonly', 'filename' ]],
      \ },
  \ }

" }}}
" Markdown {{{

let g:vim_markdown_frontmatter = 1
let g:vim_markdown_folding_style_pythonic = 0
let g:vim_markdown_override_foldtext = 0
let g:vim_markdown_no_extensions_in_markdown = 1
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0

" }}}
" CtrlP {{{

let g:ctrlp_map = '<C-o>'
let g:ctrlp_cmd = 'CtrlP'

" }}}
" Emmet {{{

let g:user_emmet_leader_key='<C-c>'

" }}}
" Netrw {{{

let g:netrw_banner = 0
let g:netrw_winsize = 25
let g:netrw_browse_split = 4
let g:netrw_liststyle = 3
let g:netrw_altv = 1

" }}}
" Gruvbox {{{

let g:gruvbox_bold = 0
let g:gruvbox_italics = 0

" }}}
" Lexima {{{

if first_time
    " Remove the auto-close-quote rule (for single and double quotes)
    call remove(g:lexima#default_rules, 11)
endif

" Then reload lexima
call lexima#set_default_rules()

" }}}

" }}}
" Filetype Config ---------------------------- {{{

" Shell {{{

augroup ft_sh
    au!
    au FileType sh setlocal tabstop=2 shiftwidth=2
augroup end

" }}}
" VisuAlg {{{

augroup ft_visualg
    au!
    au BufNewFile,BufRead,BufEnter *.alg set filetype=visualg
    au! FileType visualg setlocal tabstop=4 shiftwidth=4
    au FileType visualg set syntax=c " I don't have any syntax files for VisuAlg, so lets' use C syntax.
augroup end

" }}}
" Rust {{{

function! s:MakeRustRifle() " {{{
    if ReverseRSearch(expand("%:p:h"), "Cargo.toml")
        let b:rifle = {"std": {"body": "cargo build", "plus": "cargo run"}}
    else
        let b:rifle = {"std": {"body": "rustc %f -o %o", "plus": "%o"}}
    endif
endfunction " }}}

augroup ft_rust
    au!
    au FileType rust call s:MakeRustRifle()
    au FileType rust set foldmethod=syntax
augroup end 

" }}}
" Julia {{{

augroup ft_julia
    au!
    au BufNewFile,BufRead,BufEnter *.jl set filetype=julia
    au FileType julia let b:rifle = {"std": {"body": "julia '%f'"}}
augroup end 

" }}}
" Clojure {{{

augroup ft_clojure
    au!
    au BufNewFile,BufRead,BufEnter *.clj set filetype=clojure
    au FileType clojure let b:rifle = {"std": {"body": "clojure '%f'"}}
augroup end

" }}}
" VimScript {{{

augroup ft_vim
    au!
    au FileType vim let b:rifle = {"std": {"body": "nvim -u '%f'"}}
    au FileType vim setlocal foldmethod=marker foldmarker={{{,}}}
augroup end

" }}}
" Python {{{

augroup ft_python
    au!
    au FileType python let b:rifle = {"std": {"body": "python3 '%f'"}}
augroup end

" }}}
" Hylang {{{

augroup ft_hy
    au!
    au FileType hy let b:rifle = {"std": {"body": "hy '%f'"}}
    au FileType hy setlocal tabstop=2 shiftwidth=2
augroup end

" }}}
" F# {{{

augroup ft_fsharp
    au!
    " To be honest I wanted to compile to the .exe and run it... sadly I can't
    " without writing some substitutions script and I'm too lazy.
    au FileType fsharp let b:rifle = {"std": {"body": "fsharpi '%f'"}}
augroup end

" }}}
" Lua {{{

augroup ft_lua
    au!
    au FileType lua let b:rifle = {"std": {"body": "lua '%f'"}}
augroup end

" }}}
" Markdown {{{

command! MarkdownMetadata exec "normal ggO---\<CR>created: ".strftime("%Y-%m-%d")."\<CR>---\<CR>\<Esc>2k:Tabularize /:\\zs\<Esc>3j"
command! MarkdownCompile call SpawnTerminal("md-compile " . expand("%") . " > ~/" . expand("%:t:r") . "." . strftime("%Y-%m-%d") . ".html")

augroup ft_markdown
    au!
    au FileType markdown setlocal textwidth=72 nofoldenable noautoindent
    au FileType markdown setlocal tabstop=2 shiftwidth=2
    au FileType markdown let b:rifle = {"std": {"body": "md-preview '%f'"}}
    au FileType markdown nnoremap <Leader>df :TableFormat<CR>
    au FileType markdown nnoremap <Leader>d: vip:Tabularize /:\zs<CR>
    au FileType markdown vnoremap <Leader>d: :Tabularize /:\zs<CR>
    au FileType markdown nnoremap <Leader>dm :MarkdownMetadata<CR>
augroup end

" }}}
" C {{{

let s:cpp_rifle_win = {"body": 'C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -Command g++ \"%f\" -o \"%o\"', "plus": '%o; rm %o'}

augroup ft_c
    au!
    au BufNewFile,BufRead,BufEnter *.fx set filetype=c
    au FileType c let b:rifle = {"std": {"body": 'gcc "%f" -o "%o"', "plus": "%o; rm %o"}, "win": s:cpp_rifle_win}

    " if is_windows
        " au FileType c RfileCmdWin printf('%s -Command gcc \"%%\" \"%s\" && %s; rm %s',
        "                                  \ 'C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe', 
        "                                  \ $HOME . '\tmp_output.exe',
        "                                  \ $HOME . '\tmp_output.exe',
        "                                  \ $HOME . '\tmp_output.exe',
        "                                  \ )
    " endif
augroup end

" }}}
" C++ {{{

augroup ft_cpp
    au!
    au FileType cpp let b:rifle = {"std": {"body": 'g++ "%f" -o "%o"', "plus": "%o; rm %o"}, "win": s:cpp_rifle_win}
    " if is_windows
    "     au FileType cpp RfileCmdWin printf('%s -Command g++ \"%%\" -o \"%s\" && %s; rm %s',
    "                                      \ 'C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe', 
    "                                      \ $HOME . '\tmp_output.exe',
    "                                      \ $HOME . '\tmp_output.exe',
    "                                      \ $HOME . '\tmp_output.exe',
    "                                      \ )
    " endif
augroup end


" }}}
" Nim {{{

augroup ft_nim
    au!
    au Filetype nim setlocal shiftwidth=2 softtabstop=2
    au FileType nim let b:rifle = {"std": {"body": "nim c -r '%f'"}}
augroup end

" }}}
" Ruby {{{

augroup ft_ruby
    au!
    au FileType ruby let b:rifle = {"std": {"body": "ruby '%f'"}}
    au FileType ruby set foldmethod=syntax
augroup end

" }}}
" Common Lisp {{{

augroup ft_lisp
    au!
    au FileType lisp let b:rifle = {"std": {"body": "clisp '%f'"}}
augroup end

" }}}
" Haskell {{{

augroup ft_haskell
    au!
    au FileType haskell setlocal tabstop=2 shiftwidth=2
augroup end

" }}}
" Racket {{{

augroup ft_racket
    au!
    au FileType racket let b:rifle = {"std": {"body": "racket '%f'"}}
augroup end

" }}}
" Scribble {{{

augroup ft_scribble
    au!
    au BufNewFile,BufRead,BufEnter *.scrbl set filetype=scribble
augroup end

" }}}
" HTML {{{

augroup ft_html
    au!
    au FileType html let b:rifle = {"std": {"body": $BROWSER . " '%f'"},
        \ "win": {"body": "start '%f'"}}
augroup end

" }}}
" Extras {{{

au FileType xdefaults setlocal commentstring=\!%s

" }}}

" }}}
" Commands ----------------------------------- {{{

command! -nargs=0 Reload source $MYVIMRC | if exists("g:GuiLoaded") && g:GuiLoaded && exists("$MYGVIMRC") | source $MYGVIMRC | endif
command! -nargs=0 WhitespaceMode set list!
command! -nargs=0 WrapMode set wrap!
command! -nargs=0 OpenWORD call OpenWORD()
command! -nargs=1 RfileCmd let b:runfile_command = eval(<f-args>)
command! -nargs=1 RfileCmdWin let b:runfile_command_win = eval(<f-args>)
command! -nargs=* EditNote call EditNote(join([<f-args>], ' '))
command! -nargs=0 RunFile call RunFile()
command! -nargs=0 PagerMode call PagerMode()

cnoreabbrev rl Reload

" }}}
" Settings ----------------------------------- {{{

" Language-related
set encoding=utf-8
set langmenu=en_US
let $LANG = 'en_US'
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

set hidden " Prevent quitting vim with :q when any buffer is unsaved
set backspace=indent,eol,start
set laststatus=2 " Enable status bar
set number relativenumber " Hybrid numbers on the left edge of the screen
set textwidth=0 wrapmargin=0 " Prevent physical linebreak
set wildmode=longest,list " Bash-like command completion
set autoindent " For keeping the same indentation level on new lines
set hlsearch incsearch " Highlight & incremental search
set mouse=a
set display+=lastline
set virtualedit=
set linebreak
set wrap
set cursorline
set showcmd
set complete=.,w,b,u,t
set completeopt-=preview
set completeopt+=menuone,noselect
set shortmess+=atcI
set belloff+=ctrlg
filetype plugin indent on

" Listchars
set listchars=tab:»\ 
" set listchars+=space:·
" set listchars+=extends:%
" set listchars+=precedes:%
" set listchars+=eol:$,
set listchars+=trail:~

" Theme-related
syntax on
silent! colorscheme desert
silent! colorscheme gruvbox
let &background = is_windows ? "light" : "dark"

" Indentation
set tabstop=8 " For tab characters
set shiftwidth=4 softtabstop=4
set expandtab smarttab

" Fold Expr
set foldtext=MyFoldText()

let &t_ZH = "\<Esc>[3m"
let &t_ZR = "\<Esc>[23m"

augroup meta_terminal
    au!
    au TermOpen * setlocal norelativenumber nocursorline
augroup end

" }}}
" General Mappings --------------------------- {{{

" Leader Key
nmap <Space> <Leader>
vmap <Space> <Leader>

" Remove <Esc> delays
set timeoutlen=1000 ttimeoutlen=0

" Mouse Wheel Scrolling
if !is_android
    map <ScrollWheelUp> 15<C-Y>
    map <ScrollWheelDown> 15<C-E>
    map <RightMouse> <nop>
    map <LeftMouse> <nop>
endif

" Copy to X register
" How this works: the other keybindings usually work; but, if they don't exist or the timeout ends, <Leader> will translate to "+.
nnoremap <silent> <Leader> "+
vnoremap <silent> <Leader> "+

" Clear search query
nnoremap <silent> <Esc> :noh<CR>

" Use ç (from Portuguese/Brazilian keyboard) on normal mode for entering the command mode.
nnoremap ç :
vnoremap ç :
nnoremap Ç q:A
vnoremap Ç q:A
" Folding Commands
nnoremap <silent> <Tab> za
nnoremap <silent> <S-Tab> zm

" Escape terminal in nvim
tnoremap <silent> <Esc> <C-\><C-n>
tnoremap <silent> <C-w><Esc> <Esc>
tnoremap <silent> <C-w>h <C-\><C-n><C-w>h
tnoremap <silent> <C-w>j <C-\><C-n><C-w>j
tnoremap <silent> <C-w>k <C-\><C-n><C-w>k
tnoremap <silent> <C-w>l <C-\><C-n><C-w>l

" Use Tab to Complete or insert spaces
inoremap <silent> <Tab> <C-r>=TabOrComplete(1)<CR>
inoremap <silent> <S-Tab> <C-r>=TabOrComplete(0)<CR>

" Navigate the completion menu with <C-k>, <C-j> and <C-m>
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "<C-k>"
inoremap <expr> <C-m> pumvisible() ? "\<C-y>" : "<C-m>"

" Rifle Commands
nnoremap <silent> <Leader>r :RifleRun<CR>
nnoremap <silent> <Leader>R :Rifle<CR>

" Steve Losh: Panic Button
" (With zz on the end to center)
nnoremap <f9> mzggg?G`zzz

" Toggle Paste Mode
nnoremap <silent> <Leader>tp :set paste!<cr>

" Steve Losh: "use sane regexes"
nnoremap / /\v
vnoremap / /\v

" Insert today's date
inoremap <silent> <C-l> <C-r>=strftime("20%y-%m-%d")<CR>

" Quick character insert
inoremap <C-g>` ```<CR>```<Up><End><CR>

" CtrlP
nnoremap <silent> <C-p> :CtrlPBuffer<CR>

" Terminal Spawner
nnoremap <leader>K :call SpawnCustomShell("")<CR>

" View messages history
nnoremap <leader>m :messages<CR>

" Netrw for browsing when wanted
nnoremap <silent> <leader>= :Vexplore<CR>

" Replace everything in screen with... something
nnoremap <Leader>s :%s//g<Left><Left>

" }}}
" Quick Editing ------------------------------ {{{

call AddBookmark('v', '$MYVIMRC')
if at_home
    call AddBookmark('s', '~/projects/dotfiles/sync')
endif

" }}}
