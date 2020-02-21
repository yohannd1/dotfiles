" Vim / NeoVim Configuration
" Mantainer: YohananDiamond

" Preparations ------------------------------- {{{

let g:VIM_CONFIG = resolve(expand("<sfile>:p:h"))
let $MYVIMRC = g:VIM_CONFIG . "/init.vim"
let g:is_windows = isdirectory('C:\') ? 1 : 0
let g:is_android = isdirectory('/sdcard') ? 1 : 0
let g:at_home = isdirectory(expand('~/projects/dotfiles')) || $DOTFILES != ""

" }}}
" Plugin Setup ------------------------------- {{{

let g:pathogen_disabled = []
call add(g:pathogen_disabled, has("python3") ? "VimCompletesMe" : "deoplete.nvim")
call add(g:pathogen_disabled, executable("nim") ? "" : "nvim-nim")
call add(g:pathogen_disabled, executable("nnn") ? "" : "nnn.vim")

call pathogen#infect()

" Deoplete
let g:deoplete#enable_at_startup = 1

" Lightline
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'active': {
      \   'left': [[ 'mode', 'paste' ], [ 'readonly', 'filename' ]],
      \ },
  \ }

" nnn.vim
let g:nnn#set_default_mappings = 0
let g:nnn#layout = { 'left': '~20%' }

" vim-markdown
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_override_foldtext = 0
let g:vim_markdown_no_extensions_in_markdown = 1
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0

" CtrlP
let g:ctrlp_map = '<C-p>'
let g:ctrlp_cmd = 'CtrlPBuffer'

" Emmet
let g:user_emmet_leader_key='<C-c>'

" }}}
" GUI Options -------------------------------- {{{

if has('gui_running')
    set guioptions=agit

    let &guifont = is_windows
        \ ? 'Fixedsys 9,Ubuntu Mono 12,Fira Code 10.5,Cascadia Code 10.5,Consolas 12,Monospace 12'
        \ : 'Cascadia Code 10.5,Fira Code 10.5,Ubuntu Mono 12,Consolas 12,Monospace 12'
endif

" }}}
" Filetype Config ---------------------------- {{{

" Shell {{{

augroup ft_sh
    au!
    au FileType sh setlocal tabstop=4 shiftwidth=4
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

augroup ft_rust
    au!
    au FileType rust RunfileCommand RUST_BACKTRACE=1 cargo run "%"
    au FileType rust set foldmethod=syntax
augroup end 

" }}}
" Julia {{{

augroup ft_julia
    au!
    au BufNewFile,BufRead,BufEnter *.jl set filetype=julia
    au FileType julia RunfileCommand julia "%"
augroup end 

" }}}
" Clojure {{{

augroup ft_clojure
    au!
    au BufNewFile,BufRead,BufEnter *.clj set filetype=clojure
    au FileType clojure RunfileCommand clojure "%"
augroup end

" }}}
" VimScript {{{

augroup ft_vim
    au!
    au FileType vim RunfileCommand nvim -u "%"
    au FileType vim setlocal foldmethod=marker foldmarker={{{,}}}
augroup end

" }}}
" Python {{{

augroup ft_python
    au!
    au FileType python RunfileCommand python3 "%"
augroup end

" }}}
" Hylang {{{

augroup ft_hy
    au!
    au FileType hy RunfileCommand hy "%"
    au FileType hy setlocal tabstop=2 shiftwidth=2
augroup end

" }}}
" F# {{{

augroup ft_fsharp
    au!
    " To be honest I wanted to compile to the .exe and run it... sadly I can't
    " without writing some substitutions script and I'm too lazy.
    au FileType fsharp RunfileCommand fsharpi "%"
augroup end

" }}}
" Lua {{{

augroup ft_lua
    au!
    au FileType lua RunfileCommand lua "%"
augroup end

" }}}
" Conf (general) {{{

augroup ft_conf
    au!
augroup end

" }}}
" Markdown {{{

augroup ft_markdown
    au!
    au FileType markdown setlocal textwidth=72
    au FileType markdown command! -buffer Compile call SpawnTerminal("md-compile " . expand("%") . " > ~/" . expand("%:t:r") . "." . strftime("%Y-%m-%d") . ".html")
    au FileType markdown RunfileCommand "md-preview" "%"
    au FileType markdown nnoremap <silent> <Leader>df :TableFormat<CR>
augroup end

" }}}
" C {{{

augroup ft_c
    au!
    au BufNewFile,BufRead,BufEnter *.fx set filetype=c
augroup end

" }}}
" Nim {{{

augroup ft_nim
    au!
    au Filetype nim setlocal shiftwidth=2 softtabstop=2
    au FileType nim RunfileCommand nim c -r "%"
augroup end

" }}}
" Extras {{{
au FileType xdefaults setlocal commentstring=\!%s
" }}}

" }}}
" Commands ----------------------------------- {{{

command! -nargs=0 Reload source $MYVIMRC
command! -nargs=0 WhitespaceMode set list!
command! -nargs=0 WrapMode set wrap!
command! -nargs=0 OpenWORD call OpenWORD()
command! -nargs=* RunfileCommand let b:runfile_command = join([<f-args>], ' ') " Remember to quote '%' for better performance when using this.
command! -nargs=* EditNote call EditNote(join([<f-args>], ' '))
command! -nargs=0 RunFile call RunFile()
command! -nargs=0 PagerMode call PagerMode()

cnoreabbrev rl Reload

" }}}
" Settings ----------------------------------- {{{

set encoding=utf-8
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
" set listchars+=trail:~

" Theme-related
syntax on
set background=dark
silent! colorscheme desert
silent! colorscheme nord

" Indentation
set tabstop=8 " For tab characters
set shiftwidth=4 softtabstop=4
set expandtab smarttab

" Fold Expr
set foldtext=MyFoldText()

let &t_ZH = "\<Esc>[3m"
let &t_ZR = "\<Esc>[23m"

" Enable RGB colors {{{
" if exists('+termguicolors')
"   let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
"   let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
"   set termguicolors
" endif

" if !has('nvim') && $TERM ==# 'screen-256color'
"     let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
"     let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" endif
" }}}

" }}}
" Functions ---------------------------------- {{{

function! SpawnTerminal(command) " {{{
    let l:command_string = "terminal " . a:command
    split | exec "normal! \<C-w>j"
    exec l:command_string
    normal! i
endfunction " }}}
function! RunFile() " {{{
    if !exists("b:runfile_command")
        echo "[RunFile()]: error: `b:runfile_command` not defined."
    else
        call SpawnTerminal(b:runfile_command)
    endif
endfunction " }}}
function! OpenWORD() " {{{
    let l:WORD = expand("<cWORD>")
    exec "!xdg-open " . l:WORD . " &"
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
function! EditNote(filename) " {{{
    let l:new_filename = substitute(a:filename, ' ', '-', 'g')
    let l:new_filename = substitute(l:new_filename, '.*', '\L&', 'g')
    let l:new_filename = substitute(l:new_filename, '\v(!|/)', '', 'g')
    if isdirectory(expand("~/projects/personal/wiki"))
        if (l:new_filename != "")
            exec "e ~/projects/personal/wiki/" . l:new_filename . ".md"
        else
            echo "... No arguments provided."
        endif
    else
        echo "Not found: '~/projects/personal/wiki'. Please create said directory."
    endif
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

" }}}
" General Mappings --------------------------- {{{

" Leader Key
nmap <Space> <Leader>
vmap <Space> <Leader>

" Remove <Esc> delays
set timeoutlen=1000 ttimeoutlen=0

" Mouse Wheel Scrolling
if is_android
else
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
nnoremap <silent> <Leader>l :noh<CR>

" Use ç (from Portuguese/Brazilian keyboard) on normal mode for entering the command mode.
nnoremap ç :
nnoremap Ç q:i

" Folding Commands
nnoremap <silent> <Tab> za
nnoremap <silent> <S-Tab> zm

" Escape terminal in nvim
tnoremap <silent> <Esc> <C-\><C-n>
tnoremap <silent> <Leader><Esc> <Esc>
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

" Run a file
nnoremap <silent> <Leader>r :RunFile<CR>

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

" NnnPicker
nnoremap <leader>n :NnnPicker '%:p:h'<CR>

" }}}
" Quick Editing ------------------------------ {{{

if at_home
    call AddBookmark('v', '$MYVIMRC')
    call AddBookmark('s', '~/projects/dotfiles/sync')
endif

" }}}
