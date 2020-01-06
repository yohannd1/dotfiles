" Vim / NeoVim Configuration
" Mantainer: YohananDiamond

" Welcome! ----------------------------------- {{{ 

" Welcome to my vim config!

" Special thanks to these people:
" - Steve Losh, for that gigantic vimrc he made (I'll put the link here later);

" Some reminders for myself:
" To see all map modes, :h map-overview

" }}}
" Prologue ----------------------------------- {{{

let INIT_PATH = resolve(expand("<sfile>:p:h"))

" }}}
" Plugin Setup ------------------------------- {{{

" ft. Pathogen
let g:pathogen_disabled = []
if has("python3")
    call add(g:pathogen_disabled, "VimCompletesMe")
else
    call add(g:pathogen_disabled, "deoplete.nvim")
endif
call add(g:pathogen_disabled, "SimpylFold")
exec pathogen#infect()

" }}}
" Plugin Settings ---------------------------- {{{

" Deoplete
let g:deoplete#enable_at_startup = 1

" Lightline
let g:lightline = {
      \ 'colorscheme': 'onedark',
      \ 'active': {
      \   'left': [[ 'mode', 'paste' ], [ 'readonly', 'filename' ]],
      \ },
  \ }

" }}}
" GUI ---------------------------------------- {{{

if has('gui_running')
    set guioptions=agit

    if isdirectory('C:\')
        let &guifont='Fixedsys 9,Ubuntu Mono 12,Fira Code 10.5,Cascadia Code 10.5,Consolas 12,Monospace 12'
    else
        let &guifont='Cascadia Code 10.5,Fira Code 10.5,Ubuntu Mono 12,Consolas 12,Monospace 12'
    endif
endif

" }}}
" Filetype Config ---------------------------- {{{

" Shell {{{

augroup ft_sh
    au!
    au FileType sh setlocal tabstop=4 shiftwidth=4
    au FileType sh set foldmethod=marker
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
    au FileType rust RunfileCommand cargo run "%"
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
    au FileType python set foldmethod=marker " Trying this out for a bit.
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

" }}}
" Mini Plugins ------------------------------- {{{

" Tab or Complete {{{

""" Used when no completion plugin is available.
""" When pressing the tab key, decide if it's needed to complete the current word, or else simply insert the tab key.
""" There is a mapping in the Mappings section for this.

function! TabOrComplete(mode)
    if (col(".") > 1) && !(strcharpart(getline("."), col(".") - 2, 1) =~ '\v[ \t]')
        if (a:mode == 0)
            return "\<C-P>"
        elseif (a:mode == 1)
            return "\<C-N>"
        endif
    else 
        return "\<Tab>"
    endif
endfunction

" }}}
" (Not Implemented) Auto-pair {{{

""" Checks if it's a good idea to insert a matching ')' for '(' and similars.
" function PairNeeded()
" 
" end

" }}}

" }}}
" Commands ----------------------------------- {{{

command! -nargs=0 Reload source $MYVIMRC
command! -nargs=0 WhitespaceMode set list!
command! -nargs=0 WrapMode set wrap!
command! -nargs=0 OpenWORD call OpenWORD()
command! -nargs=* RunfileCommand let b:runfile_command = join([<f-args>], ' ') " Remember to quote '%' for better performance when using this.
command! -nargs=0 RunFile call RunFile()

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
" set completeopt=longest,menuone
set completeopt-=preview
set completeopt+=menuone,noselect
set shortmess+=c
set belloff+=ctrlg
filetype plugin indent on

" Listchars
set listchars=tab:»\ 
set listchars+=space:·
set listchars+=extends:%
set listchars+=precedes:%
set listchars+=eol:$,
set listchars+=trail:~

" Theme-related
syntax on
set background=dark
if !exists("/sdcard") | colorscheme onedark | endif

" Indentation
set tabstop=4 shiftwidth=4
set softtabstop=4
set expandtab
set smarttab

" Fold Expr
set foldtext=MyFoldText()

" Enable RGB colors {{{
set termguicolors

if !has('nvim') && $TERM ==# 'screen-256color'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
" }}}

" }}}
" Functions ---------------------------------- {{{

function! RunFile() " {{{
    if !exists("b:runfile_command")
        echo "[RunFile()]: error: `b:runfile_command` not defined."
    else
        let l:command_string = "terminal " . b:runfile_command
        split | exec "normal! \<C-w>j"
        exec l:command_string
        normal! i
    endif
endfunction " }}}
function! OpenWORD() " {{{
    let l:WORD = expand("<cWORD>")
    exec "!xdg-open " . l:WORD . " &"
endfunction " }}}
function! MyFoldText() " {{{
    let l:tab_char = strpart(' ', shiftwidth())
    let l:line_contents = substitute(getline(v:foldstart), '\t', l:tab_char, 'g')
    let l:line_contents = substitute(l:line_contents, '{{{', '', 'g') " Remove fold marker

    let l:numbers_width = &foldcolumn + &number * &numberwidth
    let l:window_width = winwidth(0) - numbers_width - 1
    let l:folded_lines_number = v:foldend - v:foldstart

    let l:line_contents = strpart(l:line_contents, 0, l:window_width - 2 - len(l:folded_lines_number))
    let l:void_size = l:window_width - len(l:line_contents) - len(folded_lines_number)
    let l:void_char = '·'

    return l:line_contents . repeat(l:void_char, l:void_size) . l:folded_lines_number . 'l   '
endfunction " }}}

" }}}

" General Mappings --------------------------- {{{

" Leader Key
nmap <Space> <Leader>
vmap <Space> <Leader>

" Remove <Esc> delays
set timeoutlen=1000 ttimeoutlen=0

" Mouse Wheel Scrolling
map <ScrollWheelUp> 15<C-Y>
map <ScrollWheelDown> 15<C-E>

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

" Clap!
nnoremap <silent> <C-p> :Clap buffers<CR>
nnoremap <silent> <C-o> :Clap files<CR>
nnoremap <silent> <M-o> :Clap grep<CR>

" }}}
" Quick Editing ------------------------------ {{{

nnoremap <silent> <Leader>ev :e $MYVIMRC<CR>
nnoremap <silent> <Leader>et :e ~/git/personal/todo/todo.tq<CR>
nnoremap <silent> <Leader>ex :e ~/.tmux.conf<CR>
nnoremap <silent> <Leader>es :e ~/git/dotfiles/sync<CR>

" }}}
