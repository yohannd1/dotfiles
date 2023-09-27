local utils = require("cfg.utils")

local exec = function(s) vim.api.nvim_exec(s, false) end

return function()
    local vim_runtime_dir = vim.env.VIMRUNTIME

    -- FIXME: am I doing this right
    -- utils.source_if_present(vim_runtime_dir .. "/delmenu.vim") or utils.source_if_present(vim_runtime_dir .. "/menu.vim")

    vim.o.encoding = "utf-8"
    vim.o.langmenu = "en_US"
    vim.env.LANG = "en_US"

    exec([[
    set hidden
    set title
    set backspace=indent,eol,start
    set laststatus=2
    set number relativenumber
    set wildmenu
    set wildmode=longest:full,full
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

    " Indentation
    set tabstop=8 " For tab characters, I guess
    set shiftwidth=4 softtabstop=4
    set expandtab smarttab

    set listchars=tab:\ \ ,trail:¬

    syntax on

    " That's how the italics work (or not)
    let &t_ZH = "\<Esc>[3m"
    let &t_ZR = "\<Esc>[23m"
    ]])

    vim.o.autochdir = not utils.os.is_windows

    if vim.g.neovide then
        vim.o.guifont = "Cascadia Code:h9"

        vim.g.neovide_transparency = 0.8
        vim.g.neovide_cursor_vfx_mode = "ripple"
    end

    -- me when i copy paste functions into an exec block
    exec([[
    function! NextBuffer() " {{{
      bnext
      silent doautocmd User BufSwitch
    endfunction " }}}
    function! PrevBuffer() " {{{
      bprevious
      silent doautocmd User BufSwitch
    endfunction " }}}
    ]])
end
