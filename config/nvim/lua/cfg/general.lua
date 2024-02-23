local ucm = _G.useConfModule
local dummy = _G.dummy

local utils = ucm("utils")

local exec = function(s) vim.api.nvim_exec(s, false) end

return function()
    local vim_runtime_dir = vim.env.VIMRUNTIME

    -- FIXME: am I doing this right
    -- utils.source_if_present(vim_runtime_dir .. "/delmenu.vim") or utils.source_if_present(vim_runtime_dir .. "/menu.vim")

    vim.o.encoding = "utf-8"
    vim.o.langmenu = "en_US"
    vim.env.LANG = "en_US"

    vim.o.hidden = true
    vim.o.title = true
    vim.o.number = true
    vim.o.relativenumber = true

    exec([[
    set backspace=indent,eol,start
    set laststatus=2
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

    dummy.toggleVirtualEdit = function()
        local v = (vim.o.ve == "") and "all" or ""
        vim.o.ve = v
        exec(string.format([[echomsg "Virtual edit set to '%s'"]], v))
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

    function! AddSnippet(key, data) " {{{
      execute 'nnoremap <silent> <buffer> <Leader>i'.a:key.' i'.a:data.'<Esc>'
    endfunction " }}}
    ]])

    exec("nnoremap <silent> <Leader>f :Hydra extrafind<CR>")
    vim.fn["hydra#hydras#register"] {
        name = "extrafind",
        title = "Extra find",
        show = "popup",
        exit_key = "q",
        feed_key = false,
        foreign_key = true,
        single_command = true,
        position = "s:bottom_right",
        keymap = {{
            name = "In buffer",
            keys = {
                {"t", "lua dummy.findTodos()", "TODOs (in buffer)"},
                {"b", "lua require('telescope.builtin').buffers()", "buffers"},
                {"h", "lua require('telescope.builtin').help_tags()", "help tags"},
            }
        }},
    }

    dummy.findTodos = function()
        local queries = {'<TODO>', '<FIXME>', '<XXX>'}
        for _, q in ipairs(vim.b.todo_queries or {}) do
            table.insert(queries, q)
        end

        local query = string.format("\\v(%s)", table.concat(queries, "|"))

        vim.fn.search(query)
        vim.fn.histadd("/", query)
    end
end
