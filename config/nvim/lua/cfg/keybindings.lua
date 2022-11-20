local utils = require("cfg.utils")

local map = function(m, lhs, rhs, args)
    local args = args or {}
    vim.api.nvim_set_keymap(m, lhs, rhs, args)
end

local forChars = function(chars, fn)
    for c in utils.splitIter(chars, "") do
        fn(c)
    end
end

-- Quick binding arguments
local arg_nr = {noremap = true}
local arg_nr_s = {noremap = true, silent = true}
local arg_s = {silent = true}

return function()
    -- Related config
    vim.o.timeoutlen = 1000
    vim.o.ttimeoutlen = 10

    -- Leader keys
    forChars("nv", function(m)
        map(m, "<Space>", "<Leader>")
        map(m, ",", "<Leader>")
    end)

    -- Mouse wheel scrolling (except on android)
    if not utils.os.is_android then
        map("n", "<ScrollWheelUp>", "<C-u>", arg_nr)
        map("n", "<ScrollWheelDown>", "<C-d>", arg_nr)
        map("i", "<ScrollWheelUp>", "<C-o><C-u>", arg_nr)
        map("i", "<ScrollWheelDown>", "<C-o><C-d>", arg_nr)

        -- TODO: check if this is working
        -- TODO: disable visual mode in insert mode
    end

    -- Disable mouse clicks without modifier key
    forChars("ni", function(m)
        -- TODO: visual mode drag when off normal mode
        map(m, "<LeftMouse>", "<nop>", arg_nr)
        map(m, "<RightMouse>", "<nop>", arg_nr)

        -- NOTE: This is enabled by default already
        -- map(m, "<C-LeftMouse>", "<LeftMouse>", arg_nr)
        -- map(m, "<C-RightMouse>", "<RightMouse>", arg_nr)
    end)

    map("n", "<Esc>", ":noh<CR>", arg_nr_s)

    -- Use ccedilla for entering into command modes
    forChars("nv", function(m)
        map(m, "ç", ":", arg_nr)
        map(m, "Ç", "q:A", arg_nr)
    end)

    -- Clipboard versions of keymappings
    forChars("nv", function(m)
        forChars("yYpPdDxX", function(key)
            map(m, "<Space>" .. key, '"+' .. key, arg_nr_s)
        end)
    end)

    -- Use Tab to complete or insert indent
    map("i", "<Tab>", "<C-r>=TabOrComplete(1)<CR>", arg_nr_s)
    map("i", "<S-Tab>", "<C-r>=TabOrComplete(0)<CR>", arg_nr_s)

    -- Folding commands
    -- " nnoremap <silent> <Tab> za
    -- " nnoremap <silent> <S-Tab> zm

    -- Perl-ish regex searches
    forChars("nv", function(m)
        -- Case insensitive
        map(m, "/", "/\\v\\c()<Left>", arg_nr)
        map(m, "?", "?\\v\\c()<Left>", arg_nr)

        -- Case sensitive
        map(m, "<Leader>/", "/\\v()<Left>", arg_nr)
        map(m, "<Leader>?", "?\\v()<Left>", arg_nr)
    end)

    -- Now for replacing
    map("n", "<Leader>s", [[:%s/\v/g<Left><Left>]], arg_nr)
    map("v", "<Leader>s", [[:s/\v/g<Left><Left>]], arg_nr)
    map("n", "<Leader>S", [[:%s/<C-r>///g<Left><Left>]], arg_nr)
    map("v", "<Leader>S", [[:s/<C-r>///g<Left><Left>]], arg_nr)

    -- I KEEP PRESSING K BUT I DONT WANT HELP
    forChars("nv", function(m)
        map(m, "K", "<Nop>", arg_nr)
        map(m, "gK", "K", arg_nr)
    end)

    -- Toggle virtualedit
    map("n", "<Leader>tv", ":call ToggleVirtualEdit()<CR>", arg_nr)

    -- Improved file opener
    map("n", "gf", ":call OpenSelected()<CR>", arg_nr)

    -- Wiki - toggle bullet items
    map("n", "<Leader>,", ":call VimwikiXToggleItem()<CR>", arg_nr)

    -- Buffer formatting
    map("n", "<Leader>bf", ":FormatBuffer<CR>", arg_nr)

    -- Insert date
    map("i", "<C-u>", "<Nop>", arg_s)
    vim.keymap.set("i", "<C-u>d", function()
        return vim.fn.strftime("%Y/%m/%d")
    end, {expr = true})

    -- Buffer navigation
    map("n", "<C-j>", ":call NextBuffer()<CR>", arg_nr_s)
    map("n", "<C-k>", ":call PrevBuffer()<CR>", arg_nr_s)

    -- Terminal escaping
    map("t", "<C-w><Esc>", "<Esc>", arg_nr_s) -- TODO: this one is not working
    forChars("hjkl", function(l)
        map("t", "<C-w>" .. l, "<C-\\><C-n><C-w>" .. l, arg_nr_s)
    end)

    -- Navigate the completion menu with <C-k>, <C-j> and <C-m>
    do
        local pv_check = function(lhs, rhs)
            return function()
                return vim.fn.pumvisible() == 1 and lhs or rhs
            end
        end

        vim.keymap.set("i", "<C-j>", pv_check("<C-n>", "<C-j>"), {expr = true})
        vim.keymap.set("i", "<C-k>", pv_check("<C-p>", "<C-k>"), {expr = true})
        vim.keymap.set("i", "<Down>", pv_check("<C-n>", "<Down>"), {expr = true})
        vim.keymap.set("i", "<Up>", pv_check("<C-p>", "<Up>"), {expr = true})
        vim.keymap.set("i", "<C-m>", pv_check("<C-y>", "<C-m>"), {expr = true})
    end
end
