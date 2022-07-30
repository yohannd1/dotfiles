local utils = require("cfg.utils")
local vim = _G.vim
local dummy = _G.dummy

local mode_map = {
    ["n"]  = "NORMAL",
    ["no"] = "NORMAL (OP)",
    ["v"]  = "VISUAL",
    ["V"]  = "VISUAL LINE",
    ["s"]  = "SELECT",
    ["S"]  = "SELECTION LINE",
    ["i"]  = "INSERT",
    ["R"]  = "REPLACE",
    ["Rv"] = "VISUAL REPLACE",
    ["c"]  = "COMMAND",
    ["cv"] = "VIM EX",
    ["ce"] = "EX",
    ["r"]  = "PROMPT",
    ["rm"] = "MORE",
    ["r?"] = "CONFIRM",
    ["!"]  = "SHELL",
    ["t"]  = "TERMINAL",
    [utils.parseEscapeCode "<C-V>"] = "VISUAL BLOCK",
    [utils.parseEscapeCode "<C-S>"] = "SELECTION BLOCK",
}

function dummy.statusLineGetFiletype()
    return (vim.bo.filetype == "") and "no ft" or vim.bo.filetype
end

function dummy.statusLineGetMode()
    local mode = vim.fn.mode()
    return mode_map[mode] or ("{" .. mode .. "}")
end

return function()
    vim.o.statusline = (
        "%#TabLineSel# %{v:lua.dummy.statusLineGetMode()} " ..
        "%#Normal# %r " ..
        "%#Normal# %= " ..
        "%#Normal# %{v:lua.dummy.statusLineGetFiletype()} (%{&fileformat}) " ..
        "%#TabLine# %p%% " ..
        "%#TabLineSel# %3l:%-3c "
    )
end
