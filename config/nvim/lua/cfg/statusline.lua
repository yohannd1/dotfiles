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

dummy.statusLineGetFiletype = function()
  return (vim.bo.filetype == "") and "no ft" or vim.bo.filetype
end

dummy.statusLineGetMode = function()
  local mode = vim.fn.mode()
  return mode_map[mode] or ("{" .. mode .. "}")
end

dummy.statusLineGetShortName = function()
  -- FIXME: actually use the value of $HOME for this
  local x = vim.fn.expand("%:p"):gsub("^/home/(%w+)", "~")
  local LIMIT = 35
  if #x > LIMIT then
    x = "..." .. x:sub(#x-LIMIT+4, #x)
  end
  return x
end

vim.o.statusline = (
  "%#TabLineSel# %{v:lua.dummy.statusLineGetMode()} " ..
  "%#Normal#%r" ..
  "%#Normal# %{v:lua.dummy.statusLineGetShortName()}" ..
  "%#Normal# %= " ..
  "%#Normal# %{v:lua.dummy.statusLineGetFiletype()} (%{&fileformat}) " ..
  "%#TabLine# %p%% " ..
  "%#TabLineSel# %3l:%-3c "
)
