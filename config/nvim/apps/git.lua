local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end

local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h"))

-- local HOME = assert(os.getenv("HOME"))
-- local XDG_CACHE_HOME = os.getenv("XDG_CACHE_HOME") or (HOME .. "/.cache")
-- local PACK_DIR = XDG_CACHE_HOME .. "/nvim/apps/git"
local PACK_DIR = assert(os.getenv("PACK_DIR"))

exec("nnoremap ç :")
vim.o.packpath = vim.o.packpath .. "," .. PACK_DIR
exec("packadd vim-fugitive")

exec("Git")
exec("wincmd j|wincmd q")
