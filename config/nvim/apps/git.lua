local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end

local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h"))

local PACK_DIR = assert(os.getenv("PACK_DIR"))

exec("nnoremap ç :")
vim.o.packpath = vim.o.packpath .. "," .. PACK_DIR
exec("packadd vim-fugitive")

exec("Git")
exec("wincmd j|wincmd q")
