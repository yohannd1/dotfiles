_G.dummy = {}
local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end

vim.o.background = "light"
exec("colorscheme gruvbox")

vim.g.is_first = (vim.g.is_first == nil) and 1 or 0

-- prepare config dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)
vim.opt.runtimepath:append { CONF_DIR }
vim.g.config_root = CONF_DIR

-- bootstrap module system
assert(loadfile(CONF_DIR .. "/lua/prepare.lua"))()
local ucm = _G.useConfModule

-- load modules
ucm("general")
ucm("keybindings")
ucm("filetypes")
