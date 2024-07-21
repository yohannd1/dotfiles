_G.dummy = {}
local vim = _G.vim
local exec = utils.exec

-- prepare config dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)
vim.opt.runtimepath:append { CONF_DIR }
vim.g.config_root = CONF_DIR
assert(loadfile(CONF_DIR .. "/lua/prepare.lua"))()

vim.o.background = "light"
exec("colorscheme gruvbox")

-- load modules
require("cfg.general")
require("cfg.keybindings")
require("cfg.filetypes")
