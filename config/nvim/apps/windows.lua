_G.dummy = {}
local vim = _G.vim

-- prepare config dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)
vim.opt.runtimepath:append { CONF_DIR }
vim.g.config_root = CONF_DIR

vim.o.background = "light"

-- load modules
require("cfg.general")
require("cfg.keybindings")
require("cfg.filetypes")
