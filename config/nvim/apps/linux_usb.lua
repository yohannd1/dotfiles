_G.dummy = {}
local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end

-- prepare config dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)
vim.opt.runtimepath:append { CONF_DIR }
vim.g.config_root = CONF_DIR

local utils = require("cfg.utils")
utils.services.defKeyMenu = function() end -- dummy

-- load modules
require("cfg.general")
require("cfg.keybindings")
require("cfg.filetypes")
require("cfg.statusline")

-- exec("colorscheme retrobox")

vim.b.base16_gui_colors = true
vim.cmd.color("base16")

local fs_root = CONF_DIR .. "/../../../.."

exec(string.format("command! ENotes e %s/Repos/PhoneDocs/Pocket/Main.acr", fs_root))

local plugged_path = fs_root .. "/Cache/nvim_plugged"

require("cfg.plugins").init({
  plugins = { "acrylic.vim", "vim-buftabline", "janet.vim", "vim-commentary", "nerdtree", "vim-auto-popmenu" },
  root_path = plugged_path,
})

-- Manually run plugin files (because I suspended it on the command line)
-- For context, :help load-plugins and :help --noplugin
vim.cmd([[ runtime! plugin/**/*.{vim,lua} ]])

vim.o.shell = "dotf.wrap.usb-shell"
