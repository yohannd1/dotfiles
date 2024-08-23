_G.dummy = {}
local vim = _G.vim

-- prepare config dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
local RUNTIME_DIR = assert(vim.env.VIMRUNTIME, "could not get runtime dir")
local USR_DIR = vim.fs.normalize(("%s/../../.."):format(RUNTIME_DIR))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)

local fs_root = vim.fs.normalize(("%s/../../../.."):format(CONF_DIR))
local dotfiles_dir = ("%s/Repos/dotfiles"):format(fs_root)
local plugged_dir = ("%s/Cache/nvim_plugged"):format(fs_root)

vim.g.config_root = CONF_DIR
vim.opt.runtimepath = {
  RUNTIME_DIR,
  ("%s/pack/dist/opt/matchit"):format(RUNTIME_DIR),
  ("%s/lib/nvim"):format(USR_DIR),
  CONF_DIR,
}

local utils = require("cfg.utils")
utils.services.defKeyMenu = function() end -- dummy

require("cfg.general")
require("cfg.keybindings")
require("cfg.filetypes")
require("cfg.statusline")
require("cfg.rifle")

local theme_name = "onedark"
vim.o.termguicolors = true
vim.b.base16_use_true_colors = true
vim.b.base16_true_color_map = utils.loadColorschemeFromYaml(
  ("%s/config/dots/themes/%s.yaml"):format(dotfiles_dir, theme_name)
)
vim.cmd.color("base16")

vim.cmd(("command! ENotes e %s/Repos/PhoneDocs/Pocket/Main.acr"):format(fs_root))

require("cfg.plugins").init({
  plugins = {
    "acrylic.vim", "vim-buftabline", "janet.vim",
    "vim-commentary", "nerdtree", "vim-auto-popmenu",
  },
  root_path = plugged_dir,
})

-- Manually run plugin files (because I suspended it on the command line)
-- For context, :help load-plugins and :help --noplugin
vim.cmd([[ runtime! plugin/**/*.{vim,lua} ]])

vim.g.rifle_mode = "buffer"
vim.g.rifle_split_direction = "right"
vim.o.shell = "dotf.wrap.usb-shell"
