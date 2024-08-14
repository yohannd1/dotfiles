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

exec("colorscheme retrobox")

local fs_root = CONF_DIR .. "/../../../.."

exec(string.format("command! ENotes e %s/Repos/PhoneDocs/Pocket/Main.acr", fs_root))

local plugged_path = fs_root .. "/Cache/nvim_plugged"

require("cfg.plugins").init({
  plugins = { "acrylic.vim", "vim-buftabline", "janet.vim", "vim-commentary", "nerdtree", "vim-auto-popmenu" },
  root_path = plugged_path,
})

-- manually sourcing every needed plugin file because FOR SOME REASON ITS NOT DOING IT BY ITSELF
local runtime_paths = vim.fn.split(vim.o.runtimepath, ",")
for _, rtp in ipairs(runtime_paths) do
  local glob = vim.fn.glob(("%s/plugin/*.vim"):format(rtp), false, true)
  for _, file in ipairs(glob) do
    vim.cmd.source(file)
  end
end

vim.o.shell = "dotf.wrap.usb-shell"
