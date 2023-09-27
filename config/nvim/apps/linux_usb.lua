_G.dummy = {}
local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end
local forceLoad = function(path) return assert(loadfile(path))() end

vim.g.is_first = (vim.g.is_first == nil) and 1 or 0

-- prepare config dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)
vim.opt.runtimepath:append { CONF_DIR }
vim.g.config_root = CONF_DIR

-- load config modules
local files = {"keybindings.lua", "general.lua"}
for _, file in ipairs(files) do
    forceLoad(CONF_DIR .. "/lua/cfg/" .. file)()
end

-- forceLoad(CONF_DIR .. "/lua/cfg/plugins.lua").load()

exec("colorscheme retrobox")
