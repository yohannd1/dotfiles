_G.dummy = {}
local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end
local forceLoad = function(path) return assert(loadfile(path))() end

-- prepare package dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)

-- load config modules
local files = {"keybindings.lua", "general.lua"}
for _, file in ipairs(files) do
    forceLoad(CONF_DIR .. "/lua/cfg/" .. file)()
end

exec("colorscheme retrobox")
