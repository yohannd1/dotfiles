_G.dummy = {}
local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end

exec("colorscheme habamax")

vim.g.is_first = (vim.g.is_first == nil) and 1 or 0

-- prepare config dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)
vim.opt.runtimepath:append { CONF_DIR }
vim.g.config_root = CONF_DIR

exec("command! Notes e " .. CONF_DIR .. "/../../../PhoneDocs/Pocket/Main.acr")

-- bootstrap module system
assert(loadfile(CONF_DIR .. "/lua/prepare.lua"))()
local ucm = _G.useConfModule

local paths_to_add = {CONF_DIR .. "/../../../../Software/Janet/"}
for _, p in ipairs(paths_to_add) do
    if vim.fn.isdirectory(p) ~= 0 then
        vim.env.PATH = vim.env.PATH .. ":" .. p
    end
end

-- load modules
ucm("general")()
ucm("keybindings")()
ucm("filetypes")()
