_G.dummy = {}
local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end

exec("colorscheme retrobox")

vim.g.is_first = (vim.g.is_first == nil) and 1 or 0

-- prepare config dir
local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h:h"))
package.path = string.format("%s;%s/lua/?.lua", package.path, CONF_DIR)
vim.opt.runtimepath:append { CONF_DIR }
vim.g.config_root = CONF_DIR

local fs_root = CONF_DIR .. "/../../../.."

exec(string.format("command! ENotes e %s/Repos/PhoneDocs/Pocket/Main.acr", fs_root))

-- bootstrap module system
assert(loadfile(CONF_DIR .. "/lua/prepare.lua"))()
local ucm = _G.useConfModule

-- (I am handling this elsewhere now)
--
-- local paths_to_add = {fs_root .. "/Software/Janet/"}
-- for _, p in ipairs(paths_to_add) do
--     if vim.fn.isdirectory(p) ~= 0 then
--         vim.env.PATH = vim.env.PATH .. ":" .. p
--     end
-- end

-- load modules
ucm("general")
ucm("keybindings")
ucm("filetypes")
ucm("statusline")

local plugged_path = fs_root .. "/Cache/nvim_plugged"

ucm("plugins").init({
    plugins = { "acrylic.vim", "vim-buftabline", "janet.vim", "vim-commentary", "vim-vinegar" },
    root_path = plugged_path,
})

-- buftabline isn't loading by itself... why???
if vim.fn.isdirectory(plugged_path .. "/vim-buftabline") then
    vim.api.nvim_exec(
        string.format([[source %s/vim-buftabline/plugin/buftabline.vim]], plugged_path),
        false
    )
end

vim.o.shell = "dotf.wrap.usb-shell"
