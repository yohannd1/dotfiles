local dummy = {}
_G.dummy = dummy

local vim = _G.vim

local autopairs = require("nvim-autopairs")
local map_key = vim.api.nvim_set_keymap

autopairs.setup({})
autopairs.enable()

-- Pre-calculate as much as possible to avoid overhead
local esc_code = autopairs.esc("<CR>")
local bs_code = autopairs.esc("<BS>")
local autopairs_cr = autopairs.autopairs_cr
local autopairs_bs = autopairs.autopairs_bs

function dummy.imap_enter_handle()
  if vim.fn.pumvisible() ~= 0 then
    return (
      " " .. bs_code -- ignore the completion menu
      .. autopairs_cr() -- process autopairs
    )
  else
    return autopairs_cr()
  end
end

function dummy.imap_bs_handle()
  return autopairs_bs()
end

map_key("i", "<CR>", "v:lua.dummy.imap_enter_handle()", {expr = true, noremap = true})
map_key("i", "<BS>", "v:lua.dummy.imap_bs_handle()", {expr = true, noremap = true})

-- vim: sw=2 et
