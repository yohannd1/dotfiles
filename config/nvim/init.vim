" vim: fdm=marker sw=2 sts=2 foldenable
"
" Cancel if this is not being sourced by NeoVim
if !has("nvim")
  echoerr "You are not using NeoVim; this configuration doesn't work properly with Vim. Sorry!"
  finish
endif

" initialize lua 'dummy' table
lua _G.dummy = {}

" Get config root
let g:config_root = resolve(expand("<sfile>:p:h"))
let g:plugin_path = g:config_root . "/plugged"

" load plugin config
lua require("cfg.plugins").init({ plugins = "all", root_path = vim.g.plugin_path })

" load the rest of the config
lua require("cfg.general")
lua require("cfg.statusline")
lua require("cfg.keybindings")
lua require("cfg.filetypes")
lua require("cfg.rifle")

set notermguicolors
colorscheme base16
