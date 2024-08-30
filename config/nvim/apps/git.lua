local vim = _G.vim

local PACK_DIR = assert(vim.env.PACK_DIR)

vim.cmd.nnoremap("ç :")
vim.o.packpath = vim.o.packpath .. "," .. PACK_DIR
vim.cmd.packadd("vim-fugitive")

vim.cmd.Git()
vim.cmd.wincmd("j")
vim.cmd.wincmd("q")
