local vim = _G.vim
local dummy = _G.dummy

local utils = require("cfg.utils")
local exec = utils.exec
local map = utils.map
local setGlobals = utils.setGlobals

local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

vim.env.VIM_INIT = vim.g.config_root .. "/init.vim"
vim.env.GVIM_INIT = vim.g.config_root .. "/ginit.vim"
vim.g.is_win = utils.os.is_windows

vim.o.encoding = "utf-8"
vim.o.langmenu = "en_US"
vim.env.LANG = "en_US"

local TAB_WIDTH = 8
local SPACE_WIDTH = 4

setGlobals {
  -- default indent settings
  tabstop = TAB_WIDTH,
  shiftwidth = SPACE_WIDTH,
  softtabstop = SPACE_WIDTH,
  expandtab = true,
  smarttab = true,
  autoindent = true,

  hidden = true,
  title = true,
  number = true,
  relativenumber = true,

  backspace = "indent,eol,start",
  laststatus = 2,

  wildmenu = true,
  wildmode = "longest:full,full",

  -- search options
  hlsearch = true,
  incsearch = true,

  -- word wrap
  linebreak = true,
  wrap = true,

  cursorline = true, -- line highlighting
  showcmd = true, -- show keystrokes & visual mode size

  belloff = "all",
  mouse = "a",

  -- 'i' was interesting too but it seems too expensive; 't' for no tags
  complete = ".,w,b,u,k,kspell",
  completeopt = "menu,menuone,noselect",

  -- show as much of the last line as possible
  display = "lastline",

  list = true,
  listchars = "tab:  ,trail:¬",

  -- no fold by default
  foldenable = false,

  -- don't show mode on the command line
  showmode = false,

  -- cindent default config
  cinoptions = "g0,:0",

  -- scroll ahead :)
  scrolloff = 3,
}

vim.opt.shortmess:append("atCI")

-- attempt to make italics work
exec([[
let &t_ZH = "\<Esc>[3m"
let &t_ZR = "\<Esc>[23m"
]])

vim.cmd("syntax on")
vim.cmd("filetype plugin indent on")

-- only auto-cd if we're not on windows
vim.o.autochdir = not utils.os.is_windows

if vim.g.neovide then
  vim.o.guifont = "Cascadia Code:h9"

  vim.g.neovide_transparency = 0.8
  vim.g.neovide_cursor_vfx_mode = "ripple"
end

exec([[ command! Fgitmerge /\v^(\<{4,}|\={4,}|\>{4,}) ]])

dummy.toggleVirtualEdit = function()
  local value = (vim.o.ve == "") and "all" or ""
  vim.o.ve = value

  local message = string.format("Virtual edit set to '%s'", value)
  vim.api.nvim_echo({{message}}, false, {})
end

dummy.bufSwitch = function(next)
  vim.cmd(next and "bnext" or "bprevious")
  vim.cmd([[ silent doautocmd User BufSwitch ]])
end

-- TODO: improved snippet system (move into separate module ig)
dummy.addSnippet = function(key, data)
  exec(string.format("nnoremap <silent> <buffer> <Leader>i%s i%s<Esc>", key, data))
end

dummy.itemToggleTodo = function()
  local fn = vim.b.item_toggletodo_func or dummy.itemToggleTodoDefault
  fn()
end

dummy.itemToggleTodoVisual = function()
  exec("normal mz") -- mark

  -- get beginning and end lines
  exec("normal '<")
  local l1 = vim.fn.line(".")
  exec("normal '>")
  local l2 = vim.fn.line(".")
  local count = l2 - l1

  exec("normal '<")
  for _ = 1, count do
    dummy.itemToggleTodo()
    exec("normal j")
  end

  exec("normal `z")
end

-- me when i copy paste functions into an exec block
dummy.itemToggleTodoDefault = function()
  local cur_line = vim.fn.getline(".")
  local preferred_done = vim.b.item_toggletodo_preferred_done or "x"

  local trim = vim.fn.trim
  local OPEN_SQUARE_PATT = trim([[ \v^(\s*)([*-]*)(\s*)\[ \] ]])
  local OPEN_ROUND_PATT = trim([[ \v^(\s*)([*-]*)(\s*)\( \) ]])
  local CLOSED_SQUARE_PATT = trim([[ \v^(\s*)([*-]*)(\s*)\[[Xx]\] ]])
  local CLOSED_ROUND_PATT = trim([[ \v^(\s*)([*-]*)(\s*)\([Xx]\) ]])

  local afterReplace = function()
    vim.cmd.nohlsearch()
    vim.cmd("normal! ``")
  end

  local match = vim.fn.match
  if match(cur_line, OPEN_SQUARE_PATT) ~= -1 then
    exec(("s/%s/\\1\\2\\3[%s]"):format(OPEN_SQUARE_PATT, preferred_done))
    afterReplace()
    return
  end

  if match(cur_line, OPEN_ROUND_PATT) ~= -1 then
    exec(("s/%s/\\1\\2\\3(%s)"):format(OPEN_ROUND_PATT, preferred_done))
    afterReplace()
    return
  end

  if match(cur_line, CLOSED_SQUARE_PATT) ~= -1 then
    print("***")
    exec(("s/%s/\\1\\2\\3[ ]"):format(CLOSED_SQUARE_PATT))
    afterReplace()
    return
  end

  if match(cur_line, CLOSED_ROUND_PATT) ~= -1 then
    exec(("s/%s/\\1\\2\\3( )"):format(CLOSED_ROUND_PATT))
    afterReplace()
    return
  end

  print("No to-do detected on the current line")
end

-- autocmd({"TextYankPost"}, {
--   pattern = "*",
--   callback = function()
--     vim.highlight.on_yank({
--       higroup = "Normal",
--     })
--   end
-- })

dummy.findTodos = function()
  local queries = {'<TODO>', '<FIXME>', '<XXX>'}
  for _, q in ipairs(vim.b.todo_queries or {}) do
    table.insert(queries, q)
  end

  local query = string.format("\\v(%s)", table.concat(queries, "|"))

  vim.fn.search(query)
  vim.fn.histadd("/", query)
end

-- Pager mode
dummy.pagerMode = function(filetype)
  if filetype ~= nil then
    vim.o.filetype = filetype
  end
  exec("setlocal ts=8 nomod nolist noma timeoutlen=0 nocursorline norelativenumber noshowcmd")
  local arg_nr_bs = { noremap = true, buffer = true, silent = true }
  map("n", "d", "<C-d>", arg_nr_bs)
  map("n", "u", "<C-u>", arg_nr_bs)
  map("n", "q", ":q<CR>", arg_nr_bs)
  map("n", "j", "<C-e>", arg_nr_bs)
  map("n", "k", "<C-y>", arg_nr_bs)
  exec("normal M")
end
vim.cmd([[ command! -nargs=* PagerMode call v:lua.dummy.pagerMode(<f-args>) ]])

dummy.myFoldText = function()
  local strpart = vim.fn.strpart
  local substitute = vim.fn.substitute

  local foldmarker = vim.fn.split(vim.o.foldmarker, ',')
  local tab_char = strpart(' ', vim.fn.shiftwidth())

  local line_contents = substitute(vim.fn.getline(vim.v.foldstart), '\t', tab_char, 'g')
  line_contents = substitute(line_contents, ' *' .. foldmarker[1] .. [[\d* *]], '', 'g')

  local number_int = vim.o.number and 1 or 0
  local numbers_width = vim.o.foldcolumn + number_int * vim.o.numberwidth
  local window_width = vim.fn.winwidth(0) - numbers_width - 1
  local folded_lines_number = vim.v.foldend - vim.v.foldstart

  line_contents = strpart(line_contents, 0, window_width - 2 - vim.fn.len(folded_lines_number))
  local void_size = window_width - vim.fn.len(line_contents) - vim.fn.len(folded_lines_number)
  local void_char = '·'

  local rs = vim.fn["repeat"](void_char, void_size)
  return line_contents .. rs .. folded_lines_number .. 'l   '
end
vim.o.foldtext = [[v:lua.dummy.myFoldText()]]
vim.cmd([[ command! FoldNone set nofoldenable ]])
vim.cmd([[ command! FoldBracket set foldenable foldmethod=marker foldmarker={,} ]])

-- highlight to-dos, fixmes etc.
-- partially stolen from https://github.com/sakshamgupta05/vim-todo-highlight
do
  local NAMES = {"TODO", "FIXME", "XXX", "NOTE"}
  for _, name in ipairs(NAMES) do
    local gname = ("annotation_%s"):format(name:lower())
    augroup(gname, {clear = true})
    autocmd({"Syntax"}, {
      pattern = "*",
      callback = function()
        local fmt = [[syntax match %s /\v\_.<%s>:?/hs=s+1 contained containedin=.*Comment.*]]
        exec(fmt:format(gname, name))
      end,
    })
    exec(("hi link %s Todo"):format(gname))
  end
end

vim.g.rifle_mode = utils.os.is_android and "buffer" or "popup"

if utils.os.is_android then
  -- local theme_name_path = ("%s/dots/theme"):format(vim.env.XDG_DATA_DIR)
  -- local theme_name = vim.fn.trim(vim.fn.readfile(theme_name_path)[1])
  -- vim.o.termguicolors = true
  -- vim.b.base16_use_true_colors = true
  -- vim.b.base16_true_color_map =
  --   utils.loadColorschemeFromYaml(("%s/config/dots/themes/%s.yaml"):format(vim.env.DOTFILES, theme_name))
  -- vim.cmd.colorscheme("base16")
  vim.o.termguicolors = false
end

-- TODO: inside neovim, replace the $EDITOR with a wrapper script that connects
-- to the current neovim instance, opens a buffer, and waits for the buffer to
-- unload before exiting.
--
-- Waiting for neovim to implement --remote-wait to do this. See
-- https://neovim.io/doc/user/remote.html
--
-- This might be possible without --remote-wait already, by using a BufWipeout
-- hook, but I'd need to figure out how that works and then make the wrapper
-- script wait till neovim responds back to it with a message saying the buffer
-- closed. Might not be too hard though?
