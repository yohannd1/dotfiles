local vim = _G.vim
local dummy = _G.dummy

local utils = require("cfg.utils")
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
  -- relativenumber = true,

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
vim.cmd([[ let &t_ZH = "\<Esc>[3m" ]])
vim.cmd([[ let &t_ZR = "\<Esc>[23m" ]])

vim.cmd("syntax on")
vim.cmd("filetype plugin indent on")

-- only auto-cd if we're not on windows
vim.o.autochdir = not utils.os.is_windows

if vim.g.neovide then
  vim.o.guifont = "Cascadia Code:h9"

  vim.g.neovide_transparency = 0.8
  vim.g.neovide_cursor_vfx_mode = "ripple"
end

vim.cmd([[command! Fgitmerge /\v^(\<{4,}|\={4,}|\>{4,})]])

dummy.toggleVirtualEdit = function()
  local value = (vim.o.ve == "") and "all" or ""
  vim.o.ve = value

  local message = string.format("Virtual edit set to '%s'", value)
  vim.api.nvim_echo({{message}}, false, {})
end

dummy.bufSwitch = function(dir)
  if dir == "next" then vim.cmd.bnext()
  elseif dir == "prev" then vim.cmd.bprevious()
  else error(("Unknown direction: %s"):format(dir))
  end
  vim.cmd([[ silent doautocmd User BufSwitch ]])
end

-- TODO: improved snippet system (move into separate module ig)
dummy.addSnippet = function(key, data)
  vim.cmd(("nnoremap <silent> <buffer> <Leader>i%s i%s<Esc>"):format(key, data))
end

dummy.itemToggleTodo = function()
  local fn = vim.b.item_toggletodo_func or dummy.itemToggleTodoDefault
  fn()
end

dummy.itemToggleTodoVisual = function()
  local doKeys = utils.doKeys
  local line = vim.fn.line

  doKeys("mz") -- mark

  -- get beginning and end lines
  doKeys("'<")
  local l1 = line(".")
  doKeys("'>")
  local l2 = line(".")
  local count = l2 - l1

  doKeys("'<")
  for _ = 0, count do
    dummy.itemToggleTodo()
    doKeys("j")
  end

  doKeys("`z")
end

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
    vim.cmd(("s/%s/\\1\\2\\3[%s]"):format(OPEN_SQUARE_PATT, preferred_done))
    afterReplace()
    return
  end

  if match(cur_line, OPEN_ROUND_PATT) ~= -1 then
    vim.cmd(("s/%s/\\1\\2\\3(%s)"):format(OPEN_ROUND_PATT, preferred_done))
    afterReplace()
    return
  end

  if match(cur_line, CLOSED_SQUARE_PATT) ~= -1 then
    vim.cmd(("s/%s/\\1\\2\\3[ ]"):format(CLOSED_SQUARE_PATT))
    afterReplace()
    return
  end

  if match(cur_line, CLOSED_ROUND_PATT) ~= -1 then
    vim.cmd(("s/%s/\\1\\2\\3( )"):format(CLOSED_ROUND_PATT))
    afterReplace()
    return
  end

  print("No to-do detected on the current line")
end

-- Highlight yanked selection briefly
if false then -- turned off lol
  autocmd({"TextYankPost"}, {
    pattern = "*",
    callback = function()
      vim.highlight.on_yank({
        higroup = "Normal",
      })
    end
  })
end

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
  vim.cmd([[ setlocal ts=8 nomod nolist noma timeoutlen=0 nocursorline norelativenumber noshowcmd ]])
  local arg_nr_bs = { noremap = true, buffer = true, silent = true }
  map("n", "d", "<C-d>", arg_nr_bs)
  map("n", "u", "<C-u>", arg_nr_bs)
  map("n", "q", ":q<CR>", arg_nr_bs)
  map("n", "j", "<C-e>", arg_nr_bs)
  map("n", "k", "<C-y>", arg_nr_bs)
  utils.doKey("M")
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
        vim.cmd(fmt:format(gname, name))
      end,
    })
    vim.cmd(("hi link %s Todo"):format(gname))
  end
end

if vim.g.rifle_mode == nil then
  vim.g.rifle_mode = utils.os.is_android and "buffer" or "popup"
end

vim.g.rifle_split_direction = utils.os.is_android and "down" or "right"

vim.api.nvim_create_user_command("Find", function(t)
  vim.cmd(("silent grep %s"):format(t.args))
  vim.cmd.copen()
end, { nargs = "*" })

vim.api.nvim_create_user_command("AcrMentionedIn", function(t)
  local parent_dir = vim.fn.expand("%:h")
  local current_path = vim.fn.expand("%f")
  vim.cmd(("Find %q %q"):format(current_path:gsub("%.acr$", ""), parent_dir))
  vim.cmd.copen()
end, { nargs = "*" })

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
