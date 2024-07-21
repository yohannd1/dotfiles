local dummy = _G.dummy

local utils = require("cfg.utils")
local exec = utils.exec
local map = utils.map
local setGlobals = utils.setGlobals

local vim_runtime_dir = vim.env.VIMRUNTIME
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

local mapVimFn = function(name, m)
  exec(string.format([[
    function! %s()
      lua %s()
    endfunction
  ]], name, m))
end

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

exec("syntax on")
exec("filetype plugin indent on")

-- only auto-cd if we're not on windows
vim.o.autochdir = not utils.os.is_windows

if vim.g.neovide then
  vim.o.guifont = "Cascadia Code:h9"

  vim.g.neovide_transparency = 0.8
  vim.g.neovide_cursor_vfx_mode = "ripple"
end

dummy.toggleVirtualEdit = function()
  local value = (vim.o.ve == "") and "all" or ""
  vim.o.ve = value

  local message = string.format("Virtual edit set to '%s'", value)
  vim.api.nvim_echo({{message}}, false, {})
end

dummy.bufSwitch = function(next)
  exec(next and "bnext" or "bprevious")
  exec("silent doautocmd User BufSwitch")
end

-- TODO: improved snippet system (move into separate module ig)
dummy.addSnippet = function(key, data)
  exec(string.format("nnoremap <silent> <buffer> <Leader>i%s i%s<Esc>", key, data))
end

dummy.itemToggleTodo = function()
  local fn = vim.b.item_toggletodo_func or vim.fn.Item_Default_ToggleTodo
  fn()
end

-- TODO: remove this
mapVimFn("Item_ToggleTodo", "dummy.itemToggleTodo")
mapVimFn("Item_ToggleTodoVisual", "dummy.itemToggleTodoVisual")

dummy.itemToggleTodoVisual = function()
  exec("normal mz") -- mark

  -- get beginning and end lines
  exec("normal '<")
  local l1 = vim.fn.line(".")
  exec("normal '>")
  local l2 = vim.fn.line(".")
  local count = l2 - l1

  exec("normal '<")
  for i = 0, count do
    vim.fn.Item_ToggleTodo()
    exec("normal j")
  end

  exec("normal `z")
end

-- me when i copy paste functions into an exec block
exec([[
function! Item_Default_ToggleTodo() " {{{
  let current_line = getline('.')

  let preferred_done = exists("b:item_toggletodo_preferred_done")
        \ ? b:item_toggletodo_preferred_done
        \ : "x"

  let open_square_patt = '\v^(\s*)([*-]*)(\s*)\[ \]'
  if current_line =~ open_square_patt
    exec 's/' . open_square_patt . '/' . '\1\2\3[' . preferred_done . ']'
    nohlsearch
    normal ``
    return
  endif

  let open_round_patt = '\v^(\s*)([*-]*)(\s*)\( \)'
  if current_line =~ open_round_patt
    exec 's/' . open_round_patt . '/' . '\1\2\3(' . preferred_done . ')'
    nohlsearch
    normal ``
    return
  endif

  let closed_square_patt = '\v^(\s*)([*-]*)(\s*)\[[Xx]\]'
  if current_line =~ closed_square_patt
    exec 's/' . closed_square_patt . '/' . '\1\2\3[ ]'
    nohlsearch
    normal ``
    return
  endif

  let closed_round_patt = '\v^(\s*)([*-]*)(\s*)\([Xx]\)'
  if current_line =~ closed_round_patt
    exec 's/' . closed_round_patt . '/' . '\1\2\3( )'
    nohlsearch
    normal ``
    return
  endif

  echo "No to-do detected on the current line"
endfunction " }}}
]])

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
  map("n", "d", "<C-d>")
  map("n", "u", "<C-u>")
  map("n", "q", ":q<CR>")
  map("n", "j", "<C-e>")
  map("n", "k", "<C-y>")
  exec("normal M")
end
vim.cmd("command! -nargs=* PagerMode call v:lua.dummy.pagerMode(<f-args>)")

-- Folding
exec([[
function! MyFoldText()
  let l:foldmarker = split(&foldmarker, ',')
  let l:tab_char = strpart(' ', shiftwidth())
  let l:line_contents = substitute(getline(v:foldstart), '\t', l:tab_char, 'g')
  let l:line_contents = substitute(l:line_contents, ' *'.l:foldmarker[0].'\d* *', '', 'g')

  let l:numbers_width = &foldcolumn + &number * &numberwidth
  let l:window_width = winwidth(0) - numbers_width - 1
  let l:folded_lines_number = v:foldend - v:foldstart

  let l:line_contents = strpart(l:line_contents, 0, l:window_width - 2 - len(l:folded_lines_number))
  let l:void_size = l:window_width - len(l:line_contents) - len(folded_lines_number)
  let l:void_char = '·'

  return l:line_contents . repeat(l:void_char, l:void_size) . l:folded_lines_number . 'l   '
endfunction
set foldtext=MyFoldText()
command! FoldNone set nofoldenable
command! FoldBracket set foldenable foldmethod=marker foldmarker={,}
]])

-- highlight to-dos, fixmes etc.
-- partially stolen from https://github.com/sakshamgupta05/vim-todo-highlight
--
-- TODO: make this work inside regions (e.g. strings)
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
