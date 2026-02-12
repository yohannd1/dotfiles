-- Vim keybinding config
--
-- TODO: explain how I do plugin keybinding configuration (indeed, I need to do some optional verification here; we'll see)
--
-- Filetype-specific keybindings should be prefixed by <Leader>h (it's acessible enough for me)

local vim = _G.vim
local jobstart = vim.fn.jobstart
local col = vim.fn.col
local getline = vim.fn.getline

local dummy = _G.dummy
local rifle = require("cfg.rifle")
local snippets = require("cfg.snippets")
local format = require("cfg.format")

local utils = require("cfg.utils")
local doKeys = utils.doKeys
local lazy = utils.lazy
local box = utils.box
local lazyRepeatable = utils.lazyRepeatable
local services = utils.services
local map = utils.map
local forChars = utils.forChars
local editFile = utils.editFile

local DOTFILES = vim.env.DOTFILES
local TERMINAL = vim.env.TERMINAL

local lazyJobStart = function(...)
  return lazy(jobstart, {...})
end

-- Quick binding arguments
local arg_nr = { noremap = true }
local arg_nr_s = { noremap = true, silent = true }
local arg_s = { silent = true }

-- Related config
vim.o.timeoutlen = 1000
vim.o.ttimeoutlen = 10

-- set leader key
vim.g.mapleader = " "

-- Mouse wheel scrolling (except on android)
if not utils.os.is_android then
  map("n", "<ScrollWheelUp>", "<C-u>", arg_nr)
  map("n", "<ScrollWheelDown>", "<C-d>", arg_nr)
  map("i", "<ScrollWheelUp>", "<C-o><C-u>", arg_nr)
  map("i", "<ScrollWheelDown>", "<C-o><C-d>", arg_nr)
end

-- Disable mouse clicks without modifier key
forChars("ni", function(m)
  map(m, "<LeftMouse>", "<nop>", arg_nr)
  map(m, "<RightMouse>", "<nop>", arg_nr)

  -- NOTE: This is enabled by default already
  map(m, "<C-LeftMouse>", "<LeftMouse>", arg_nr)
  map(m, "<C-RightMouse>", "<RightMouse>", arg_nr)
end)

map("n", "<Esc>", ":noh<CR>", arg_nr_s)

-- Use ccedilla for entering into command modes
forChars("nv", function(m)
  map(m, "ç", ":", arg_nr)
  map(m, "Ç", "q:A", arg_nr)
end)

-- Clipboard versions of keymappings
forChars("nv", function(m)
  forChars("yYpPdDxX", function(key)
    map(m, "<Space>" .. key, '"+' .. key, arg_nr_s)
  end)
end)

-- Alt + h/l for quicker indentation across any mode.
--
-- I was thinking about doing Alt + ,/. for more consistency with << and >> but I think
-- my brain will accept using the direction keys more easily.
--
-- NOTE: I plan to use Alt keybindings for things that should work across any mode.
for k, once in pairs({h = "<", l = ">"}) do
  local kb = "<M-" .. k .. ">"
  local twice = once .. once
  local arg = {}

  map("i", kb, "<Esc>" .. twice .. "i", arg) -- using esc-i instead of c-o because the latter keeps the cursor static for some reason
  map("n", kb, twice, arg)
  map("v", kb, once .. "gv", arg)
end

-- Alt + o : toggle todo<->done state in items
map("n", "<M-o>", utils.task.toggleLine, { noremap = true, silent = true, desc = "toggle todo" })
map("v", "<M-o>", utils.task.toggleVisual, { noremap = true, silent = true, desc = "toggle todo" })

-- Wrapper around tab completion.
--
-- Used when no completion plugin is available.
--
-- When pressing the tab key, decide if it's needed to complete the current
-- word, or else simply insert the tab key.
local tabOrComplete = function(scroll_direction)
  local scrollKey = function()
    if scroll_direction == "up" then
      return vim.keycode("<C-p>")
    elseif scroll_direction == "down" then
      return vim.keycode("<C-n>")
    else
      error(("Unknown scroll direction: %s"):format(scroll_direction))
    end
  end

  if vim.fn.pumvisible() ~= 0 then
    return scrollKey()
  end

  local not_first_char = col(".") > 1
  local is_space = not_first_char and vim.fn.strcharpart(vim.fn.getline("."), col(".") - 2, 1):match("[ \t]")
  return (not is_space) and scrollKey() or vim.keycode("<Tab>")
end

-- Use Tab to complete or insert indent
map("i", "<Tab>", lazy(tabOrComplete, "down"), { noremap = true, expr = true })
map("i", "<S-Tab>", lazy(tabOrComplete, "up"), { noremap = true, expr = true })

-- Perl-ish regex searches
forChars("nv", function(m)
  -- Case insensitive
  map(m, "/", "/\\v\\c()<Left>", arg_nr)
  map(m, "?", "?\\v\\c()<Left>", arg_nr)

  -- Case sensitive
  map(m, "<Leader>/", "/\\v()<Left>", arg_nr)
  map(m, "<Leader>?", "?\\v()<Left>", arg_nr)
end)

-- Quick :substitute
map("n", "<Leader>s", [[:%s/\v/g<Left><Left>]], arg_nr)
map("v", "<Leader>s", [[:s/\v/g<Left><Left>]], arg_nr)
map("n", "<Leader>S", [[:%s/<C-r>///g<Left><Left>]], arg_nr)
map("v", "<Leader>S", [[:s/<C-r>///g<Left><Left>]], arg_nr)

-- Quick :global
forChars("nv", function(m)
  local callback = function()
    local result = vim.fn.input({
      prompt = "Query: ",
      default = "",
      cancelreturn = -1,
    })

    if result == -1 then
      return ""
    elseif result == "" then
      result = "."
    end

    return (":g/%s/normal "):format(result)
  end

  map(m, "<Leader>.", callback, { expr = true })
end)

-- Per-line macros for visual ranges
vim.cmd([[
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

function! ExecuteMacroOverVisualRange()
  echo "@"
  execute ":'<,'>normal @".nr2char(getchar())
endfunction
]])

-- Per-line dot
map("x", ".", ":normal .<CR>", arg_nr)

-- I KEEP PRESSING K BUT I DONT WANT IT HELP
forChars("nv", function(m)
  map(m, "K", "<Nop>", arg_nr)
  map(m, "gK", "K", arg_nr)
end)

-- Small... snippets?
forChars("ic", function(m)
  map(m, "<C-u>", "<Nop>", arg_s)

  local mapE = function(k, v)
    local func
    if type(v) == "function" then
      func = v
    elseif type(v) == "string" then
      func = function() return v end
    else
      error(("unexpected type: %s"):format(type(v)))
    end
    vim.keymap.set(m, "<C-u>" .. k, func, { expr = true })
  end

  mapE("d", lazy(vim.fn.strftime, "%Y/%m/%d"))
  mapE("'", "``````" .. "<Left>" .. "<Left>" .. "<Left>")

  -- Android hardware keyboard pain
  mapE("c", "``<Left>")
  mapE("u", "^")
end)

-- Buffer navigation
map("n", "<C-j>", lazy(dummy.bufSwitch, "next"), { noremap = true, silent = true, desc = "next buffer" })
map("n", "<C-k>", lazy(dummy.bufSwitch, "prev"), { noremap = true, silent = true, desc = "prev buffer" })

dummy.betterJoin = function()
  -- line numbers
  local line = vim.fn.line
  local ln_cur = line(".")
  local ln_last = line("$")
  if ln_cur == ln_last then
    return -- we're at the end. there's nothing to join.
  end

  local l_cur = getline(ln_cur):gsub("%s+$", "")
  local l_next = getline(ln_cur+1):gsub("^%s+", "")
  doKeys("jdd") -- delete the old "next line"
  if ln_cur < ln_last - 1 then
    doKeys("k") -- a fix because if we delete the last line it'll push us upward already
  end

  -- add whitespace if needed (following the whitespace matcher's result)
  local ws_m = vim.b.better_join_whitespace_matcher
  local add_whitespace = ( ws_m == nil or ws_m(l_cur, l_next) )

  -- replace and get back to where we were
  vim.fn.setline(".", l_cur .. (add_whitespace and " " or "") .. l_next)
  local ret_y = ln_cur
  local ret_x = vim.fn.strcharlen(l_cur)
  vim.api.nvim_win_set_cursor(0, {ret_y, ret_x})
end

dummy.betterJoinVisual = function()
  local l_start, l_end = utils.getVisualLineNumbers()
  local diff = l_end - l_start

  doKeys(vim.keycode("<Esc>"))
  doKeys(l_start .. "G")
  for _ = 1, diff do
    dummy.betterJoin()
  end
end

map("n", "J", lazyRepeatable(dummy.betterJoin), arg_nr_s)
map("v", "J", dummy.betterJoinVisual, arg_nr_s)

-- Terminal commands
map("t", "<M-w>", "<C-\\><C-n>", arg_nr_s)
map("t", "<C-w>.", "<C-\\><C-n>", arg_nr_s)
map("t", "<C-w>q", "<C-\\><C-n><C-w>q", arg_nr_s)
map("t", "<C-w><C-j>", "<C-j>", arg_nr_s)
map("t", "<C-w><C-k>", "<C-k>", arg_nr_s)
forChars("hjkl", function(l)
  map("t", "<C-w>" .. l, "<C-\\><C-n><C-w>" .. l, arg_nr_s)
end)

-- Tab navigation
map("n", "<C-x>j", ":tabn<CR>", arg_nr_s)
map("n", "<C-x>k", ":tabp<CR>", arg_nr_s)
map("n", "<C-x>n", ":tabnew<CR>", arg_nr_s)

-- Buffer closing
vim.cmd.cabbrev("bd Bclose")
vim.cmd.cabbrev("bd! Bclose!")

-- Navigate the completion menu with <C-k>, <C-j> and <C-m>
do
  local pv_check = function(lhs, rhs)
    return function()
      return vim.fn.pumvisible() == 1 and lhs or rhs
    end
  end

  vim.keymap.set("i", "<C-j>", pv_check("<C-n>", "<C-j>"), { expr = true })
  vim.keymap.set("i", "<C-k>", pv_check("<C-p>", "<C-k>"), { expr = true })
  vim.keymap.set("i", "<Down>", pv_check("<C-n>", "<Down>"), { expr = true })
  vim.keymap.set("i", "<Up>", pv_check("<C-p>", "<Up>"), { expr = true })
  vim.keymap.set("i", "<C-m>", pv_check("<C-y>", "<C-m>"), { expr = true })
end

vim.cmd([[ map <C-m> <CR> ]])

dummy.nerdTreeToggleX = function()
  if vim.fn.exists("b:NERDTree") ~= 0 then
    vim.cmd.NERDTreeClose()
  else
    vim.cmd.NERDTreeCWD()
  end
end

map("n", "-", dummy.nerdTreeToggleX, arg_nr)
-- map("n", "<Leader>G", ":Goyo<CR>", arg_nr_s)

-- better n/N keys
map("n", "n", "/<Up><CR>", arg_nr_s)
map("n", "N", "?<Up><CR>", arg_nr_s)

-- open hovered text
dummy.openCurrentWORD = function()
  local matchstr = vim.fn.matchstr
  local cword = vim.fn.expand("<cWORD>")

  local url_result = matchstr(cword, [[\v(https?|www\.)://[a-zA-Z0-9/\-\.%_?#=&+~:()]+]])
  if #url_result > 0 then
    local url = cword
    print("Found URL: " .. url)
    local browser = assert(vim.env.BROWSER, "Could not find a suitable browser to open the WORD (set it via $BROWSER)")

    if vim.fn.confirm("Open using a browser?", "&Yes\n&No") == 1 then
      jobstart({browser, url})
    end

    return
  end

  local file = matchstr(cword, [[\v[a-zA-Z0-9_\-\./]+]])
  if #file > 0 then
    print("Found file: " .. file)
    if vim.fn.confirm("Open in a new buffer", "&Yes\n&No") == 1 then
      vim.cmd.edit(file)
    end

    return
  end

  print("Could not find a suitable file or url in the current WORD")
end
map("n", "gf", dummy.openCurrentWORD, arg_nr)

map("n", "<Leader>bf", format.formatBuffer, { noremap = true, desc = "format buffer" })

local tsc_builtin = require("telescope.builtin")
map("n", "<Leader>ft", dummy.findTodos, { noremap = true, desc = "find TODOs (in buffer)" })
map("n", "<Leader>fb", tsc_builtin.buffers, { noremap = true, desc = "find buffers" })
map("n", "<Leader>fh", tsc_builtin.help_tags, { noremap = true, desc = "find help tags" })
map("n", "<Leader>f.", tsc_builtin.find_files, { noremap = true, desc = "find files" })
map("n", "<Leader>fg", tsc_builtin.git_files, { noremap = true, desc = "find files in git repo" })
map("n", "<Leader>fl", tsc_builtin.live_grep, { noremap = true, desc = "live grep" })
map("n", "<Leader>m", tsc_builtin.commands, { noremap = true, desc = "find commands" })
map("n", "<Leader>fr", dummy.menuOpenRecent, { noremap = true, desc = "find recent files" })

for k, path in pairs({
  v = vim.env.VIM_INIT,
  r = ("%s/config/dots/resources.lua"):format(DOTFILES),
  e = ("%s/config/dots/env.sh"):format(DOTFILES),
  p = ("%s/config/dots/path.sh"):format(DOTFILES),
}) do
  local desc = ("edit: %s"):format(vim.fs.basename(path))
  map("n", "<Leader>e" .. k, lazy(vim.cmd.edit, path), { noremap = true, desc = desc })
end

local rifle_maps = { r = "run", b = "build", c = "check", t = "test", d = "debug" }
for k, action in pairs(rifle_maps) do
  local opts = { noremap = true, desc = ("rifle %s"):format(action) }
  map("n", "<Leader>r" .. k, function() rifle.run(action) end, opts)
end

local getWikiPage = function(id)
  return ("%s/%s.acr"):format(vim.g.acr_wiki_dir, id)
end

local special_map = {}
local special_map_path = ("%s/../wiki_meta/special-pages.conf"):format(vim.g.acr_wiki_dir)
if vim.fn.filereadable(special_map_path) ~= 0 then
  for _, line in ipairs(vim.fn.readfile(special_map_path)) do
    local s = vim.split(line, " ", { plain = true, trimempty = true })
    assert(#s == 2, "too much stuff in the line")
    special_map[s[1]] = s[2]
  end
else
  print("warning: wiki special map not found")
end

local getSpecialWikiPage = function(name)
  local id = special_map[name]
  assert(id, ("no page with id '%q' found"):format(name))
  return getWikiPage(id)
end

dummy.plan_sidebar = utils.Sidebar.new(getSpecialWikiPage("plan"))
map("n", "<Leader>c", function() dummy.plan_sidebar:toggle() end, arg_nr_s)

dummy.wikiOpenJournal = function()
  local result = vim.system({"acr-journal", "get-path"}, { text = true }):wait()
  assert(result.code == 0, "command failed to run")
  editFile(vim.trim(result.stdout))
end

-- wiki keybindings
local wiki_mappings = {
  {"w", lazy(editFile, getSpecialWikiPage("index")), "open index"},
  {"s", lazy(editFile, getSpecialWikiPage("scratchpad")), "open scratchpad"},
  {"P", lazy(editFile, getSpecialWikiPage("plan")), "open plan"},
  {"p", lazy(editFile, getSpecialWikiPage("yearly-week-plan-2026")), "open week plan (2026)"},
  {"o", lazy(dummy.wikiFzOpen, {}), "search on the wiki"},
  {"j", lazy(dummy.wikiOpenJournal, {}), "open journal"},
  {"R", lazy(dummy.wikiFzInsertRef, { after_cursor = false }), "add reference ←"},
  {"r", lazy(dummy.wikiFzInsertRef, { after_cursor = true }), "add reference →"},
  {"N", lazy(dummy.wikiNewFileInsertRef, { after_cursor = false }), "new note + add reference ←"},
  {"n", lazy(dummy.wikiNewFileInsertRef, { after_cursor = true }), "new note + add reference →"},
}
for _, entry in ipairs(wiki_mappings) do
  map("n", "<Leader>w" .. entry[1], entry[2], { noremap = true, desc = entry[3] })
end

local quickFixIsOpen = function()
  local t = vim.fn.filter(vim.fn.getwininfo(), "v:val.quickfix && !v:val.loclist")
  return #t > 0
end

local toggleQuickFix = function()
  if quickFixIsOpen() then
    vim.cmd.cclose()
  else
    vim.cmd.copen()
  end
end

map("n", "<Leader>qq", toggleQuickFix, { noremap = true, desc = "quickfix: toggle" })
map("n", "<Leader>qn", ":cnext<CR>", { noremap = true, desc = "quickfix: next" })
map("n", "<Leader>qp", ":cprev<CR>", { noremap = true, desc = "quickfix: previous" })

map("n", "<Leader>l", [[:messages<CR>]], arg_nr)

-- open terminal
map("n", "<Leader>tn", [[:terminal<CR>i]], arg_nr)
map("n", "<Leader>tN", lazyJobStart("tmux", "new-window"), { noremap = true, desc = "new tmux tab" })
map("n", "<Leader>to", lazyJobStart(TERMINAL), { noremap = true, desc = "new terminal window" })

services.defKeyMenu({
  id = "buffer",
  title = "Buffer navigation",
  single_command = false,
  keymaps = {{
    name = "Operations",
    keys = {
      {"j", "lua dummy.bufSwitch('next')", "next"},
      {"k", "lua dummy.bufSwitch('prev')", "previous"},
      {"d", "bd", "delete"},
      {"w", "w", "save"},
    },
  }}
})
-- map("n", "<Leader>B", lazy(services.loadKeyMenu, "buffer"), arg_nr_s)

map("n", "<Leader>k", vim.cmd.bdelete, { noremap = true, desc = "delete buffer" })

map("n", "<Leader>L", ":lua print(vim.inspect())<Left><Left>", { noremap = true, desc = "lua expr" })

-- TODO: make this better! so it can replace key-menus like hydra
map("n", "<Leader>B", function()
  local choices = {
    {"j", "next", lazy(dummy.bufSwitch, "next")},
    {"k", "prev", lazy(dummy.bufSwitch, "prev")},
    {"d", "delete", lazy(vim.cmd, "bdelete")},
    {"w", "write", lazy(vim.cmd, "write")},
  }

  -- build string for the confirm dialog
  local options_str = nil
  do
    local t = {}
    for _, x in ipairs(choices) do
      table.insert(t, ("&%s %s"):format(x[1], x[2]))
    end
    table.insert(t, "&q quit")
    options_str = table.concat(t, "\n")
  end

  while true do
    vim.cmd("redraw") -- TODO: use something better
    local c = vim.fn.confirm("", options_str)
    if c <= #choices then
      choices[c][3]() -- run the comand
    elseif c == #choices + 1 then
      break -- quit
    else
      print("Invalid choice...")
    end
  end
end, arg_nr)

map("n", "<Leader>i,", snippets.fuzzyMenu, arg_nr_s)

map("n", "<Leader>,", ":ProgSnipLine<CR>", arg_nr)
map("v", "<Leader>,", ":ProgSnipVisual<CR>", arg_nr)

-- TODO: how to make this better?
-- map("n", "<Leader>gr", ":NumRead<CR>", arg_nr_s)
-- map("n", "<Leader>g+", ":NumWriteInc<CR>", arg_nr_s)
-- map("n", "<Leader>g-", ":NumWriteDec<CR>", arg_nr_s)

-- "Maximize & minimize" windows... kinda
map("n", "<C-w>M", "<C-w>|<C-w>_", { desc = "maximize window" })
map("n", "<C-w>m", "<C-w>=", { desc = "equalize windows" })

-- Strudel keybindings
box(utils.tryRequire("strudel")):andThen(function(strudel)
  map("n", "<Leader>hh", strudel.toggle, { desc = "strudel: play/stop" })
  map("n", "<Leader>hu", strudel.update, { desc = "strudel: update" })
  map("n", "<Leader>ho", strudel.launch, { desc = "strudel: launch browser" })
  map("n", "<Leader>hO", strudel.quit, { desc = "strudel: quit browser" })
end)
