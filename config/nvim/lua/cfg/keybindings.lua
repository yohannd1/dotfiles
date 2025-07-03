local vim = _G.vim
local dummy = _G.dummy
local utils = require("cfg.utils")
local rifle = require("cfg.rifle")
local snippets = require("cfg.snippets")

local services = utils.services
local lazy = utils.lazy
local map = utils.map
local forChars = utils.forChars
local editFile = utils.editFile

local DOTFILES = vim.env.DOTFILES

-- Quick binding arguments
local arg_nr = { noremap = true }
local arg_nr_s = { noremap = true, silent = true }
local arg_s = { silent = true }

-- Related config
vim.o.timeoutlen = 1000
vim.o.ttimeoutlen = 10

-- set leader key
vim.g.mapleader = " "

-- Soft wrap keybindings
dummy.setSoftWrapBinds = function(enable)
  local keys = "jk0$"

  if not enable then
    forChars(keys, function(k)
      vim.cmd.nunmap(k)
      vim.cmd.nunmap("g" .. k)
    end)
    return
  end

  forChars(keys, function(k)
    local opts = { noremap = true, expr = true }
    forChars("nv", function(m)
      local fmt = string.format
      map(m, k, fmt([[v:count == 0 ? 'g%s' : '%s']], k, k), opts)
      map(m, "g" .. k, fmt([[v:count == 0 ? '%s' : 'g%s']], k, k), opts)
    end)
  end)
end
dummy.setSoftWrapBinds(true)

vim.cmd([[ command! -nargs=0 SWBindOn lua dummy.setSoftWrapBinds(true) ]])
vim.cmd([[ command! -nargs=0 SWBindOff lua dummy.setSoftWrapBinds(false) ]])

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
map("n", "<M-o>", ":lua dummy.itemToggleTodo()<CR>", arg_nr_s)
map("v", "<M-o>", "<Esc>:lua dummy.itemToggleTodoVisual()<CR>", arg_nr_s)

-- Used when no completion plugin is available.
--
-- When pressing the tab key, decide if it's needed to complete the current
-- word, or else simply insert the tab key.
dummy.tabOrComplete = function(scroll_direction)
  local col = vim.fn.col
  local esc = function(x)
    return vim.api.nvim_replace_termcodes(x, true, true, true)
  end

  local scrollKey = function()
    if scroll_direction == "up" then
      return esc("<C-p>")
    elseif scroll_direction == "down" then
      return esc("<C-n>")
    else
      error(("Unknown scroll direction: %s"):format(scroll_direction))
    end
  end

  if vim.fn.pumvisible() ~= 0 then
    return scrollKey()
  end

  local not_first_char = col(".") > 1
  local is_space = not_first_char and vim.fn.strcharpart(vim.fn.getline("."), col(".") - 2, 1):match("[ \t]")
  return (not is_space) and scrollKey() or esc("<Tab>")
end

-- Use Tab to complete or insert indent
map("i", "<Tab>", "<C-r>=v:lua.dummy.tabOrComplete('down')<CR>", arg_nr_s)
map("i", "<S-Tab>", "<C-r>=v:lua.dummy.tabOrComplete('up')<CR>", arg_nr_s)

-- Folding commands
-- " nnoremap <silent> <Tab> za
-- " nnoremap <silent> <S-Tab> zm

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

  vim.keymap.set(m, "<Leader>.", callback, { expr = true })
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

-- Toggle virtualedit
map("n", "<Leader>tv", ":lua dummy.toggleVirtualEdit()<CR>", arg_nr)

-- Small... snippets?
forChars("ic", function(m)
  map(m, "<C-u>", "<Nop>", arg_s)

  -- Current date
  vim.keymap.set(m, "<C-u>d", function()
    return vim.fn.strftime("%Y/%m/%d")
  end, {expr = true})

  -- Many backquotes
  vim.keymap.set(m, "<C-u>'", function()
    return "``````" .. "<Left>" .. "<Left>" .. "<Left>"
  end, {expr = true})

  -- Android hardware keyboard pain
  vim.keymap.set(m, "<C-u>c", function()
    return "``" .. "<Left>"
  end, {expr = true})
end)

-- Buffer navigation
map("n", "<C-j>", ":lua dummy.bufSwitch('next')<CR>", arg_nr_s)
map("n", "<C-k>", ":lua dummy.bufSwitch('prev')<CR>", arg_nr_s)

dummy.betterJoin = function()
  local line = vim.fn.line
  local getline = vim.fn.getline
  local doKeys = utils.doKeys

  -- line numbers
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
  local getpos = vim.fn.getpos
  local line_start = getpos("'<")[2]
  local line_end = getpos("'>")[2]
  local line_diff = line_end - line_start

  utils.doKeys(("%dG"):format(line_start))
  for _ = 1, line_diff do
    dummy.betterJoin()
  end
  utils.doKeys(("%dG"):format(line_start))
end

map("n", "J", ":lua dummy.betterJoin()<CR>", arg_nr_s)
map("v", "J", ":lua dummy.betterJoinVisual()<CR>", arg_nr_s)

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

map("n", "-", ":lua dummy.nerdTreeToggleX()<CR>", arg_nr)
map("n", "<Leader>o", ":lua dummy.menuOpenRecent()<CR>", arg_nr)
map("n", "<Leader>G", ":Goyo<CR>", arg_nr_s)

map("n", "<Leader>L", ":set cursorline!<CR>", arg_nr)
map("n", "<Leader>C", ":set cursorcolumn!<CR>", arg_nr)

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
      vim.fn.jobstart({browser, url})
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
map("n", "gf", ":lua dummy.openCurrentWORD()<CR>", arg_nr)

map("n", "<Leader>bf", ":lua require('cfg.format').formatBuffer()<CR>", arg_nr)

local telescope_builtin = require("telescope.builtin")
map("n", "<Leader>ft", dummy.findTodos, { noremap = true, desc = "find TODOs (in buffer)" })
map("n", "<Leader>fb", telescope_builtin.buffers, { noremap = true, desc = "find buffers" })
map("n", "<Leader>fh", telescope_builtin.help_tags, { noremap = true, desc = "find help tags" })
map("n", "<Leader>f.", telescope_builtin.find_files, { noremap = true, desc = "find files" })

for k, path in pairs({
  v = vim.env.VIM_INIT,
  r = ("%s/config/dots/resources.lua"):format(DOTFILES),
  e = ("%s/config/dots/env.sh"):format(DOTFILES),
  p = ("%s/config/dots/path.sh"):format(DOTFILES),
}) do
  local desc = ("edit: %s"):format(vim.fs.basename(path))
  local callback = function() vim.cmd.edit(path) end
  map("n", "<Leader>e" .. k, callback, { noremap = true, desc = desc })
end

local rifle_maps = { r = "run", b = "build", c = "check", t = "test", d = "debug" }
for k, action in pairs(rifle_maps) do
  local opts = { noremap = true, desc = ("rifle %s"):format(action) }
  map("n", "<Leader>r" .. k, function() rifle.run(action) end, opts)
end

local getWikiPage = function(id)
  return ("%s/%s.acr"):format(vim.g.acr_wiki_dir, id)
end

dummy.plan_sidebar = utils.Sidebar.new(getWikiPage("202407161554-F1C8E4"))
map("n", "<Leader>c", [[:lua dummy.plan_sidebar:toggle()<CR>]], arg_nr_s)

dummy.wikiOpenJournal = function()
  local result = vim.system({"acr-journal", "get-path"}, { text = true }):wait()
  assert(result.code == 0, "command failed to run")
  editFile(vim.trim(result.stdout))
end

-- wiki keybindings
local wiki_mappings = {
  {"w", lazy(editFile, getWikiPage("index")), "open index"},
  {"s", lazy(editFile, getWikiPage("202105021825-E80938")), "open scratchpad"},
  {"P", lazy(editFile, getWikiPage("202407161554-F1C8E4")), "open plan"},
  {"p", lazy(editFile, getWikiPage("202501061628-CB9C1A")), "open week plan (2024)"},
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

dummy.toggleQuickFix = function()
  if quickFixIsOpen() then
    vim.cmd.cclose()
  else
    vim.cmd.copen()
  end
end

map("n", "<Leader>q", dummy.toggleQuickFix, { noremap = true, desc = "toggle quickfix" })
map("n", "<Leader>l", [[:messages<CR>]], arg_nr)
map("n", "<Leader>T", [[:terminal<CR>i]], arg_nr)
map("n", "<Leader>[", lazy(vim.fn.jobstart, {"tmux", "new-window"}), { noremap = true, desc = "tmux: new tab" })

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
map("n", "<Leader>B", lazy(services.loadKeyMenu, "buffer"), arg_nr_s)

map("n", "<Leader>k", vim.cmd.bdelete, { noremap = true, desc = "delete buffer" })

-- TODO: make this better! so it can replace key-menus like hydra
-- map("n", "<Leader>B", function()
--   local choices = {
--     {"j", "next", lazy(dummy.bufSwitch, "next")},
--     {"k", "prev", lazy(dummy.bufSwitch, "prev")},
--     {"d", "delete", lazy(vim.cmd, "bdelete")},
--     {"w", "write", lazy(vim.cmd, "write")},
--   }

--   -- build string for the confirm dialog
--   local options_str = nil
--   do
--     local t = {}
--     for _, x in ipairs(choices) do
--       table.insert(t, ("&%s %s"):format(x[1], x[2]))
--     end
--     table.insert(t, "&q quit")
--     options_str = table.concat(t, "\n")
--   end

--   while true do
--     local c = vim.fn.confirm("", options_str)
--     if c <= #choices then
--       choices[c][3]() -- run the comand
--     elseif c == #choices + 1 then
--       break -- quit
--     else
--       print("Invalid choice...")
--     end
--   end
-- end, arg_nr)

map("n", "<Leader>i,", snippets.fuzzyMenu, arg_nr_s)

map("n", "<Leader>gr", ":NumRead<CR>", arg_nr_s)
map("n", "<Leader>g+", ":NumWriteInc<CR>", arg_nr_s)
map("n", "<Leader>g-", ":NumWriteDec<CR>", arg_nr_s)
