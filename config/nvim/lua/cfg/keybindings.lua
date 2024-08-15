-- vim: foldenable foldmethod=marker foldmarker={{{,}}}

local dummy = _G.dummy
local utils = require("cfg.utils")
local services = utils.services

local getLineToEnd = function() return vim.fn.getline('.'):sub(vim.fn.col('.')) end
local exec = utils.exec
local map = utils.map
local forChars = utils.forChars

-- Quick binding arguments
local arg_nr = { noremap = true }
local arg_nr_s = { noremap = true, silent = true }
local arg_s = { silent = true }

-- Related config
vim.o.timeoutlen = 1000
vim.o.ttimeoutlen = 10

-- Leader keys
forChars("nv", function(m)
  map(m, "<Space>", "<Leader>")
  map(m, ",", "<Leader>")
end)

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
    local opts = {noremap = true, expr = true}
    forChars("nv", function(m)
      local fmt = string.format
      map(m, k, fmt([[v:count == 0 ? 'g%s' : '%s']], k, k), opts)
      map(m, "g" .. k, fmt([[v:count == 0 ? '%s' : 'g%s']], k, k), opts)
    end)
  end)
end
dummy.setSoftWrapBinds(true)

vim.cmd([[command! -nargs=0 SWBindOn lua dummy.setSoftWrapBinds(true)]])
vim.cmd([[command! -nargs=0 SWBindOff lua dummy.setSoftWrapBinds(false)]])

-- Mouse wheel scrolling (except on android)
if not utils.os.is_android then
  map("n", "<ScrollWheelUp>", "<C-u>", arg_nr)
  map("n", "<ScrollWheelDown>", "<C-d>", arg_nr)
  map("i", "<ScrollWheelUp>", "<C-o><C-u>", arg_nr)
  map("i", "<ScrollWheelDown>", "<C-o><C-d>", arg_nr)
end

-- Disable mouse clicks without modifier key
forChars("ni", function(m)
  -- TODO: visual mode drag when off normal mode
  map(m, "<LeftMouse>", "<nop>", arg_nr)
  map(m, "<RightMouse>", "<nop>", arg_nr)

  -- NOTE: This is enabled by default already
  -- map(m, "<C-LeftMouse>", "<LeftMouse>", arg_nr)
  -- map(m, "<C-RightMouse>", "<RightMouse>", arg_nr)
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

exec([[
  function! TabOrComplete(mode)
  """ Used when no completion plugin is available.
  """ When pressing the tab key, decide if it's needed to complete the current word, or else simply insert the tab key.
  """ There is a mapping in the Mappings section for this.
  if (col(".") > 1) && !(strcharpart(getline("."), col(".") - 2, 1) =~ '\v[ \t]')
    if (a:mode == 0)
      return "\<C-P>"
    elseif (a:mode == 1)
      return "\<C-N>"
    endif
  else
    return "\<Tab>"
  endif
endfunction
]])

-- Use Tab to complete or insert indent
map("i", "<Tab>", "<C-r>=TabOrComplete(1)<CR>", arg_nr_s)
map("i", "<S-Tab>", "<C-r>=TabOrComplete(0)<CR>", arg_nr_s)

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

-- Now for replacing
map("n", "<Leader>s", [[:%s/\v/g<Left><Left>]], arg_nr)
map("v", "<Leader>s", [[:s/\v/g<Left><Left>]], arg_nr)
map("n", "<Leader>S", [[:%s/<C-r>///g<Left><Left>]], arg_nr)
map("v", "<Leader>S", [[:s/<C-r>///g<Left><Left>]], arg_nr)

-- I KEEP PRESSING K BUT I DONT WANT IT HELP
forChars("nv", function(m)
  map(m, "K", "<Nop>", arg_nr)
  map(m, "gK", "K", arg_nr)
end)

-- Toggle virtualedit
map("n", "<Leader>tv", ":lua dummy.toggleVirtualEdit()<CR>", arg_nr)

map("i", "<C-u>", "<Nop>", arg_s)

-- Insert date
vim.keymap.set("i", "<C-u>d", function()
  return vim.fn.strftime("%Y/%m/%d")
end, {expr = true})

-- Insert the shits
vim.keymap.set("i", "<C-u>'", function()
  return "``````" .. "<Left>" .. "<Left>" .. "<Left>"
end, {expr = true})

-- Buffer navigation
map("n", "<C-j>", ":lua dummy.bufSwitch(true)<CR>", arg_nr_s)
map("n", "<C-k>", ":lua dummy.bufSwitch(false)<CR>", arg_nr_s)

-- Better Join - a join command similar to the one in emacs (or evil-mode, idk) {{{
dummy.betterJoin = function()
  local normal = function(s) exec("normal! " .. s) end
  local rmTrailWhs = function() normal("V:s/\\s\\+$//e\\<CR>") end

  local opts = vim.b.better_join_opts or {}
  local add_whitespace_match = opts.add_whitespace_match or -1
  vim.b._foo = add_whitespace_match

  normal("$m`") -- go to the end of the line and set a mark there
  rmTrailWhs() -- remove trailing whitespace
  normal("J") -- actually join lines
  rmTrailWhs() -- remove trailing whitespace again
  normal("``l") -- go to the mark we had set and move 1 to the right

  -- At this point, we're at the start of what was previously the line
  -- below. Let's remove the potential whitespace just in case.
  if getLineToEnd():match("^%s+") then
    normal("dw")
  end

  -- Add whitespace if a specific match is wanted
  if add_whitespace_match ~= -1
    and getLineToEnd():match("^" .. add_whitespace_match) then
    normal("i ")
  end
end

dummy.betterJoinVisual = function()
  local line_start = getpos("'<")[1]
  local line_end = getpos("'>")[1]
  local line_diff = line_end - line_start

  exec("normal \\<Esc>" .. line_start .. "G")
  for _ = 1,line_diff do dummy.betterJoin() end
  exec("normal " .. line_start .. "G")
end

map("n", "J", ":lua dummy.betterJoin()<CR>", arg_nr_s)
map("v", "J", ":lua dummy.betterJoinVisual()<CR>", arg_nr_s)
-- }}}

-- Terminal commands
map("t", "<C-w>.", "<C-\\><C-n>", arg_nr_s)
map("t", "<C-w>q", "<C-\\><C-n><C-w>q", arg_nr_s)
map("t", "<C-w><C-j>", "<C-j>", arg_nr_s)
map("t", "<C-w><C-k>", "<C-k>", arg_nr_s)
map("t", "<C-j>", "<C-\\><C-n>:call NextBuffer()<CR>", arg_nr_s)
map("t", "<C-k>", "<C-\\><C-n>:call PrevBuffer()<CR>", arg_nr_s)
forChars("hjkl", function(l)
  map("t", "<C-w>" .. l, "<C-\\><C-n><C-w>" .. l, arg_nr_s)
end)

-- Tab navigation
map("n", "<C-x>j", ":tabn<CR>", arg_nr_s)
map("n", "<C-x>k", ":tabp<CR>", arg_nr_s)
map("n", "<C-x>n", ":tabnew<CR>", arg_nr_s)

-- Buffer closing
exec("cabbrev bd Bclose")
exec("cabbrev bd! Bclose!")

-- Navigate the completion menu with <C-k>, <C-j> and <C-m>
do
  local pv_check = function(lhs, rhs)
    return function()
      return vim.fn.pumvisible() == 1 and lhs or rhs
    end
  end

  vim.keymap.set("i", "<C-j>", pv_check("<C-n>", "<C-j>"), {expr = true})
  vim.keymap.set("i", "<C-k>", pv_check("<C-p>", "<C-k>"), {expr = true})
  vim.keymap.set("i", "<Down>", pv_check("<C-n>", "<Down>"), {expr = true})
  vim.keymap.set("i", "<Up>", pv_check("<C-p>", "<Up>"), {expr = true})
  vim.keymap.set("i", "<C-m>", pv_check("<C-y>", "<C-m>"), {expr = true})
end

exec("map <C-m> <CR>")

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

  local url = matchstr(cword, [[\v(https?|www\.)://[a-zA-Z0-9/\-\.%_?#=&+~:()]+]])
  if #url > 0 then
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

dummy.formatBuffer = function()
  if vim.b.format_command == nil then
    print("No format command found (set it with b:format_command)")
    return
  end

  local append = vim.fn.append

  local lines = vim.fn.getline(1, "$")

  local in_file = vim.fn.tempname()
  vim.fn.writefile(lines, in_file)

  local cmd = ("%s < %s"):format(vim.b.format_command, in_file)
  local output = vim.fn.systemlist(cmd)

  if vim.v.shell_error ~= 0 then
    vim.g.last_format_err = output
    print("Formatting failed - see buffer for more info")
    exec([[
      if bufname("*Format Errors*") == ""
        split
        wincmd j
        enew
        file *Format Errors*
        setlocal buftype=nofile
        setlocal bufhidden=delete
        setlocal noswapfile
        setlocal nobuflisted
      else
        let s:bufwindow = bufwinid("*Format Errors*")

        if s:bufwindow != -1
          call win_gotoid(s:bufwindow)
        else
          split
          wincmd j
          buffer *Format Errors*
        endif
      endif
    ]])

    vim.opt_local.modifiable = true
    exec("normal ggdG")
    append("$", "Formatting failed:")
    for _, line in ipairs(output) do
      append("$", "  " .. line)
    end
    exec("normal ggdd")
    vim.opt_local.modifiable = false
  else
    local bw = vim.fn.bufwinid("*Format Errors*")
    if bw ~= -1 then
      vim.fn.nvim_win_close(bw, false)
    end

    -- TODO: turn this into a "snapshot position"
    -- something like utils.savePosition(); utils.restorePosition()
    local l_win_top = vim.fn.line("w0")
    local c_line = vim.fn.line(".")
    local c_col = vim.fn.col(".")
    exec("normal ggdG")
    vim.fn.setline(1, output)
    exec(("normal %dGzt"):format(l_win_top))
    exec(("normal %dG%d|"):format(c_line, c_col))
  end

  vim.fn.delete(in_file)
end
map("n", "<Leader>bf", ":lua dummy.formatBuffer()<CR>", arg_nr)

services.defKeyMenu({
  id = "extrafind",
  title = "Find",
  keymaps = {{
    name = "In buffer",
    keys = {
      {"t", "lua dummy.findTodos()", "TODOs (in buffer)"},
      {"b", "lua require('telescope.builtin').buffers()", "buffers"},
      {"h", "lua require('telescope.builtin').help_tags()", "help tags"},
      {".", "lua require('telescope.builtin').find_files()", "files"},
    },
  }}
})
map("n", "<Leader>f", [[:lua require("cfg.utils").services.loadKeyMenu("extrafind")<CR>]], arg_nr_s)

services.defKeyMenu({
  id = "edit",
  title = "Edit",
  keymaps = {{
    name = "Common files",
    keys = {
      {"v", "e $VIM_INIT", "init.vim"},
      {"r", "e $DOTFILES/config/dots/resources.lua", "resources.lua"},
    },
  }},
})
map("n", "<Leader>e", [[:lua require("cfg.utils").services.loadKeyMenu("edit")<CR>]], arg_nr_s)

-- rifle
services.defKeyMenu({
  id = "rifle",
  title = "Rifle",
  keymaps = {{
    name = "General",
    keys = {
      {"r", [[Rifle 'run']], "run"},
      {"b", [[Rifle 'build']], "build"},
      {"c", [[Rifle 'check']], "check"},
      {"t", [[Rifle 'test']], "test"},
    },
  }},
})
map("n", "<Leader>r", [[:lua require("cfg.utils").services.loadKeyMenu("rifle")<CR>]], arg_nr_s)

-- wiki stuff
services.defKeyMenu({
  id = "wiki",
  title = "Wiki",
  keymaps = {
    {
      name = "Open...",
      keys = {
        {"w", "e ~/wiki/vimwiki/index.acr", "index"},
        {"s", "e ~/wiki/vimwiki/202105021825-E80938.acr", "scratchpad"},
        {"P", "e ~/wiki/vimwiki/202407161554-F1C8E4.acr", "plan"},
        {"p", "e ~/wiki/vimwiki/202401151901-42E4FA.acr", "week plan (2024)"},
        {"o", "lua dummy.wikiFzOpen({})", "search"},
        -- {"O", "lua dummy.wikiFzOpen({}, {'acw-get-projects'})", "select a project"},
      },
    },

    {
      name = "References",
      keys = {
        {"R", "lua dummy.wikiFzInsertRef({after_cursor = false})", "add reference ←"},
        {"r", "lua dummy.wikiFzInsertRef({after_cursor = true})", "add reference →"},
        {"N", "lua dummy.wikiNewFileInsertRef({after_cursor = false})", "new note + add reference ←"},
        {"n", "lua dummy.wikiNewFileInsertRef({after_cursor = true})", "new note + add reference →"},
      }
    },
  },
})
map("n", "<Leader>w", [[:lua require("cfg.utils").services.loadKeyMenu("wiki")<CR>]], arg_nr_s)
