local ucm = _G.useConfModule
local dummy = _G.dummy

local utils = ucm("utils")

local exec = function(s) vim.api.nvim_exec(s, false) end

local mapVimFn = function(name, map)
  exec(string.format([[
    function! %s()
      lua %s()
    endfunction
  ]], name, map))
end

local vim_runtime_dir = vim.env.VIMRUNTIME
local autocmd = vim.api.nvim_create_autocmd

-- FIXME: am I doing this right
-- utils.source_if_present(vim_runtime_dir .. "/delmenu.vim") or utils.source_if_present(vim_runtime_dir .. "/menu.vim")

vim.o.encoding = "utf-8"
vim.o.langmenu = "en_US"
vim.env.LANG = "en_US"

vim.o.hidden = true
vim.o.title = true
vim.o.number = true
vim.o.relativenumber = true

vim.o.backspace = "indent,eol,start"
vim.o.laststatus = 2

-- default indent settings
local TAB_WIDTH = 8
local SPACE_WIDTH = 4
vim.o.tabstop = TAB_WIDTH
vim.o.shiftwidth = SPACE_WIDTH
vim.o.softtabstop = SPACE_WIDTH
vim.o.expandtab = true
vim.o.smarttab = true

vim.o.wildmenu = true
vim.o.wildmode = "longest:full,full"

vim.o.autoindent = true

vim.o.hlsearch = true
vim.o.incsearch = true

exec([[
    set linebreak wrap
    set cursorline " line highlighting
    set showcmd
    set shortmess+=atcI
    set belloff+=ctrlg
    set mouse=a
    set display+=lastline
    set complete=.,w,b,u,k,kspell " 'i' was interesting too but it seems too expensive; 't' for no tags
    set completeopt-=preview
    set completeopt+=menuone,noselect
    set noshowmode
    set list
    set nofoldenable
    set cinoptions+=g0
    set cinoptions+=:0
    set scrolloff=3 " scroll ahead :)

    set listchars=tab:\ \ ,trail:¬

    syntax on

    " That's how the italics work (or not)
    let &t_ZH = "\<Esc>[3m"
    let &t_ZR = "\<Esc>[23m"
]])

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
function! ReverseRSearch(basedir, query) " {{{
  let l:current_dir = a:basedir
  while 1
    let l:current_glob = glob(l:current_dir . "/*", 0, 1)
    let l:list_match = ListMatch(l:current_glob, '.*/' .. a:query .. '$') || ListMatch(l:current_glob, '.*\\' .. a:query .. '$')
    if (l:current_dir == "/") || (l:current_dir =~ '^\w:\\$') " *NIX or Windows root directories
      return l:list_match
    else
      if l:list_match
        return 1
      else
        let l:current_dir = fnamemodify(l:current_dir, ":h")
      endif
    endif
  endwhile
endfunction " }}}
function! ListMatch(list, pattern) " {{{
  for e in a:list
    if e =~ a:pattern
      return 1
    endif
  endfor
  return 0
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
