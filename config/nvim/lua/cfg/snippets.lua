local vim = _G.vim

local utils = require("cfg.utils")

local doKeys = utils.doKeys
local services = utils.services

local M = {}

M.register = function(opts)
  local key = assert(opts.key, "key not specified")

  local content = assert(opts.content, "content not specified")
  if type(content) == "string" then
    content = vim.split(vim.trim(content), "\n", { plain = true, trimempty = true })
  end

  local marker = opts.marker

  local reindent = opts.reindent
  if reindent == nil then
    reindent = true
  end

  -- FIXME: I think this is inneficient but it wasn't working if I didn't do this
  local snippets = vim.b.snippets or {}
  snippets[key] = {
    content = content,
    marker = marker,
    reindent = reindent,
  }
  vim.b.snippets = snippets

  vim.keymap.set("n", "<leader>i" .. key, function() M.use(key) end)
end

M.use = function(key)
  local s = (vim.b.snippets or {})[key]
  if s == nil then
    error(("No such key: %s"):format(key))
  end

  local start_lnum = vim.fn.line(".")
  local line_count = #s.content
  local append_count = line_count

  local is_empty_line = vim.trim(vim.fn.getline(".")) == ""
  if is_empty_line then
    append_count = append_count - 1
  else
    start_lnum = start_lnum + 1
  end

  for _ = 1, append_count do
    doKeys("o")
  end

  doKeys(start_lnum .. "G")
  vim.fn.setline(".", s.content)

  if s.reindent then
    doKeys(("%dG"):format(start_lnum))
    doKeys(("=%dj"):format(math.max(0, line_count-1)))
  end

  if s.marker ~= nil then
    doKeys(("%dG"):format(start_lnum))

    if not utils.searchLiteral(s.marker)
      or vim.fn.line(".") < start_lnum
      or vim.fn.line(".") >= start_lnum + line_count
    then
      error("failed to find marker in the inserted snippet lines")
    end

    local char_count = vim.fn.strcharlen(s.marker)
    assert(char_count > 0, "WHY IS THE CHAR COUNT <= 0")
    doKeys(("d%dl"):format(char_count))
  end
end

M.fuzzyMenu = function()
  local snippets_list = {}
  for k, _ in pairs(vim.b.snippets or {}) do
    table.insert(snippets_list, k)
  end
  services.fuzzyPicker({
    prompt = "Pick a snippet",
    source = { func = function() return snippets_list end },
    on_choice = function(choice) M.use(choice) end
  })
end

return M
