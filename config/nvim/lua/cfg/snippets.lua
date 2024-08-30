local vim = _G.vim
local utils = require("cfg.utils")
local doKeys = utils.doKeys

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

  if vim.trim(vim.fn.getline(".")) == "" then
    doKeys("ddk")
  end
  local cur_lnum = vim.fn.line(".")
  doKeys("mz")
  vim.fn.append(cur_lnum, s.content)
  doKeys("`z")

  local start_line = cur_lnum + 1
  local line_count = #(s.content)

  if s.reindent then
    doKeys(("%dG"):format(start_line))
    doKeys(("=%dj"):format(math.max(0, line_count-1)))
  end

  if s.marker ~= nil then
    utils.searchLiteral(s.marker)
    if vim.fn.line(".") >= start_line + line_count then
      error("found marker past the inserted lines. whoops!")
    end

    local char_count = vim.fn.strcharlen(s.marker)
    assert(char_count > 0, "WHY IS THE CHAR COUNT <= 0")
    doKeys(("d%dl"):format(char_count))
  end
end

return M
