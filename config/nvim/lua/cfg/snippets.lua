local vim = _G.vim
local utils = require("cfg.utils")
local doKeys = utils.doKeys

local M = {}

M.register = function(opts)
  local key = assert(opts.key, "key not specified")

  local content = assert(opts.content, "content not specified")
  if type(content) == "string" then
    content = vim.split(content, "\n", { plain = true, trimempty = true })
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
end

M.use = function(key)
  local s = (vim.b.snippets or {})[key]
  if s == nil then
    error(("No such key: %s"):format(key))
  end

  if vim.trim(vim.fn.getline(".")) ~= "" then
    doKeys("o")
  end
  local start_line = vim.fn.line(".")

  doKeys("mz")
  vim.fn.setline(".", s.content)
  doKeys("`z")

  local line_count = #(s.content)

  if s.reindent then
    doKeys(("=%dj"):format(line_count))
  end

  if s.marker ~= nil then
    utils.searchLiteral(s.marker)
    if vim.fn.line(".") >= start_line + line_count then
      error("found marker past the inserted lines. whoops!")
    end

    local char_count = vim.fn.strcharlen(marker)
    assert(char_count > 0, "WHY IS THE CHAR COUNT <= 0")
    doKeys(("d%dl"):format(char_count))
  end
end

return M
