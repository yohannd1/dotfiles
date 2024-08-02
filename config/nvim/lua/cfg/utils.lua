local vim = _G.vim
local M = {}

M.os = {}
M.os.is_android = vim.fn.isdirectory("/sdcard") ~= 0
M.os.is_windows = (vim.fn.has("win32") ~= 0 or vim.fn.has("win64") ~= 0)
M.os.is_tty = vim.env.DISPLAY == "" and not M.os.is_android

M.log = {}
M.log.history = {}
M.log.addLog = function(message)
  table.insert(M.log.history, os.date("%Y%m%d %H:%M :: ") .. msg)
end
M.log.printAll = function()
  for _, message in ipairs(M.log.history) do
    print(message)
  end
end

M.exec = function(str)
  vim.api.nvim_exec(str, false)
end

M.parseEscapeCode = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

M.overrideTableWith = function(dest, src)
  for k, v in pairs(src) do
    dest[k] = v
  end
end

-- TODO: is this the most efficient way
M.splitIter = function(str, separator)
    local length = str:len()

    local start_p = 1
    local end_p = 1

    return function()
        while true do
            if start_p > length then
                return nil
            end

            if separator == "" then
                local i = end_p
                end_p = end_p + 1
                start_p = end_p
                return str:sub(i, i)
            elseif (str:sub(end_p, end_p) == separator) then
                local s = start_p
                local e = end_p

                end_p = end_p + 1
                start_p = start_p + 1

                return str:sub(s, e-1)
            end

            end_p = end_p + 1
        end
    end
end

M.pathAppend = function(dir)
    local p = vim.env.PATH
    if not p:find(dir) then
        vim.env.PATH = p .. (M.os.is_windows and ";" or ":") .. dir
    end
end

M.string = {}
M.string.endsWith = function(haystack, suffix)
    return string.sub(haystack, -#suffix) == suffix
end

M._features = {}

M.hasIntegerRepr = function(num)
  return tostring(num):match("^%d+$") ~= nil
end

M.addTextInLine = function(text, opts)
  local after_cursor = opts.after_cursor

  local offset = after_cursor and 1 or 0
  if opts.telescope_fix then
    offset = offset - 1
  end

  local str_pos = vim.fn.col(".") - 1 + offset
  local line = vim.fn.getline(".")

  local before = vim.fn.strpart(line, 0, str_pos)
  local after = vim.fn.strpart(line, str_pos)
  local rebuilt = before .. text .. after

  vim.fn.setline(".", rebuilt)
  vim.cmd("normal! " .. (str_pos+2) .. "|")
end

M.sourceIfPresent = function(path)
  if vim.fn.filereadable(path) ~= 0 then
    vim.cmd.source(path)
  end
end

M.map = function(m, lhs, rhs, args)
  local args = args or {}
  vim.api.nvim_set_keymap(m, lhs, rhs, args)
end

M.forChars = function(chars, fn)
  for c in M.splitIter(chars, "") do
    fn(c)
  end
end

M.setLocals = function(opts)
  for k, v in pairs(opts) do
    vim.opt_local[k] = v
  end
end

M.setGlobals = function(opts)
  for k, v in pairs(opts) do
    vim.opt[k] = v
  end
end

M.services = {}

return M
