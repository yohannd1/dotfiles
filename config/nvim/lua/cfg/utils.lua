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

M.moveCursorHorizontal = function(offset)
  assert(
    M.hasIntegerRepr(math.abs(offset)),
    string.format(
      "Absolute of `offset` (%s) has no integer representation",
      math.abs(offset)
    )
  )

  if offset == 0 then
    return 0
  end

  local direction_str = (offset > 0) and "l" or "h"
  local direction_signal = (offset > 0) and 1 or -1

  for i = 1, math.abs(offset) do
    local ccol = vim.fn.col(".")
    vim.cmd("normal! " .. direction_str)
    if vim.fn.col(".") == ccol then
      -- Didn't move at all this round - it's the end of the line. Let's return already then.
      return i * direction_signal
    end
  end

  return offset
end

M.columnAtCharOffset = function(offset)
  local moved = M.moveCursorHorizontal(offset)
  if moved ~= offset then
    M.moveCursorHorizontal(-moved)
  else
    local ccol = vim.fn.col(".")
    M.moveCursorHorizontal(-offset)
    return ccol
  end
end

M.makeAddTxt = function(after_cursor)
  return function(text)
    assert(text ~= nil, "Argument `text` must not be nil")

    local line = vim.fn.getline(".")
    local start_offset = after_cursor and 0 or -1
    local divide_point = M.columnAtCharOffset(start_offset) or 0

    vim.fn.setline(
      ".",
      table.concat({
        vim.fn.strpart(line, 0, divide_point), -- FIXME: why is this still splitting multibyte chars(?) in half?
        text,
        vim.fn.strpart(line, divide_point),
      }, "")
    )
  end
end

return M
