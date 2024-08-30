local vim = _G.vim
local M = {}
M.os = {}
M.os.is_android = vim.fn.isdirectory("/sdcard") ~= 0
M.os.is_windows = (vim.fn.has("win32") ~= 0 or vim.fn.has("win64") ~= 0)
M.os.is_tty = vim.env.DISPLAY == "" and not M.os.is_android

M.log = {}
M.log.history = {}
M.log.add = function(msg)
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
  vim.api.nvim_set_keymap(m, lhs, rhs, args or {})
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

local services_mt = {}
services_mt.__index = function(_, key)
  error("No service registered with the name " .. key)
end
M.services = setmetatable({}, services_mt)

M.randomHexString = function(length)
  local CHARS = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"}
  local acc = {}

  for _ = 1, length do
    local n = math.floor(math.random() * 16)
    local c = CHARS[n+1]
    table.insert(acc, c)
  end

  return table.concat(acc)
end

M.loadColorschemeFromYaml = function(path)
  local colors = {}
  local tryParseLine = function(l)
    local id, color = l:match([[base(0[0-9A-Z]): "([0-9a-zA-Z]+)"]])
    if id == nil or color == nil then
      return nil
    end
    colors[id] = "#" .. color
  end

  local fd = assert(io.open(path), "Failed to open theme file at " .. path)
  while true do
    local line = fd:read("line")
    if line == nil then break end
    tryParseLine(line)
  end
  fd:close()

  return colors
end

M.doKeys = function(keys)
  vim.cmd.normal({
    args = { vim.api.nvim_replace_termcodes(keys, true, true, true) },
    bang = true
  })
end

M.xor = function(a,b)
  return (not a) ~= (not b)
end

M.tableJoin = function(...)
  local result = {}
  for _, t in ipairs({...}) do
    for _, v in ipairs(t) do table.insert(result, v) end
  end
  return result
end

do
  local splitWindow = function(dir)
    if dir == "down" then
      vim.cmd.split()
      vim.cmd.wincmd("j")
    elseif dir == "up" then
      vim.cmd.split()
    elseif dir == "left" then
      vim.cmd.vsplit()
    elseif dir == "right" then
      vim.cmd.vsplit()
      vim.cmd.wincmd("l")
    else
      error(("Invalid split direction: %s"):format(dir))
    end
    vim.cmd.enew()
  end

  local windows = {}
  M.uni_win = {}
  M.uni_win.get = function(id)
    local w = windows[id]
    return (w ~= nil and vim.api.nvim_win_is_valid(w)) and w or nil
  end
  M.uni_win.focus = function(id, opts)
    opts = opts or {}
    local create_direction = opts.create_direction or "down"

    local w = windows[id]
    if w == nil or not vim.api.nvim_win_is_valid(w) then
      splitWindow(create_direction)
      w = vim.api.nvim_get_current_win()
      windows[id] = w
    else
      vim.api.nvim_set_current_win(w)
    end
    return w
  end
  M.uni_win.drop = function(id)
    windows[id] = nil
  end
  M.uni_win.delete = function(id)
    local w = windows[id]
    if w ~= nil and vim.api.nvim_win_is_valid(w) then
      vim.api.nvim_win_close(w, true)
      windows[id] = nil
    end
  end

  local buffers = {}
  M.uni_buf = {}
  M.uni_buf.get = function(id)
    local b = buffers[id]
    return (b ~= nil and vim.api.nvim_buf_is_valid(b)) and b or nil
  end
  M.uni_buf.focus = function(id, opts)
    opts = opts or {}
    local create_fn = assert(opts.create_fn, "missing create_fn field")
    local replace = opts.replace or false

    local makeBuf = function()
      create_fn()
      local b = vim.api.nvim_get_current_buf()
      buffers[id] = b
      return b
    end

    local b = buffers[id]
    if b == nil or not vim.api.nvim_buf_is_valid(b) then
      makeBuf()
    elseif replace then
      local b_new = makeBuf()
      if vim.api.nvim_buf_is_valid(b) then
        -- might be false in the rare case the buffer automatically replaces the other one... :v
        vim.api.nvim_buf_delete(b, {force = true, unload = false})
      end
      b = b_new
    else
      vim.api.nvim_set_current_buf(b)
    end

    return b
  end
  M.uni_buf.drop = function(id)
    buffers[id] = nil
  end
  M.uni_buf.delete = function(id)
    vim.api.nvim_buf_delete(buffers[id], {force = true, unload = false})
    buffers[id] = nil
  end
end

M.searchLiteral = function(query)
  -- I'm not fully sure about how to do this but I think \V has got me covered.
  -- :help /\V
  local escaped = "\\V" .. vim.fn.escape(query, "\\")
  vim.fn.search(escaped)
end

return M
