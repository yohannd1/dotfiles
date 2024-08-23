local M = {}

local vim = _G.vim
local utils = require("cfg.utils")

local append = vim.fn.append

M.formatBuffer = function()
  -- TODO: use the same window-creation API that rifle uses

  local format_command = vim.b.format_command
  if format_command == nil then
    print("No format command found (set it with b:format_command)")
    return
  end

  local lines = vim.fn.getline(1, "$")

  local in_file = vim.fn.tempname()
  vim.fn.writefile(lines, in_file)

  local cmd = ("%s < %s"):format(format_command, in_file)
  local output = vim.fn.systemlist(cmd)

  local BUF_TITLE = "*Format Errors*"

  if vim.v.shell_error ~= 0 then
    vim.g.last_format_err = output
    print("Formatting failed - see buffer for more info")

    if vim.fn.bufname(BUF_TITLE) == "" then
      vim.cmd.split()
      vim.cmd.wincmd("j")
      vim.cmd.enew()
      vim.cmd.file(BUF_TITLE)
      vim.cmd([[ setlocal buftype=nofile bufhidden=delete noswapfile nobuflisted ]])
    else
      local bw = vim.fn.bufwinid(BUF_TITLE)
      if bw ~= -1 then
        vim.fn.win_gotoid(bw)
      else
        vim.cmd.split()
        vim.cmd.wincmd("j")
        vim.cmd.buffer(BUF_TITLE)
      end
    end

    vim.opt_local.modifiable = true
    utils.doKeys("ggdG")
    append("$", "Formatting failed:")
    for _, line in ipairs(output) do
      append("$", "  " .. line)
    end
    utils.doKeys("ggdd")
    vim.opt_local.modifiable = false
  else
    local bw = vim.fn.bufwinid(BUF_TITLE)
    if bw ~= -1 then
      vim.fn.nvim_win_close(bw, false)
    end

    -- TODO: turn this into a "snapshot position" function
    -- something like utils.savePosition(); utils.restorePosition()
    local l_win_top = vim.fn.line("w0")
    local c_line = vim.fn.line(".")
    local c_col = vim.fn.col(".")
    utils.doKeys("ggdG")
    vim.fn.setline(1, output)
    utils.doKeys(("%dGzt"):format(l_win_top))
    utils.doKeys(("%dG%d|"):format(c_line, c_col))
  end

  vim.fn.delete(in_file)
end

return M
