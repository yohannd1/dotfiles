local M = {}

local vim = _G.vim
local utils = require("cfg.utils")

local append = vim.fn.append

M.formatBuffer = function()
  local BUF_TITLE = "*Format Errors*"

  local format_command = vim.b.format_command
  if format_command == nil then
    print("No format command found (set it with b:format_command)")
    return
  end

  local lines = vim.fn.getline(1, "$")

  local tmp_in_file = vim.fn.tempname()
  vim.fn.writefile(lines, tmp_in_file)

  local cmd = ("%s < %s"):format(format_command, tmp_in_file)
  local output = vim.fn.systemlist(cmd)

  vim.fn.delete(tmp_in_file)

  local successful = vim.v.shell_error == 0
  if successful then
    local w = utils.uni_win.get("aux")
    local b = utils.uni_buf.get("format_errors")
    if w ~= nil and b ~= nil and vim.fs.basename(vim.api.nvim_buf_get_name(b)) == BUF_TITLE then
      utils.uni_buf.delete("format_errors")
      utils.uni_win.delete("aux")
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
  else
    local createFormatError = function()
      vim.cmd.enew()
      vim.cmd.file(BUF_TITLE)
      vim.cmd([[ setlocal buftype=nofile bufhidden=delete noswapfile nobuflisted ]])
      vim.b.is_format_buffer = true
      vim.opt_local.modifiable = true
      vim.cmd([[ normal! ggdG ]])
      append("$", "Formatting failed:")
      for _, line in ipairs(output) do
        append("$", "  " .. line)
      end
      utils.doKeys("ggdd")
      vim.opt_local.modifiable = false
    end

    utils.uni_win.focus("aux", { create_direction = "down" })
    utils.uni_buf.focus("format_errors", {
      replace = true,
      create_fn = createFormatError,
    })

    print(("Formatting failed - see buffer %q for more info"):format(BUF_TITLE))
  end
end

return M
