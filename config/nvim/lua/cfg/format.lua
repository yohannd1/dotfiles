local M = {}

local vim = _G.vim
local utils = require("cfg.utils")

local append = vim.fn.append

local getFormatOpts = function()
  local opts = vim.b.format_opts
  if opts == nil then
    opts = {}

    -- Try to get legacy format option (just a shell command)
    local legacy_cmd = vim.b.format_command
    if legacy_cmd ~= nil then
      assert(type(legacy_cmd) == "string", "legacy vim.b.format_command found, but it's not a string!")
      opts.command = { "exec", { "sh", "-c", legacy_cmd } }
    end
  end

  return opts
end

local getBufContent = function()
  return table.concat(vim.fn.getline(1, "$"), "\n")
end

M.formatBuffer = function()
  local BUF_TITLE = "*Format Errors*"

  local fmt_opts = getFormatOpts()

  local command = fmt_opts.command
  if command == nil then
    error("No format command found (set it with vim.b.format_opts.command)")
  end

  if command[1] == "exec" then
    local result = vim.system(command[2], { text = true, stdin = getBufContent() }):wait()

    -- local successful = vim.v.shell_error == 0
    local successful = result.code == 0
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
      vim.fn.setline(1, vim.split(result.stdout, "\n", { trimempty = true }))
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
        for _, line in ipairs(vim.split(result.stderr, "\n", { trimempty = true })) do
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
  elseif command[1] == "vim.cmd" then
    vim.cmd(command[2])
  elseif command[1] == "function" then
    command[2]()
  else
    error(("Unknown command type %q"):format(command[1]))
  end
end

return M
