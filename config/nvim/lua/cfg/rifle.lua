local vim = _G.vim
local executable = vim.fn.executable
local utils = require("cfg.utils")

local M = {}

M.run = function(command)
  if executable("rifle-run") == 0 then
    error("could not find `rifle-run` in PATH")
  end

  local has_display = (vim.env.DISPLAY ~= nil) or (vim.env.WAYLAND_DISPLAY ~= nil)
  local supports_popup = has_display
  local default_rifle_mode = supports_popup and "popup" or "buffer"
  local split_direction = vim.g.rifle_split_direction or vim.b.rifle_split_direction or "right"
  local rifle_mode = vim.b.rifle_mode or vim.g.rifle_mode or default_rifle_mode
  local rifle_ft = vim.b.rifle_ft or vim.o.filetype

  -- because neovim on termux is struggling!!!!11 (FIXME)
  local dotfiles = assert(vim.env.DOTFILES, "$DOTFILES not set (sorry)")
  local rifle_run_path = ("%s/scripts/rifle-run"):format(dotfiles)
  local runread_path = ("%s/scripts/runread"):format(dotfiles)
  local cmd = {rifle_run_path, command, rifle_ft, vim.fn.expand("%:p")}
  local cmd_prefix = utils.os.is_android and {"bash"} or {}

  if rifle_mode == "popup" then
    local tbl = utils.tableJoin({"termup", runread_path}, cmd)
    vim.fn.jobstart(tbl)
  elseif rifle_mode == "buffer" then
    local createRifleTerm = function()
      vim.cmd.enew()
      vim.fn.termopen(utils.tableJoin({cmd_prefix, runread_path}, cmd))
      vim.cmd.file("*Rifle*")
      vim.cmd([[ normal! i ]])
    end

    utils.uni_win.focus("aux", { create_direction = split_direction })
    utils.uni_buf.focus("rifle", {
      replace = true,
      create_fn = createRifleTerm,
    })
  elseif rifle_mode == "silent" then
    vim.fn.jobstart(utils.tableJoin({cmd_prefix, cmd}))
  else
    error(("invalid rifle mode: %s"):format(rifle_mode))
  end
end

M.rifleReset = function()
  utils.uni_buf.drop("aux")
end

vim.cmd([[ command! -nargs=1 Rifle call v:lua.require('cfg.rifle').run(<f-args>) ]])
vim.cmd([[ command! RifleReset call v:lua.require('cfg.rifle').rifleReset() ]])

return M
