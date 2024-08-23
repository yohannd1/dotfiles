local dummy = _G.dummy
local vim = _G.vim
local executable = vim.fn.executable
local utils = require("cfg.utils")

local static = {}
local M = {}

local splitWindow = function()
  local dir = vim.g.rifle_split_direction or vim.b.rifle_split_direction or "down"

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
  end
end

M.run = function(command)
  if executable("rifle-run") == 0 then
    error("could not find `rifle-run` in PATH")
  end

  local has_display = (vim.env.DISPLAY ~= nil) or (vim.env.WAYLAND_DISPLAY ~= nil)
  local supports_popup = has_display and executable("termup") ~= 0
  local default_rifle_mode = supports_popup and "popup" or "buffer"
  local rifle_mode = vim.b.rifle_mode or vim.g.rifle_mode or default_rifle_mode
  local rifle_ft = vim.b.rifle_ft or vim.o.filetype

  -- because neovim on termux is struggling!!!!11 (FIXME)
  local dotfiles = assert(vim.env.DOTFILES, "$DOTFILES not set (sorry)")
  local rifle_exe_path = ("%s/scripts/rifle-run"):format(dotfiles)
  local cmd = {"bash", rifle_exe_path, command, rifle_ft, vim.fn.expand("%:p")}

  if rifle_mode == "popup" then
    local tbl = utils.tableJoin({"termup", "runread"}, cmd)
    vim.fn.jobstart(tbl)
  elseif rifle_mode == "buffer" then
    -- open the window or go to the last one, if it exists
    if static.window == nil or not vim.api.nvim_win_is_valid(static.window) then
      splitWindow()
      static.window = vim.api.nvim_get_current_win()
    else
      vim.api.nvim_set_current_win(static.window)
    end

    vim.cmd.enew()
    vim.fn.termopen(utils.tableJoin({"runread"}, cmd))

    -- save the buffer's handle and, if there was an old one, force-delete it
    local old_buffer = static.buffer
    static.buffer = vim.api.nvim_get_current_buf()
    if old_buffer ~= nil and vim.api.nvim_buf_is_valid(old_buffer) then
      vim.api.nvim_buf_delete(old_buffer, {force = true, unload = false})
    end

    vim.cmd([[ normal! i ]])
  elseif rifle_mode == "silent" then
    vim.fn.jobstart(cmd)
  else
    error(("invalid rifle mode: %s"):format(rifle_mode))
  end
end

M.rifleReset = function()
  static.window = nil
  static.buffer = nil
end

vim.cmd([[ command! -nargs=1 Rifle call v:lua.require('cfg.rifle').run(<f-args>) ]])
vim.cmd([[ command! RifleReset call v:lua..require('cfg.rifle').rifleReset() ]])

return M
