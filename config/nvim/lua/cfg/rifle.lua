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
  local supports_popup = has_display
  print(has_display)
  local default_rifle_mode = supports_popup and "popup" or "buffer"
  local split_direction = vim.g.rifle_split_direction or vim.b.rifle_split_direction or "right"
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
    local createRifleTerm = function()
      vim.cmd.enew()
      vim.fn.termopen(utils.tableJoin({"runread"}, cmd))
      vim.cmd.file("*Rifle*")
      vim.cmd([[ normal! i ]])
    end

    utils.uni_win.focus("aux", {
      create_direction = split_direction,
    })
    utils.uni_buf.focus("rifle", {
      replace = true,
      create_fn = createRifleTerm,
    })
  elseif rifle_mode == "silent" then
    vim.fn.jobstart(cmd)
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
