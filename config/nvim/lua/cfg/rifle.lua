local dummy = _G.dummy
local vim = _G.vim
local executable = vim.fn.executable
local utils = require("cfg.utils")

dummy.rifle = function(command)
  if executable("rifle-run") == 0 then
    error("could not find `rifle-run` in PATH")
  end

  local has_display = (vim.env.DISPLAY ~= nil) or (vim.env.WAYLAND_DISPLAY ~= nil)
  local supports_popup = has_display and executable("termup") ~= 0
  local default_rifle_mode = has_display and "popup" or "buffer"
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
    vim.cmd("split")
    vim.cmd("wincmd j")
    vim.cmd("enew")
    vim.fn.termopen(cmd)
    vim.cmd("normal! i")
  elseif rifle_mode == "silent" then
    vim.fn.jobstart(cmd)
  else
    error(("invalid rifle mode: %s"):format(rifle_mode))
  end
end

vim.cmd([[ command! -nargs=1 Rifle call v:lua.dummy.rifle(<f-args>) ]])
