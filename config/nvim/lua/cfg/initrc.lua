_G.dummy = _G.dummy or {}
local dummy = _G.dummy

local vim = _G.vim
local ucm = _G.useConfModule
local utils = require("cfg.utils")
_G._utils = utils

dummy.log = utils.log.addLog
dummy.showLogs = utils.log.printAll

-- TODO: inside neovim, replace the $EDITOR with a wrapper script that connects
-- to the current neovim instance, opens a buffer, and waits for the buffer to
-- unload before exiting.
--
-- Waiting for neovim to implement --remote-wait to do this. See
-- https://neovim.io/doc/user/remote.html
--
-- This might be possible without --remote-wait already, by using a BufWipeout
-- hook, but I'd need to figure out how that works and then make the wrapper
-- script wait till neovim responds back to it with a message saying the buffer
-- closed. Might not be too hard though?

-- Load configuration from other files
require("cfg.general")
require("cfg.statusline")
require("cfg.keybindings")
require("cfg.filetypes")

-- vim: sw=2 et
