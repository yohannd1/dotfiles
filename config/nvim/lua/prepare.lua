local utils = require("cfg.utils")

-- compatibility vars
vim.env.VIM_INIT = vim.g.config_root .. "/init.vim"
vim.env.GVIM_INIT = vim.g.config_root .. "/ginit.vim"
vim.g.is_win = utils.os.is_windows

return nil
