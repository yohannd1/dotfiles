local utils = require("cfg.utils")

return function()
    local vim_runtime_dir = vim.env.VIMRUNTIME

    -- FIXME: am I doing this right
    -- utils.source_if_present(vim_runtime_dir .. "/delmenu.vim") or utils.source_if_present(vim_runtime_dir .. "/menu.vim")

    vim.o.encoding = "utf-8"
    vim.o.langmenu = "en_US"
    vim.env.LANG = "en_US"
end
