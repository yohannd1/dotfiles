local utils = require("cfg.utils")

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local nvim_exec = vim.api.nvim_exec

local setLocals = function(locals)
    for k, v in pairs(locals) do
        vim.opt_local[k] = v
    end
end

local ft = {}

ft.asm = function()
    setLocals { expandtab = false, sw = 8, ts = 8 }
end

return function()
    -- TODO: start using this after I move the entire "augroup buffer_load" section into here
    -- augroup("buffer_load", {clear = true})

    autocmd("FileType", {
        pattern = "*",
        group = "buffer_load",
        callback = function()
            -- Filetype execution
            local filetype = vim.o.filetype
            if ft[filetype] then
                ft[filetype]()
            else
                local cmd = [[
                    if has_key(g:, "ft") && has_key(g:ft, &filetype)
                        call g:ft[&filetype]()
                    endif
                ]]

                nvim_exec(cmd, nil)
            end
        end
    })
end
