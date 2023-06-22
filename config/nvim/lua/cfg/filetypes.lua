-- vim: fdm=marker foldenable
-- Preparation {{{

local vim = _G.vim
local utils = require("cfg.utils")

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
local nvim_exec = vim.api.nvim_exec

local setLocals = function(locals)
    for k, v in pairs(locals) do
        vim.opt_local[k] = v
    end
end

local setTabIndent = function(indent)
    setLocals {
        shiftwidth = indent,
        tabstop = indent,
        expandtab = false,
    }
end

local setSpaceIndent = function(indent)
    setLocals {
        shiftwidth = indent,
        tabstop = 8,
        expandtab = true,
    }
end

local addSnippets = function(snips)
    for k, v in pairs(snips) do
        vim.fn["AddSnippet"](k, v)
    end
end

local ft = {}
local ext_ft = {}

local func = function()
    augroup("buffer_load", {clear = true})

    autocmd({"BufNewFile", "BufReadPost"}, {
        pattern = "*",
        group = "buffer_load",
        callback = function()
            local filename = vim.fn.expand("%f")
            for ext, v in pairs(ext_ft) do
                if utils.string.endsWith(filename, ext) then
                    if type(v) == "function" then
                        v()
                    else
                        vim.opt_local.filetype = v
                    end
                    break
                end
            end
        end
    })

    autocmd({"BufNewFile", "BufRead"}, {
        pattern = "*",
        group = "buffer_load",
        callback = "AddToRecFile",
    })

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

                nvim_exec(cmd, false)
            end

            vim.fn["SetupMakefileRifle"]()
        end
    })

    autocmd("BufEnter", {
        pattern = "*",
        group = "buffer_load",
        callback = "ApcReenable",
    })

    autocmd("TermOpen", {
        pattern = "*",
        group = "buffer_load",
        callback = function()
            setLocals { relativenumber = false, number = false, cursorcolumn = false }
        end
    })
end

-- }}}

-- Extension -> filetype
ext_ft.rpy = "python"
ext_ft.terra = "terra"
ext_ft.as = "actionscript"
ext_ft.fx = "c"
ext_ft.h = "c"
ext_ft.clj = "clojure"
ext_ft.alg = "visualg"
ext_ft.jl = "julia"
ext_ft.scrbl = "scribble"
ext_ft.mpp = "cpp"
ext_ft.PKGBUILD = "PKGBUILD"
ext_ft.asm = "nasm"
ext_ft.acr = "acrylic"
ext_ft.gml = "gml"
ext_ft.tsx = function()
    if vim.fn.getline(1):find("<?xml") == 1 then
        setLocals { filetype = "xml" }
    end
end

ft.actionscript = function()
    setLocals {
        syntax = "javascript",
    }
    setTabIndent(4)
end

ft.gml = function()
    setLocals {
        syntax = "javascript",
        foldmethod = "marker",
        foldenable = true,
        foldmarker = "#region,#endregion",
    }
    setTabIndent(4)
end

ft.asm = function()
    setTabIndent(8)
end

ft.xdefaults = function()
    setLocals { commentstring = "!%s" }
end

ft.c = function()
    setTabIndent(8)
    setLocals {
        foldmethod = "syntax",
        commentstring = "/* %s */",
        textwidth = 80,
    }

    vim.b.format_command = "clang-multicfg-format c"

    addSnippets {
        s = "#include <stdio.h>",
        m = "int main(void) {<CR><CR>}<Up>",
    }
end

ft.cpp = function()
    setLocals {
        foldmethod = "syntax",
        commentstring = "// %s",
    }

    vim.b.format_command = "clang-multicfg-format cpp"

    addSnippets {
        s = "#include <iostream>",
        v = "#include <vector>",
        M = "#include <memory>",
        m = "int main() {<CR><CR>}<Up>",
    }
end

ft.sh = function()
    setSpaceIndent(2)
    setLocals {
        foldmethod = "syntax",
    }

    -- " let g:is_bash = 1
    -- " let g:sh_fold_enabled = 0

    -- " 1 (001): fold functions
    -- " let g:sh_fold_enabled += 1

    -- " 2 (010): fold heredoc
    -- " let g:sh_fold_enabled += 2

    -- " 4 (100): fold if/for/case/...
    -- " let g:sh_fold_enabled += 4
end

ft.zsh = function()
    setSpaceIndent(2)
    setLocals {
        foldmethod = "syntax",
    }
end

ft.vim = function()
    setLocals {
        foldmethod = "marker",
        textwidth = 72,
    }
end

ft.hy = function()
    setSpaceIndent(2)
    setLocals {
        foldmethod = "syntax",
    }
end

ft.nim = function()
    setSpaceIndent(2)
    setLocals {
        foldmethod = "syntax",
    }

    local this_folder = vim.fn.expand("%:p:h")
    local found = vim.fn.ReverseRSearch(this_folder, "*.nimble") ~= 0
    vim.b.rifle_ft = found and "@nimble" or "nim"
end
ft.nims = ft.nim

ft.haskell = function()
    setSpaceIndent(2)
end

ft.html = function()
    vim.b.rifle_mode = "silent"

    addSnippets {
        -- hell
        m = "<!DOCTYPE html><CR><html><CR><head><CR><title>Title</title><CR><meta charset=\"UTF-8\"/><CR><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/><CR><link rel=\"stylesheet\" href=\"style.css\"/><CR></head><CR><CR><body><CR><p>Hello, World!</p><CR></body><CR></html><Esc>gg",
    }
end

ft.rust = function()
    setLocals {
        foldmethod = "syntax",
        textwidth = 100,
    }

    local this_folder = vim.fn.expand("%:p:h")
    local found = vim.fn.ReverseRSearch(this_folder, "Cargo.toml") ~= 0
    vim.b.rifle_ft = found and "@cargo" or "rust"

    addSnippets {
        m = "fn main() {<CR><CR>}<Up>",
    }
end

ft.java = function()
    local this_folder = vim.fn.expand("%:p:h")
    local found = vim.fn.ReverseRSearch(this_folder, "gradlew") ~= 0
    vim.b.rifle_ft = found and "@gradlew" or "java"

    vim.b.format_command = "google-java-format --aosp - 2>/dev/null"

    addSnippets {
        m = [[public class Main {<CR>public static void main(String[] args) {<CR>System.out.println("Hello, World!");<CR>}<CR>}<Up><Up><C-o>_]],
    }
end

ft.make = function()
    setTabIndent(8)
end

ft.tex = function()
    vim.b.rifle_ft = "tex"
    vim.b.rifle_mode = "buffer"
    setLocals { textwidth = 72 }
end
ft.plaintex = ft.tex

ft.zig = function()
    local this_folder = vim.fn.expand("%:p:h")
    local found = vim.fn.ReverseRSearch(this_folder, "build.zig") ~= 0
    vim.b.rifle_ft = found and "@zig-build" or "zig"

    vim.b.format_command = "zig fmt --stdin"

    setLocals {
        textwidth = 120,
    }

    addSnippets {
        s = [[const std = @import("std");]],
        m = [[pub fn main() anyerror!void {<CR><CR>}<Up>]],
        t = [[test {<CR><CR>}<Up>]],
    }
end

ft.moon = function()
    setSpaceIndent(2)
end

ft.javascript = function()
    vim.b.format_command = "prettier-stdin"
end

ft.python = function()
    vim.b.format_command = "python3 -m black - 2>/dev/null"

    nvim_exec([[
        syn keyword Boolean True
        syn keyword Boolean False
        syn keyword Boolean None
    ]], false)

    addSnippets {
        m = [[def main():<CR>pass<CR><CR>if __name__ == "__main__":<CR>main()]],
        c = [[from dataclasses import dataclass]],
        a = [[from abc import abstractmethod]],
    }
end

ft.yaml = function()
    setSpaceIndent(2)
end

ft.markdown = function()
    _G.dummy.markdown_fold = function(lnum)
        -- FIXME: more efficient: have a match for '^#+' and count the length, then set that as the fold level
        for count = 5, 1, -1 do
            local s = string.rep("#", count)
            if vim.fn.getline(lnum):find(s) == 1 then
                return ">" .. count
            end
        end

        return "="
    end


    vim.b.rifle_mode = "silent"
    vim.b.rifle_ft = "markdown"

    setSpaceIndent(2)

    setLocals {
        fdm = "expr", foldexpr = [[ v:lua.dummy.markdown_fold(vim.v.lnum) ]],
        textwidth = 72,
        autoindent = false,
        commentstring = "<!-- %s -->",
    }
end

ft.vlang = function()
    setTabIndent(4)
    vim.b.format_command = "fmt-wrapper-v"
end

ft.fennel = function()
    nvim_exec([[ hi link FennelKeyword String ]], false)
end

ft.gdscript = function()
    setTabIndent(4)
end

ft.d = function()
    setSpaceIndent(4)
    vim.b.format_command = "dfmt"
end

ft.json = function()
    vim.b.format_command = "jq ."
end

ft.php = function()
    setLocals { commentstring = "// %s" }
end

ft.apache = function()
    setLocals { commentstring = "# %s" }
end

ft.alda = function()
    setSpaceIndent(2)
end

ft.uxntal = function()
    setTabIndent(8)

    vim.opt_local.iskeyword = vim.opt_local.iskeyword + "-"
end

ft.acrylic = function()
    addSnippets {
        t = "%:title ",
    }

    vim.b.item_toggletodo_preferred_done = "x"

    nvim_exec([[
        let b:todo_queries = ['^(\s*)([-*]\s+)?\( \)', '^(\s*)([-*]\s+)?\[ \]']
    ]], false)

    setSpaceIndent(2)
    setLocals { foldenable = true }

    -- Custom syntax
    nvim_exec([[
        syn match acrXDatetime /\v\d{1,2}(:\d{2}){1,2}(AM|PM)?/
        syn match acrXDatetime /\v\d{4}\/\d{2}\/\d{2}/
        hi link acrXDatetime Special
    ]], false)
end

ft.PKGBUILD = function()
    setSpaceIndent(2)
end

-- function! ft.vimwiki() " {{{
--   setlocal sw=2

--   syn match VimwikiXNodeAttr /\v[A-Za-z0-9_.]+\{/
--   hi link VimwikiXNodeAttr String

--   syn match VimwikiXEscapedHash /\v\\\#/
--   hi link VimwikiXEscapedHash Function

--   syn match VimwikiXTag /\v\#([A-Za-z0-9_]+)(\.[A-Za-z0-9_]+)*/
--   hi link VimwikiXTag Function

--   syn match VimwikiXTodo /\v^\s*([*-]\s+)?\[ \]/
--   syn match VimwikiXTodo /\v^\s*([*-]\s+)?\( \)/

--   syn match VimwikiXDone /\v^\s*([*-]\s+)?\[X\]/
--   syn match VimwikiXDone /\v^\s*([*-]\s+)?\(X\)/

--   syn match VimwikiDatetime /\v\d{1,2}(:\d{2}){1,2}(AM|PM)?/
--   syn match VimwikiDatetime /\v\d{4}\/\d{2}\/\d{2}/
--   hi link VimwikiDatetime Special

--   syn match VimwikiXHeaderAttr /\v^\s*\%:(custom\.)?[A-Za-z_][A-Za-z0-9_]*/
--   hi link VimwikiXHeaderAttr Function

--   syn match VimwikiXFuncCall /\v\@[A-Za-z_][A-Za-z0-9_]*/
--   hi link VimwikiXFuncCall Function

--   syn match VimwikiXFuncSpread /\v\@[A-Za-z_][A-Za-z0-9_]*-\>/
--   hi link VimwikiXFuncSpread Function

--   silent! nunmap <buffer> o
--   silent! nunmap <buffer> O
--   silent! nmap <buffer> <C-h> <BS>

--   call AddSnippet("j", '%:title Journal for <C-r>=strftime("%Y/%m/%d")<CR>')
--   call AddSnippet("t", '%:title ')

--   let b:item_toggletodo_preferred_done = "X"
-- endfunction " }}}

return func
