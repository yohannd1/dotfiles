-- vim: fdm=marker foldenable
-- Preparation {{{

local vim = _G.vim

local utils = require("cfg.utils")
local setLocals = utils.setLocals

local snippets = require("cfg.snippets")

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

local defaultJoinMatcher = function(l_cur, l_next)
  return not (l_cur:match("[(%[{]$") or l_next:match("^[)%]}]"))
end

local lispJoinMatcher = function(l_cur, l_next)
  return not l_cur:match("[(%[{]$") and not l_next:match("^[)%]}]")
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

-- add a simple generic snippet
local addSnippet = function(key, content)
  snippets.register({
    key = key,
    content = content,
    reindent = true,
  })
end

-- add a simple generic snippet w/ marker
local addMSnippet = function(key, content)
  snippets.register({
    key = key,
    content = content,
    marker = "<[@]>",
    reindent = true,
  })
end

local findPattTillRoot = function(patt)
  local res = vim.fs.root(0, function(name, _)
    return name:match(patt) ~= nil
  end)

  return not (res == nil or vim.fn.isdirectory(res) ~= 0)
end

local ft = {}
local ext_ft = {}
local ft_hooks = {}

local initialize = function()
  augroup("buffer_load", { clear = true })

  autocmd({"BufNewFile", "BufReadPost"}, {
    pattern = "*",
    group = "buffer_load",
    callback = function()
      local filename = vim.fn.expand("%f")
      for ext, v in pairs(ext_ft) do
        if utils.string.endsWith(filename, "." .. ext) then
          if type(v) == "function" then
            v()
          else
            vim.opt_local.filetype = v
          end
          break
        end
      end
      for _, f in ipairs(ft_hooks) do
        f()
      end
    end
  })

  if not utils.os.is_android and vim.fn.executable("filehist") ~= 0 then
    local function tryAddCurFileToRec()
      local path = vim.fn.expand("%:p")
      if path ~= "" then
        vim.fn.jobstart({"filehist", "add", path})
      end
    end

    autocmd({"BufNewFile", "BufRead"}, {
      pattern = "*",
      group = "buffer_load",
      callback = tryAddCurFileToRec,
    })
  end

  autocmd("FileType", {
    pattern = "*",
    group = "buffer_load",
    callback = function()
      -- Configure better join
      vim.b.better_join_whitespace_matcher = defaultJoinMatcher

      -- Filetype execution
      local callback = ft[vim.o.filetype]
      if callback then callback() end

      if vim.fs.root(0, "Makefile") ~= nil then
        vim.b.rifle_ft = "@make"
      end
    end
  })

  if vim.g.has_vim_auto_popmenu then
    local apcReenable = function()
      if vim.b.apc_enable ~= 1 then return end
      vim.cmd.ApcEnable()
    end
    autocmd("BufEnter", {
      pattern = "*",
      group = "buffer_load",
      callback = apcReenable,
    })
  end

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
ext_ft.lang = "lang"
ext_ft.alg = "visualg"
ext_ft.as = "actionscript"
ext_ft.asm = "nasm"
ext_ft.acr = "acrylic"
ext_ft.buzz = "buzz"
ext_ft.clj = "clojure"
ext_ft.fx = "c"
ext_ft.gml = "gml"
ext_ft.h = "c"
ext_ft.jl = "julia"
ext_ft.mpp = "cpp"
ext_ft.PKGBUILD = "PKGBUILD"
ext_ft.rpy = "python"
ext_ft.scrbl = "scribble"
ext_ft.terra = "terra"
ext_ft.xdc = "tcl" -- constraint files
ext_ft.tsx = function()
  if vim.fn.getline(1):find("<?xml") == 1 then
    setLocals { filetype = "xml" }
  end
end
ext_ft.v = function()
  local filetype = vim.fn.search("^module\\>") > 0 and "verilog" or "vlang"
  setLocals { filetype = filetype }
end

table.insert(ft_hooks, function()
  local filename = vim.fn.expand("%f")
  if vim.endswith(filename, "hyprland.conf") then
    vim.opt_local.filetype = "hyprlang"
  end
end)

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
  setTabIndent(4)
  setLocals {
    foldmethod = "syntax",
    commentstring = "/* %s */", -- TODO: is there any way to figure out whether the current project is using C89 or C99? cuz this is awful...
    textwidth = 80,
    cinoptions = "g0,:0,l1,(1s"
  }

  vim.b.format_command = "fmt.clang c"

  addSnippet("s", "#include <stdio.h>")
  addMSnippet("m", [[
    int main(void) {
      <[@]>
    }
  ]])
end

ft.cpp = function()
  setLocals {
    foldmethod = "syntax",
    commentstring = "// %s",
    cinoptions = "g0,:0,l1,(1s,N-s"
  }

  vim.b.format_command = "fmt.clang c++"

  addSnippet("s", "#include <iostream>")
  addSnippet("v", "#include <vector>")
  addSnippet("M", "#include <memory>")
  addMSnippet("m", [[
    int main() {
      <[@]>
    }
  ]])
end

ft.sh = function()
  setSpaceIndent(2)
  setLocals {
    foldmethod = "syntax",
  }

  vim.b.is_bash = 1
  vim.b.sh_fold_enabled = (
    0
    -- + 1 -- fold functions
    -- + 2 -- fold heredoc
    -- + 4 -- fold if/for/case/...
  )
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
  vim.b.better_join_whitespace_matcher = lispJoinMatcher
  setLocals {
    foldmethod = "syntax",
  }
end

ft.nim = function()
  setSpaceIndent(2)
  setLocals {
    foldmethod = "syntax",
  }

  local found = findPattTillRoot("%.nimble$")
  vim.b.rifle_ft = found and "@nimble" or "nim"
end
ft.nims = ft.nim

ft.haskell = function()
  setSpaceIndent(2)
end

ft.html = function()
  vim.b.rifle_mode = "silent"

  snippets.register({
    key = "m",
    content = [[
      <!DOCTYPE html>
      <html>
        <head>
          <title>Title</title>
          <meta charset="UTF-8"/>
          <meta name="viewport" content="width=device-width,initial-scale=1"/>
          <link rel="stylesheet" href="style.css"/>
        </head>

        <body>
          <p>Hello, World!</p>
          <[@]>
        </body>
      </html>
    ]],
    marker = "<[@]>",
    reindent = true,
  })
end

ft.rust = function()
  setLocals {
    -- foldmethod = "syntax",
    textwidth = 100,
  }

  local found = vim.fs.root(0, "Cargo.toml") ~= nil
  vim.b.rifle_ft = found and "@cargo" or "rust"
  vim.b.format_opts = { command = { "vim.cmd", "RustFmt" } }

  addMSnippet("m", [[
    fn main() {
      <[@]>
    }
  ]])
end

vim.g.java_ignore_javadoc = true -- has to be before loading the buffer
ft.java = function()
  local found = vim.fs.root(0, "gradlew") ~= nil
  vim.b.rifle_ft = found and "@gradlew" or "java"
  vim.b.format_command = "google-java-format --aosp -"

  addSnippet("m", [[
    public class Main {
      public static void main(String[] args) {
        System.out.println("Hello, World!");
      }
    }
  ]])
end

ft.make = function()
  setTabIndent(8)
end

ft.tex = function()
  vim.b.rifle_ft = "tex"
  vim.b.rifle_mode = "bg_buffer"
  -- vim.b.rifle_window_height = 8
  setLocals { textwidth = 72 }

  addMSnippet("m", [[
    \documentclass{article}

    \title{Hello, world!}

    \begin{document}

    \maketitle

    <[@]>

    \end{document}
  ]])

  addMSnippet("m", [[
    \begin{align*}
    <[@]>
    \end{align*}
  ]])
end

ft.plaintex = ft.tex

ft.zig = function()
  local found = vim.fs.root(0, "build.zig") ~= nil
  vim.b.rifle_ft = found and "@zig-build" or "zig"

  vim.b.format_command = "zig fmt --stdin"

  setLocals {
    textwidth = 120,
  }

  addSnippet("s", [[const std = @import("std");]])
  addMSnippet("m", [[
    pub fn main() anyerror!void {
      <[@]>
    }
  ]])
  addMSnippet("t", [[
    test {
      <[@]>
    }
  ]])
end

ft.moon = function()
  setSpaceIndent(2)
end

ft.javascript = function()
  vim.b.format_command = "fmt.js-prettier"
end

ft.python = function()
  vim.b.format_command = "black - -q"

  vim.cmd("syn keyword Boolean True")
  vim.cmd("syn keyword Boolean False")
  vim.cmd("syn keyword Boolean None")

  addSnippet("c", [[from dataclasses import dataclass]])
  addSnippet("a", [[from abc import abstractmethod]])
  addSnippet("m", [[
    def main() -> None:
      pass

    if __name__ == "__main__":
      main()
  ]])
end

ft.yaml = function()
  setSpaceIndent(2)
end

ft.markdown = function()
  _G.dummy.markdownFold = function(lnum)
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
    fdm = "expr", foldexpr = [[ v:lua.dummy.markdownFold(vim.v.lnum) ]],
    textwidth = 72,
    autoindent = false,
    commentstring = "<!-- %s -->",
  }
end

ft.vlang = function()
  setTabIndent(4)
  vim.b.format_command = "fmt.vlang"
end

ft.fennel = function()
  vim.cmd([[ hi link FennelKeyword String ]])
end

ft.gdscript = function()
  setTabIndent(4)
  vim.cmd([[ hi link @string.special.url.gdscript Function ]])
end

ft.d = function()
  setSpaceIndent(4)
  vim.b.format_command = "dfmt"
  setLocals { commentstring = "// %s" }
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
  setTabIndent(4)

  vim.opt_local.iskeyword = vim.opt_local.iskeyword + "-"
end

ft.acrylic = function()
  addSnippet("t", "%:title ")
  vim.b.rifle_mode = "bg_buffer"

  vim.b.item_toggletodo_preferred_done = "x"
  vim.b.todo_queries = {
    "^(\\s*)([-*]\\s+)?\\( \\)",
    "^(\\s*)([-*]\\s+)?\\[ \\]",
  }

  setSpaceIndent(2)
  setLocals { foldenable = true }

  -- Custom syntax
  vim.cmd([[ syn match acrXDatetime /\v\d{1,2}(:\d{2}){1,2}(AM|PM)?/ ]])
  vim.cmd([[ syn match acrXDatetime /\v\d{4}\/\d{2}\/\d{2}/ ]])
  vim.cmd([[ hi link acrXDatetime Special ]])
  vim.cmd([[ syn match acrXTodo /\v<(TODO|FIXME|XXX)>/ ]])
  vim.cmd([[ hi link acrXTodo Todo ]])
end

ft.PKGBUILD = function()
  vim.o.syntax = "bash" -- because treesitter is breaking the PKGBUILD syntax for some reason
  setSpaceIndent(2)
end

ft.lisp = function()
  vim.b.better_join_whitespace_matcher = lispJoinMatcher
end

ft.janet = function()
  vim.b.better_join_whitespace_matcher = lispJoinMatcher
end

ft.haxe = function()
  setTabIndent(4)

  -- FIXME: this might error out if the buffer file changes
  vim.b.format_command = ("haxelib run formatter --stdin -s %s"):format(vim.fn.expand("%:p"))
end

ft.odin = function()
  setTabIndent(4)
end

ft.forth = function()
  setSpaceIndent(2)
end

ft.go = function()
  setTabIndent(4)
end

ft.buzz = function()
  vim.o.syntax = "rust"
end

ft.lua = function()
  setSpaceIndent(2)
  -- vim.lsp.start({
  --   name = 'lua-language-server',
  --   cmd = {'lua-language-server'},
  -- })
end

ft.gsl = function()
  setTabIndent(4)
end

ft.cs = function()
  setLocals { commentstring = "// %s" }
end

ft.editorconfig = function()
  addSnippet("m", [[
    root = true

    [*.{cc,hh}]
    indent_style = space
    indent_size = 4
    trim_trailing_whitespace = true
    insert_final_newline = true
  ]])
end

ft.verilog = function()
  vim.b.format_command = "verible-verilog-format -"
  vim.b.verilog_indent_modules = 1
  setSpaceIndent(2)
end

ft.systemverilog = function()
  vim.b.format_command = "verible-verilog-format -"
  vim.b.verilog_indent_modules = 1
  setSpaceIndent(2)

  snippets.register({
    key = "m",
    content = [[
module m_top;
  integer a, b;

  initial begin
    a = 10;
    b = 15;
    $display("Hello, world! a+b = %d", a + b);
    $finish;
  end
endmodule
    ]],
    reindent = false,
  })
end

ft.vhdl = function()
  vim.g.vhdl_indent_genportmap = 0
  vim.g.vhdl_indent_rhsassign = 0
  setLocals {
    -- FIXME: what IS going on here
    commentstring = "-- %s",
    comments = "b:--,s:--,sO:* -,mO:*  ,exO:*/,s1:/*,mb:*,ex:*/",
  }

  snippets.register({
    key = "m",
    content = [[
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library std;

entity main is
end entity main;

architecture behavioral of main is
    constant N : natural := 8;
begin
    process
    begin
        report "Hello, world! N = " & natural'image(N);

        std.env.stop;
        wait;
    end process;
end architecture;
    ]],
    reindent = false,
  })
end

ft.ysh = function()
  setSpaceIndent(2)
  setLocals { commentstring = "# %s" }
end

initialize()
