-- vim: fdm=marker foldenable foldmarker={{{,}}}

-- Preparations {{{
local vim = _G.vim
local dummy = _G.dummy
local _configs = {}
local M = {}

local ucm = _G.useConfModule
local utils = ucm("utils")
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end

local function plug(arg)
  local plugin = nil
  local config = nil

  if type(arg) == "string" then
    plugin = arg
  elseif type(arg) == "table" then
    plugin = arg[1]
    config = arg.config
  else
    error("Expected string or table, found " .. type(arg))
  end

  if plugin then
    assert(type(plugin) == "string", "Tried to plug non-string")
    vim.cmd(string.format("Plug '%s'", plugin))
  end

  if config then
    table.insert(_configs, arg.config)
  end
end

local firstAvailableDir = function(arg)
  for _, dir in ipairs(arg) do
    if vim.fn.isdirectory(dir) ~= 0 then
      return dir
    end
  end

  return arg.fallback or error("Could not find plugin + no fallback specified")
end

local plugins_old = function() error("used before implementation") end

do
  local plugins = {}
  local plugin_names = {}

  M.add = function(opts)
    local source = assert(opts.source, ".source entry missing")
    local name = opts.name or source
    local condition = (function(x)
      if x == nil then return true
      else return x end
    end)(opts.condition)
    local before = opts.before or function() end
    local after = opts.after or function() end

    plugins[name] = { name = name, source = source, condition = condition, before = before, after = after }
    table.insert(plugin_names, name)
  end

  M.init = function(opts)
    local is_first = (vim.g.is_first or 1) ~= 0
    if not is_first then
      return
    end

    local plugins_to_load = assert(opts.plugins, ".plugins not specified")
    local all_was_specified = (plugins_to_load == "all")
    plugins_to_load = (plugins_to_load == "all") and plugin_names or plugins_to_load

    local root_path = assert(opts.root_path, ".root_path not specified")

    local to_call = {}

    vim.fn["plug#begin"](root_path)

    for _, pname in ipairs(plugins_to_load) do
      local info = assert(plugins[pname], "plugin of name " .. pname .. " is not defined")

      local cresult
      if type(info.condition) == "function" then cresult = info.condition()
      else cresult = info.condition
      end

      if cresult then
        info.before()
        plug(info.source)
        table.insert(to_call, info.after)
      end
    end

    if all_was_specified then
      plugins_old() -- FIXME: legacy code (only when "all" was specified. or something.)
    end

    vim.fn["plug#end"]()

    for _, f in ipairs(_configs) do f() end -- FIXME: legacy code
    for _, f in ipairs(to_call) do f() end
  end
end

local hasExecutable = function(name)
  return vim.fn.executable(name) ~= 0
end

-- }}}

local HOME = assert(os.getenv("HOME"), "could not get home directory")
local pj_code = HOME .. "/pj/code"

-- M.add({
--   name = "nvim-treesitter",
--   source = "nvim-treesitter/nvim-treesitter",
--   condition = not utils.os.is_android,
--   after = function()
--     require("nvim-treesitter.configs").setup {
--       ensure_installed = {
--         "c", "lua", "vim",
--         "vimdoc", "query", "python",
--       },

--       -- Install parsers synchronously (only applied to `ensure_installed`)
--       sync_install = false,

--       -- Automatically install missing parsers when entering buffer
--       auto_install = true,

--       ignore_install = {},

--       highlight = {
--         enable = true,
--         disable = { "gitcommit", "bash", "PKGBUILD", "latex", "janet" },
--         additional_vim_regex_highlighting = true,
--       },
--     }
--   end,
-- })

-- Utils {{{
M.add({
  name = "vim-buftabline",
  source = "ap/vim-buftabline",
  before = function()
    vim.g.buftabline_indicators = 1
  end,
})

M.add({
  name = "vim-commentary",
  source = "tpope/vim-commentary",
})

-- netrw improvement
M.add({
  name = "vim-vinegar",
  source = "tpope/vim-vinegar",
})

-- }}}

-- Filetype plugins {{{
M.add({
  name = "acrylic.vim",
  source = firstAvailableDir({ pj_code .. "/acrylic.vim", fallback = "YohananDiamond/acrylic.vim" }),
})

M.add({
  name = "janet.vim",
  source = "janet-lang/janet.vim",
})

-- }}}

plugins_old = function()
  -- Editing enhancements {{{
  plug("tpope/vim-surround")
  plug("tpope/vim-repeat")

  plug({"mattn/emmet-vim", config = function()
    vim.g.user_emmet_leader_key = '<C-x>e'
  end})

  plug("godlygeek/tabular")

  plug("tpope/vim-rsi")

  -- Electric pairs
  plug({"windwp/nvim-autopairs", config = function()
    -- set up autopairs
    local autopairs = require("nvim-autopairs")
    local conds = require("nvim-autopairs.conds")
    autopairs.setup({
      check_ts = true,
      ignored_next_char = "[^%]%. });`]",
      enable_check_bracket_line = false,
    })
    autopairs.enable()

    -- don't allow single quotes pairing on lisp-types
    local sq_rule = autopairs.get_rules("'")[1]
    sq_rule.not_filetypes = {"scheme", "lisp", "fennel", "janet", "clojure"}
    sq_rule:with_pair(conds.not_after_text("["))

    -- escape codes
    local bs_code = autopairs.esc("<BS>")
    local autopairs_cr = autopairs.autopairs_cr
    local autopairs_bs = autopairs.autopairs_bs

    dummy.imap_enter_handle = function()
      if vim.fn.pumvisible() ~= 0 then
        return (
        " " .. bs_code -- ignore the completion menu
        .. autopairs_cr() -- process autopairs
        )
      else
        return autopairs_cr()
      end
    end

    dummy.imap_bs_handle = function()
      return autopairs_bs()
    end
  end})

  -- plug("tmsvg/pear-tree")
  -- plug("vim-scripts/AutoClose")
  -- plug("jiangmiao/auto-pairs")

  -- Matching pairs
  plug({"luochen1990/rainbow", config = function()
    vim.g.rainbow_active = 1
    vim.g.rainbow_conf = {
      -- parentheses = {
      --     [[start="(" end=")"]],
      --     [[start="[" end="]"]],
      --     [[start="{" end="}"]],
      -- }
      separately = {
        vimwiki = 0,
        acrylic = 0,
        uxntal = 0,
      }
    }
  end})
  -- plug({"andymass/vim-matchup", config = function()
  --     vim.g.matchup_matchparen_offscreen = {method = "popup"}
  -- end})

  -- }}}

  -- Filetypes {{{
  plug("Clavelito/indent-sh.vim")
  plug({"YohananDiamond/zig.vim", config = function()
    -- original: ziglang/zig.vim
    vim.g.zig_fmt_autosave = 0
  end})
  plug("cespare/vim-toml")
  plug("neoclide/jsonc.vim")
  plug("HerringtonDarkholme/yats.vim")
  plug("vala-lang/vala.vim")

  plug({"plasticboy/vim-markdown", config = function()
    -- markdown
    vim.g.vim_markdown_frontmatter = 1
    vim.g.vim_markdown_folding_disabled = 1
    vim.g.vim_markdown_folding_style_pythonic = 0
    vim.g.vim_markdown_override_foldtext = 0
    vim.g.vim_markdown_no_extensions_in_markdown = 1
    vim.g.vim_markdown_new_list_item_indent = 0
    vim.g.vim_markdown_auto_insert_bullets = 0
  end})

  plug("wlangstroth/vim-racket")
  plug("vim-scripts/scribble.vim")
  plug("neovimhaskell/haskell-vim")
  plug("leafo/moonscript-vim")
  plug("rust-lang/rust.vim")
  plug("vim-crystal/vim-crystal")
  plug("justinmk/vim-syntax-extra")
  plug("Vimjas/vim-python-pep8-indent")
  plug("vim-python/python-syntax")
  plug("https://gitlab.com/HiPhish/guile.vim")
  plug("YohananDiamond/fennel.vim") -- fork of bakpakin/fennel.vim
  plug("udalov/kotlin-vim")
  plug("ollykel/v-vim")
  plug("Tetralux/odin.vim")
  plug("YohananDiamond/danmakufu-ph3.vim")
  plug("hellerve/carp-vim")
  plug("habamax/vim-godot")
  plug({"jdonaldson/vaxe", config = function()
    vim.g.vaxe_lime_target = "flash"
  end})
  -- plug("daveyarwood/vim-alda")
  plug("bellinitte/uxntal.vim")
  -- plug("jakwings/vim-terra")
  plug("imsnif/kdl.vim")

  -- plug("tbastos/vim-lua")
  -- plug("hylang/vim-hy")
  -- plug("fsharp/vim-fsharp")
  -- plug("xolox/vim-lua-ftplugin")
  -- plug("teal-language/vim-teal")
  -- plug("JuliaEditorSupport/julia-vim")

  -- nim
  if hasExecutable("nim") and hasExecutable("nimsuggest") then
    plug("YohananDiamond/nvim-nim")
  end
  -- }}}

  plug("junegunn/goyo.vim")

  -- Themes
  if utils.os.is_windows then
    -- use gruvbox as the default theme for windows
    plug {"morhetz/gruvbox", config = function()
      vim.g.gruvbox_bold = 1
      vim.g.gruvbox_italics = 1
    end }
  end

  -- fork of redox-os/ion-vim
  plug("https://gitlab.redox-os.org/YohananDiamond/ion-vim")

  -- fork of skywind3000/vim-auto-popmenu
  plug({
    firstAvailableDir {
      pj_code .. "/vim-auto-popmenu",
      fallback = "YohananDiamond/vim-auto-popmenu",
    },
    config = function()
      vim.g.apc_default_state = 1
      vim.g.apc_map_enter_backspace = 0
      vim.g.apc_custom_states = {
        clap_input = 0, -- prevent conflicts with vim-clap
      }
    end
  })
  utils._features["plugin.vim-auto-popmenu"] = true

  -- acrylic
  -- FIXME: change path lol (probably a $ACR_MAIN_WIKI var)
  local WIKI = os.getenv("WIKI")
  if WIKI then
    vim.g.wiki_dir = WIKI .. "/vimwiki"
  end

  -- plug({"vimwiki/vimwiki", config = function()
    --     -- FIXME: Slowdown candidate

    --     vim.g.wiki_dir = os.getenv("WIKI") .. "/vimwiki"
    --     vim.g.vimwiki_list = {{
      --         path = vim.g.wiki_dir,
      --         path_html = "~/.cache/output/vimwiki_html",
      --         syntax = "default",
      --         ext = ".wiki"
      --     }}
      --     vim.g.vimwiki_map_prefix = "<NOP>"
      --     vim.g.vimwiki_global_ext = 0
      --     vim.g.vimwiki_conceallevel = 0
      --     vim.g.vimwiki_url_maxsave = 0
      -- end})

      -- plug({"nvim-neorg/neorg", config = function()
        --     require('neorg').setup {
          --         load = {
            --             ["core.defaults"] = {}, -- Loads default behaviour
            --             ["core.keybinds"] = {
              --                 config = { default_keybinds = false }
              --             },
              --             -- ["core.concealer"] = { -- Symbol concealing for a tidier view
              --             --     config = {
                --             --         icons = {
                  --             --             todo = { enabled = false },
                  --             --         },
                  --             --     }
                  --             -- },
                  --             ["core.promo"] = {}, -- Semantic indentation
                  --             ["core.export"] = {}, -- export to markdown
                  --         },
                  --     }
                  -- end})

                  plug({
                    firstAvailableDir { pj_code .. "/vim-hydra-fork", fallback = "YohananDiamond/vim-hydra-fork" },
                    config = function()
                      exec("nnoremap <silent> <Leader>f :Hydra extrafind<CR>")
                      vim.fn["hydra#hydras#register"] {
                        name = "extrafind",
                        title = "Extra find",
                        show = "popup",
                        exit_key = "q",
                        feed_key = false,
                        foreign_key = true,
                        single_command = true,
                        position = "s:bottom_right",
                        keymap = {{
                          name = "In buffer",
                          keys = {
                            {"t", "lua dummy.findTodos()", "TODOs (in buffer)"},
                            {"b", "lua require('telescope.builtin').buffers()", "buffers"},
                            {"h", "lua require('telescope.builtin').help_tags()", "help tags"},
                          }
                        }},
                      }
                    end
                  })

                  plug({"nvim-telescope/telescope.nvim"})
                  plug("nvim-lua/popup.nvim")
                  plug("nvim-lua/plenary.nvim")

                  -- Illuminate - delay to highlight words (in millisceconds)
                  -- plug({"RRethy/vim-illuminate", config = function()
                    --     vim.g.Illuminate_delay = 250
                    -- end})

                    -- plug("slakkenhuis/vim-margin")

                    -- plug("airblade/vim-gitgutter")

                    -- Loose config
                    plug {config = function()
                      -- :help php-indent
                      vim.g.PHP_outdentphpescape = 0
                      vim.g.PHP_default_indenting = 0

                      -- rifle
                      vim.g.rifle_mode = (vim.g.is_android == 1) and "buffer" or "popup"
                    end}

                    -- " Clap command: recent files
                    -- " TODO: remove
                    -- let g:clap_provider_recent = {
                      --       \ "source": "filehist list | tac",
                      --       \ "sink": "e",
                      --       \ "description": "Load a file from the recent list",
                      --       \ }
                    end

                    return M
