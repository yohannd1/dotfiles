-- vim: fdm=marker foldenable foldmarker={{{,}}}

-- Preparations {{{
local vim = _G.vim
local dummy = _G.dummy
local M = {}

local utils = require("cfg.utils")
local services = utils.services
local exec = utils.exec

local firstAvailableDir = function(arg)
  for _, dir in ipairs(arg) do
    if vim.fn.isdirectory(dir) ~= 0 then
      return dir
    end
  end

  return arg.fallback or error("Could not find plugin + no fallback specified")
end

do
  local plugins = {}
  local plugin_names = {}

  M.add = function(arg)
    local opts
    if type(arg) == "string" then opts = { source = arg }
    elseif type(arg) == "table" then opts = arg
    else error("Expected string/table, got " .. vim.inspect(arg))
    end

    local source = assert(opts.source, ".source entry missing")
    local name = opts.name or vim.fs.basename(source)
    local before = opts.before or function() end
    local after = opts.after or function() end

    local condition = (function(x)
      if x == nil then return true end
      return x
    end)(opts.condition)

    plugins[name] = { name = name, source = source, condition = condition, before = before, after = after }
    table.insert(plugin_names, name)
  end

  M.init = function(opts)
    local plugins_to_load = assert(opts.plugins, ".plugins not specified")
    plugins_to_load = (plugins_to_load == "all") and plugin_names or plugins_to_load

    local root_path = assert(opts.root_path, ".root_path not specified")

    local to_call = {}

    vim.fn["plug#begin"](root_path)
    local plug = vim.fn["plug#"]

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

    vim.fn["plug#end"]()

    for _, f in ipairs(to_call) do f() end

    M.afterPluginLoad()
  end
end

-- }}}

local HOME = assert(os.getenv("HOME"), "could not get home directory")
local pj_code = HOME .. "/pj/code"

local UNUSED_PLUGIN_COND = false

-- Treesitter {{{
M.add({
  name = "nvim-treesitter",
  source = "nvim-treesitter/nvim-treesitter",
  condition = false,
  after = function()
    require("nvim-treesitter.configs").setup {
      ensure_installed = {},

      -- Install parsers synchronously (only applied to `ensure_installed`)
      sync_install = false,

      -- Automatically install missing parsers when entering buffer
      auto_install = false,

      ignore_install = {},

      highlight = {
        enable = { "nim" },
        disable = { "gitcommit", "bash", "PKGBUILD", "latex", "janet" },
        additional_vim_regex_highlighting = true,
      },
    }
  end,
})
-- }}}
-- Telescope {{{
M.add({
  source = "nvim-telescope/telescope.nvim",
  after = function()
    local telescope = require("telescope")
    local conf = require("telescope.config").values
    local finders = require("telescope.finders")
    local pickers = require("telescope.pickers")
    local themes = require("telescope.themes")
    local action_state = require("telescope.actions.state")
    local actions = require("telescope.actions")

    telescope.setup {
      defaults = {
        layout_strategy = "bottom_pane",
        layout_config = {
          bottom_pane = {
            prompt_position = "bottom",
          },
        },
        mappings = {
          i = {
            ["<Esc>"] = actions.close,
            ["<C-j>"] = actions.move_selection_next,
            ["<C-k>"] = actions.move_selection_previous,
            ["<C-m>"] = actions.select_default,

            ["<ScrollWheelUp>"] = actions.move_selection_previous,
            ["<ScrollWheelDown>"] = actions.move_selection_next,
          },
          n = {}
        },
        scroll_strategy = "cycle",
      },
      pickers = {
        buffers = {
          sort_lastused = true,
          theme = "dropdown",
          previewer = false,
        },
        find_files = {
          theme = "dropdown"
        }
      },
    }

    local main_theme = themes.get_ivy()
    utils.services.fuzzyPicker = function(opts)
      local prompt = assert(opts.prompt, "Missing prompt title")
      local source = assert(opts.source, "Missing source")
      local on_choice = opts.on_choice or function() end

      local tel_opts = {}
      utils.overrideTableWith(tel_opts, main_theme)

      local finder
      if source.command then
        finder = finders.new_oneshot_job(source.command, opts)
      else
        error("Unknown source type...")
      end

      pickers.new(tel_opts, {
        prompt_title = prompt,
        finder = finder,
        sorter = conf.generic_sorter(tel_opts),
        attach_mappings = function()
          actions.select_default:replace(function(prompt_bufnr)
            local selection = action_state.get_selected_entry()
            actions.close(prompt_bufnr)
            if selection then
              on_choice(selection[1])
            end
          end)

          return true
        end
      }):find()
    end
  end
})
-- }}}
-- Utils {{{
M.add({
  source = "ap/vim-buftabline",
  before = function()
    vim.g.buftabline_indicators = 1
  end,
})

M.add({
  name = "vim-commentary",
  source = "tpope/vim-commentary",
})

M.add({
  name = "goyo.vim",
  source = "junegunn/goyo.vim",
  before = function()
    vim.g.goyo_width = 120
  end,
})

-- netrw improvement
-- M.add({
--   name = "vim-vinegar",
--   source = "tpope/vim-vinegar",
-- })

M.add({
  name = "nerdtree",
  source = "preservim/nerdtree",
  after = function()
    vim.g.NERDTreeMinimalUI = true
    vim.g.NERDTreeMinimalMenu = true
    vim.g.NERDTreeQuitOnOpen = true
  end,
})

M.add("nvim-lua/popup.nvim")
M.add("nvim-lua/plenary.nvim")
-- M.add("slakkenhuis/vim-margin")
-- M.add("airblade/vim-gitgutter")
-- }}}
-- Editing enhancements {{{
M.add("tpope/vim-surround")

M.add("tpope/vim-repeat")

M.add({
  source = "https://github.com/windwp/nvim-autopairs",
  after = function()
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

    local mapKey = vim.api.nvim_set_keymap

    mapKey("i", "<CR>", "v:lua.dummy.imap_enter_handle()", {expr = true, noremap = true})
    mapKey("i", "<BS>", "v:lua.dummy.imap_bs_handle()", {expr = true, noremap = true})

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
  end
})
-- M.add("tmsvg/pear-tree")
-- M.add("vim-scripts/AutoClose")
-- M.add("jiangmiao/auto-pairs")

M.add({
  source = "mattn/emmet-vim",
  before = function()
    vim.g.user_emmet_leader_key = '<C-x>e'
  end,
})

M.add("godlygeek/tabular")
M.add("tpope/vim-rsi")

-- Matching/rainbow pairs
M.add({
  source = "luochen1990/rainbow",
  before = function()
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
        tex = 0,
        latex = 0,
      }
    }
  end,
})
-- M.add({source = "andymass/vim-matchup", before = function()
--     vim.g.matchup_matchparen_offscreen = {method = "popup"}
-- end})

-- fork of skywind3000/vim-auto-popmenu
M.add({
  source = firstAvailableDir {
    pj_code .. "/vim-auto-popmenu",
    fallback = "yohannd1/vim-auto-popmenu",
  },
  before = function()
    vim.g.apc_default_state = 1
    vim.g.apc_map_enter_backspace = 0
    vim.g.apc_custom_states = {
      clap_input = 0, -- prevent conflicts with vim-clap
    }
    utils._features["plugin.vim-auto-popmenu"] = true
  end
})

-- }}}
-- Themes {{{
if utils.os.is_windows then
  M.add({
    source = "morhetz/gruvbox",
    after = function()
      vim.g.gruvbox_bold = 1
      vim.g.gruvbox_italics = 1
      exec("colorscheme gruvbox")
    end,
  })
end
-- }}}
-- Filetype plugins {{{
M.add({
  name = "acrylic.vim",
  source = firstAvailableDir({
    pj_code .. "/acrylic.vim",
    fallback = "yohannd1/acrylic.vim"
  }),
})

M.add({
  source = "yohannd1/zig.vim",
  before = function()
    vim.g.zig_fmt_autosave = 0
  end
})

M.add({
  source = "plasticboy/vim-markdown",
  config = function()
    vim.g.vim_markdown_frontmatter = 1
    vim.g.vim_markdown_folding_disabled = 1
    vim.g.vim_markdown_folding_style_pythonic = 0
    vim.g.vim_markdown_override_foldtext = 0
    vim.g.vim_markdown_no_extensions_in_markdown = 1
    vim.g.vim_markdown_new_list_item_indent = 0
    vim.g.vim_markdown_auto_insert_bullets = 0
  end,
})

M.add({
  source = "jdonaldson/vaxe",
  before = function()
    vim.g.vaxe_lime_target = "flash"
  end
})

-- M.add("alaviss/nim.nvim")
M.add("zah/nim.vim")

-- M.add({
--   source = "yohannd1/nvim-nim",
--   condition = hasExecutable("nim") and hasExecutable("nimsuggest"),
-- })

M.add("janet-lang/janet.vim")
M.add("lervag/vimtex")
M.add("cespare/vim-toml")
M.add("neoclide/jsonc.vim")
M.add("HerringtonDarkholme/yats.vim")
M.add("vala-lang/vala.vim")
M.add("Clavelito/indent-sh.vim")
M.add("neovimhaskell/haskell-vim")
M.add("leafo/moonscript-vim")
M.add("rust-lang/rust.vim")
M.add("vim-crystal/vim-crystal")
M.add("justinmk/vim-syntax-extra")
M.add("Vimjas/vim-python-pep8-indent")
M.add("vim-python/python-syntax")
M.add("https://gitlab.com/HiPhish/guile.vim")
M.add("yohannd1/fennel.vim") -- fork of bakpakin/fennel.vim
M.add("udalov/kotlin-vim")
M.add("ollykel/v-vim")
M.add("yohannd1/danmakufu-ph3.vim")
M.add("habamax/vim-godot")
M.add("bellinitte/uxntal.vim")

-- M.add("https://gitlab.redox-os.org/yohannd1/ion-vim") -- fork of redox-os/ion-vim
-- M.add("wlangstroth/vim-racket")
-- M.add("vim-scripts/scribble.vim")
-- M.add("Tetralux/odin.vim")
-- M.add("hellerve/carp-vim")
-- M.add("imsnif/kdl.vim")
-- M.add("daveyarwood/vim-alda")
-- M.add("jakwings/vim-terra")
-- M.add("tbastos/vim-lua")
-- M.add("hylang/vim-hy")
-- M.add("fsharp/vim-fsharp")
-- M.add("xolox/vim-lua-ftplugin")
-- M.add("teal-language/vim-teal")
-- M.add("JuliaEditorSupport/julia-vim")
-- }}}
-- Hydra {{{
M.add({
  source = firstAvailableDir { pj_code .. "/vim-hydra-fork", fallback = "yohannd1/vim-hydra-fork" },
  after = function()
    services.defKeyMenu = function(opts)
      local id = assert(opts.id, "Missing id")
      local title = assert(opts.title, "Missing title")
      local keymaps = assert(opts.keymaps, "Missing keymaps")

      vim.fn["hydra#hydras#register"]({
        name = id,
        title = title,
        show = "popup",
        exit_key = "q",
        feed_key = false,
        foreign_key = true,
        single_command = true,
        position = "s:bottom_right",
        keymap = keymaps,
      })
    end

    services.loadKeyMenu = vim.cmd.Hydra
  end
})
-- }}}
-- Unused {{{
M.add({
  source = "vimwiki/vimwiki",
  condition = UNUSED_PLUGIN_COND,
  before = function()
    vim.g.vimwiki_list = {{
        path = vim.g.acr_wiki_dir,
        path_html = "~/.cache/output/vimwiki_html",
        syntax = "default",
        ext = ".wiki"
    }}
    vim.g.vimwiki_map_prefix = "<NOP>"
    vim.g.vimwiki_global_ext = 0
    vim.g.vimwiki_conceallevel = 0
    vim.g.vimwiki_url_maxsave = 0
  end,
})

M.add({
  source = "nvim-neorg/neorg",
  condition = UNUSED_PLUGIN_COND,
  after = function()
    require('neorg').setup {
      load = {
        ["core.defaults"] = {}, -- Loads default behaviour
        ["core.keybinds"] = {
          config = { default_keybinds = false }
        },
        ["core.promo"] = {}, -- Semantic indentation
        ["core.export"] = {}, -- export to markdown
      },
    }
  end
})

-- Illuminate - delay to highlight words (in millisceconds)
M.add({
  source = "RRethy/vim-illuminate",
  condition = UNUSED_PLUGIN_COND,
  before = function()
    vim.g.Illuminate_delay = 250
  end
})
-- }}}

M.afterPluginLoad = function()
  -- :help php-indent
  vim.g.PHP_outdentphpescape = 0
  vim.g.PHP_default_indenting = 0

  -- rifle
  vim.g.rifle_mode = (utils.os.is_android == 1) and "buffer" or "popup"

  -- acrylic related stuff
  vim.g.acr_wiki_dir = assert(os.getenv("ACR_WIKI_DIR"), "no $ACR_WIKI_DIR specified")

  local wikiInsertRef = function(ref, opts)
    local text = "@ref(" .. ref .. ")"
    utils.addTextInLine(text, opts)
  end

  dummy.wikiFzOpen = function()
    services.fuzzyPicker({
      prompt = "Search on wiki",
      source = { command = {"acr-list-titles"} },
      on_choice = function(choice)
        local name = vim.fn.split(choice)[1]
        vim.cmd.edit(("%s/%s.acr"):format(vim.g.acr_wiki_dir, name))
      end
    })
  end

  dummy.wikiGenNewFilename = function(opts)
    local attempt_limit = opts.attempt_limit or 32
    local base_path = opts.base_path or ""
    for _ = 1, attempt_limit do
      local time = vim.fn.strftime("%Y%m%d%H%M")
      local suffix = utils.randomHexString(6)
      local name = string.format("%s-%s", time, suffix)
      local path = string.format("%s/%s.acr", base_path, name)
      if vim.fn.filereadable(path) == 0 then
        return { name = name, path = path }
      end
    end
    error(("Too many attempts (%d) while trying to generate filename"):format(attempt_limit))
  end

  dummy.wikiNewFileInsertRef = function(opts)
    local r = dummy.wikiGenNewFilename({ base_path = vim.g.acr_wiki_dir })
    wikiInsertRef(r.name, {
      after_cursor = opts.after_cursor,
      telescope_fix = false,
    })
    vim.cmd.edit(r.path)
  end

  dummy.wikiFzInsertRef = function(opts)
    local repr_string = opts.after_cursor and "after" or "before"
    services.fuzzyPicker({
      prompt = "Insert wiki file: " .. repr_string,
      source = { command = {"acr-list-titles"} },
      on_choice = function(choice)
        local name = vim.fn.split(choice)[1]
        wikiInsertRef(name, {
          after_cursor = opts.after_cursor,
          telescope_fix = true,
        })
      end
    })
  end

  dummy.menuOpenRecent = function()
    services.fuzzyPicker({
      prompt = "Open recent file",
      source = { command = {"filehist", "list"} },
      on_choice = function(choice)
        vim.cmd.edit(choice)
      end
    })
  end
end

return M
