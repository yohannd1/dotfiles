-- vim: fdm=marker foldenable foldmarker={{{,}}}

local vim = _G.vim
local _configs = {}
local M = {}

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
        if vim.fn.isdirectory(dir) then
            return dir
        end
    end

    if arg.fallback then
        return arg.fallback
    else
        error("Could not find plugin + no fallback specified")
    end
end

local plugins = function()
    local HOME = assert(os.getenv("HOME"), "could not get home directory")
    local pj_code = HOME .. "/pj/code"

    -- Editing enhancements {{{
    plug("tpope/vim-surround")
    plug("tpope/vim-repeat")
    plug("tpope/vim-commentary")

    plug({"mattn/emmet-vim", config = function()
        vim.g.user_emmet_leader_key = '<C-x>e'
    end})

    plug("godlygeek/tabular")

    plug({"ap/vim-buftabline", config = function()
        vim.g.buftabline_indicators = 1
    end})

    plug("tpope/vim-rsi")

    -- Electric pairs
    plug("windwp/nvim-autopairs")
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

    -- Treesitter
    plug({"nvim-treesitter/nvim-treesitter", config = function()
        require('nvim-treesitter.configs').setup {
            ensure_installed = { "c", "lua", "vim", "vimdoc", "query" },

            -- Install parsers synchronously (only applied to `ensure_installed`)
            sync_install = false,

            -- Automatically install missing parsers when entering buffer
            auto_install = true,

            ignore_install = {},

            highlight = {
                enable = true,
                disable = { "gitcommit", "bash" },
                additional_vim_regex_highlighting = false,
            },
        }
    end})

    -- Filetypes {{{
    plug("Clavelito/indent-sh.vim")
    plug({"YohananDiamond/zig.vim", config = function()
        -- original: ziglang/zig.vim
        vim.g.zig_fmt_autosave = 0
    end})
    plug("cespare/vim-toml")
    plug("neoclide/jsonc.vim")
    plug("HerringtonDarkholme/yats.vim")

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
    plug("janet-lang/janet.vim")
    plug("jdonaldson/vaxe")
    plug("daveyarwood/vim-alda")
    plug("bellinitte/uxntal.vim")

    -- plug("tbastos/vim-lua")
    -- plug("hylang/vim-hy")
    -- plug("fsharp/vim-fsharp")
    -- plug("xolox/vim-lua-ftplugin")
    -- plug("teal-language/vim-teal")
    -- plug("JuliaEditorSupport/julia-vim")

    -- nim
    if vim.fn.executable("nim") and vim.fn.executable("nimsuggest") then
        plug("YohananDiamond/nvim-nim")
    end
    -- }}}

    plug("junegunn/goyo.vim")

    -- Themes
    if vim.g.is_win > 0 then
        -- use gruvbox as the default theme for windows
        plug {"morhetz/gruvbox", config = function()
            vim.g.gruvbox_bold = 1
            vim.g.gruvbox_italics = 1
        end}
    end

    -- fork of redox-os/ion-vim
    plug("https://gitlab.redox-os.org/YohananDiamond/ion-vim")

    -- fork of skywind3000/vim-auto-popmenu
    plug({"YohananDiamond/vim-auto-popmenu", config = function()
        vim.g.apc_default_state = 1
        vim.g.apc_map_enter_backspace = 0
        vim.g.apc_custom_states = {
            clap_input = 0, -- prevent conflicts with vim-clap
        }
    end})

    -- Misc.
    plug("tpope/vim-vinegar")

    -- acrylic
    -- FIXME: change path lol
    vim.g.wiki_dir = os.getenv("WIKI") .. "/vimwiki"

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

    -- Acrylic
    plug(firstAvailableDir {
        pj_code .. "/acrylic.vim",
        fallback = "YohananDiamond/acrylic.vim",
    })

    plug(firstAvailableDir {
        pj_code .. "/vim-hydra-fork",
        fallback = "YohananDiamond/vim-hydra-fork",
    })

    plug("nvim-telescope/telescope.nvim")
    plug("nvim-lua/popup.nvim")
    plug("nvim-lua/plenary.nvim")

    -- Illuminate - delay to highlight words (in millisceconds)
    -- plug({"RRethy/vim-illuminate", config = function()
    --     vim.g.Illuminate_delay = 250
    -- end})

    -- plug("slakkenhuis/vim-margin")

    plug("airblade/vim-gitgutter")

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

function M.load()
    local is_first = vim.g.is_first > 0

    if is_first then
        vim.fn["plug#begin"](vim.g.config_root .. "/plugged")
        plugins()
        vim.fn["plug#end"]()
    end

    for _, f in ipairs(_configs) do f() end
end

return M
