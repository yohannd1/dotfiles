local M = {}

local function plug(url)
    vim.cmd(string.format("Plug '%s'", url))
end

function M.loadPlugins()
    vim.cmd("silent! call plug#begin(g:config_root . '/plugged')")

    -- Editing enhancements
    plug("tpope/vim-surround")
    plug("tpope/vim-repeat")
    plug("tpope/vim-commentary")
    plug("mattn/emmet-vim")
    plug("godlygeek/tabular")
    plug("ap/vim-buftabline")
    plug("tpope/vim-rsi")

    -- Electric pairs
    plug("windwp/nvim-autopairs")
    -- plug("tmsvg/pear-tree")
    -- plug("vim-scripts/AutoClose")
    -- plug("jiangmiao/auto-pairs")

    -- Matching pairs
    plug("luochen1990/rainbow")
    -- plug("andymass/vim-matchup")

    -- Treesitter
    plug("nvim-treesitter/nvim-treesitter")

    -- Filetypes
    plug("Clavelito/indent-sh.vim")
    plug("YohananDiamond/zig.vim") -- ziglang/zig.vim
    plug("cespare/vim-toml")
    plug("neoclide/jsonc.vim")
    plug("HerringtonDarkholme/yats.vim")
    plug("plasticboy/vim-markdown")
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

    -- Filetypes - nim
    if vim.fn.executable("nim") and vim.fn.executable("nimsuggest") then
        plug("YohananDiamond/nvim-nim")
    end

    plug("junegunn/goyo.vim")

    -- Themes
    if vim.g.is_win == 1 then
        plug("morhetz/gruvbox") -- for windows
    end
    -- plug("dracula/vim")
    -- plug("chriskempson/base16-vim")

    -- fork of redox-os/ion-vim
    plug("https://gitlab.redox-os.org/YohananDiamond/ion-vim")

    -- fork of skywind3000/vim-auto-popmenu
    plug("YohananDiamond/vim-auto-popmenu")

    -- Misc.
    plug("tpope/vim-vinegar")
    plug("vimwiki/vimwiki") -- NOTE: Slowdown candidate
    plug("nvim-neorg/neorg")
    -- plug("itchyny/lightline.vim")

    plug("YohananDiamond/vim-hydra")
    plug("nvim-telescope/telescope.nvim")
    plug("nvim-lua/popup.nvim")
    plug("nvim-lua/plenary.nvim")
    -- plug("nvim-lua/completion-nvim")
    -- plug("RRethy/vim-illuminate")
    -- plug("slakkenhuis/vim-margin")

    plug("airblade/vim-gitgutter")

    vim.cmd("call plug#end()")
end

function M.configurePlugins()
    -- rainbow
    vim.g.rainbow_active = 1
    vim.g.rainbow_conf = {
        separately = {
            vimwiki = 0,
            uxntal = 0,
        }
    }

    -- emmet
    vim.g.user_emmet_leader_key = '<C-x>e'

    -- markdown
    vim.g.vim_markdown_frontmatter = 1
    vim.g.vim_markdown_folding_disabled = 1
    vim.g.vim_markdown_folding_style_pythonic = 0
    vim.g.vim_markdown_override_foldtext = 0
    vim.g.vim_markdown_no_extensions_in_markdown = 1
    vim.g.vim_markdown_new_list_item_indent = 0
    vim.g.vim_markdown_auto_insert_bullets = 0

    -- gruvbox
    vim.g.gruvbox_bold = 1
    vim.g.gruvbox_italics = 1

    -- Buftabline
    vim.g.buftabline_indicators = 1

    vim.g.rifle_mode = (vim.g.is_android == 1) and "buffer" or "popup"

    -- " vim-auto-popmenu x Clap
    -- let g:apc_default_state = 1
    -- let g:apc_map_enter_backspace = 0
    -- let g:apc_custom_states = {
    --       \ "clap_input": 0,
    --       \ }

    -- " Clap command: recent files
    -- " TODO: remove
    -- let g:clap_provider_recent = {
    --       \ "source": "filehist list | tac",
    --       \ "sink": "e",
    --       \ "description": "Load a file from the recent list",
    --       \ }

    -- zig.vim
    vim.g.zig_fmt_autosave = 0

    -- vimwiki
    vim.g.wiki_dir = os.getenv("WIKI") .. "/vimwiki"
    vim.g.vimwiki_list = {{
        path = vim.g.wiki_dir,
        path_html = "~/.cache/output/vimwiki_html",
        syntax = "default",
        ext = ".wiki"
    }}
    vim.g.vimwiki_map_prefix = "<NOP>"
    vim.g.vimwiki_global_ext = 0
    vim.g.vimwiki_conceallevel = 0
    vim.g.vimwiki_url_maxsave = 0

    -- Illuminate - delay to highlight words (in millisceconds)
    vim.g.Illuminate_delay = 250

    -- matchup
    vim.g.matchup_matchparen_offscreen = {method = "popup"}

    -- :help php-indent
    vim.g.PHP_outdentphpescape = 0
    vim.g.PHP_default_indenting = 0

    -- neorg
    require('neorg').setup {
        load = {
            ["core.defaults"] = {}, -- Loads default behaviour
            ["core.concealer"] = {}, -- Adds pretty icons to your documents
        },
    }

    -- treesitter
    require('nvim-treesitter.configs').setup {
        -- A list of parser names, or "all" (the five listed parsers should always be installed)
        ensure_installed = { "c", "lua", "vim", "vimdoc", "query" },

        -- Install parsers synchronously (only applied to `ensure_installed`)
        sync_install = false,

        -- Automatically install missing parsers when entering buffer
        -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
        auto_install = true,

        -- List of parsers to ignore installing (for "all")
        ignore_install = { "javascript" },

        ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
        -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

        highlight = {
            enable = true,

            -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
            -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
            -- the name of the parser)
            -- list of language that will be disabled
            disable = { "c", "rust" },
            -- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
            disable = function(lang, buf)
                local max_filesize = 100 * 1024 -- 100 KB
                local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
                if ok and stats and stats.size > max_filesize then
                    return true
                end
            end,

            -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
            -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
            -- Using this option may slow down your editor, and you may see some duplicate highlights.
            -- Instead of true it can also be a list of languages
            additional_vim_regex_highlighting = false,
        },
    }
end

return M
