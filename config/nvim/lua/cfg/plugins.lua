local function plug(url)
    vim.cmd(string.format("Plug '%s'", url))
end

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
