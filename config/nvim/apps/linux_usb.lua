_G.dummy = {}
local vim = _G.vim
local exec = function(cmd) vim.api.nvim_exec(cmd, false) end
local forceLoad = function(path) return assert(loadfile(path))() end

local CONF_DIR = vim.fn.resolve(vim.fn.expand("<sfile>:p:h"))

for _, file in ipairs({"keybindings.lua"}) do
    forceLoad(CONF_DIR .. "/../lua/cfg/" .. file)()
end

exec([[
nnoremap ç :
nnoremap <Alt-h> <<
nnoremap <Alt-h> <<
nnoremap - :e .<CR>

colorscheme retrobox

set hidden
set title
set backspace=indent,eol,start
set laststatus=2
set number relativenumber
set wildmenu
set wildmode=longest:full,full
set autoindent
set hlsearch incsearch
set linebreak wrap
set cursorline " line highlighting
set showcmd
set shortmess+=atcI
set belloff+=ctrlg
set mouse=a
set display+=lastline
set complete=.,w,b,u,k,kspell
set completeopt-=preview
set completeopt+=menuone,noselect
set noshowmode
set list
set nofoldenable
set cinoptions+=g0
set cinoptions+=:0
set scrolloff=3

" That's how the italics work (or not)
let &t_ZH = "\<Esc>[3m"
let &t_ZR = "\<Esc>[23m"

let &autochdir = 1

syntax on

filetype plugin indent on
set foldtext=MyFoldText()

" Indentation
set tabstop=8
set shiftwidth=4 softtabstop=4
set expandtab smarttab

set listchars=tab:\ \ ,trail:¬
]])
