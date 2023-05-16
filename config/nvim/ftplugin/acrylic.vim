setlocal autoindent
setlocal nosmartindent
setlocal nocindent

setlocal commentstring=\%%\ %s

" Keybindings
nnoremap <buffer> <C-m> :call acrylic#open_cword()<CR>
nnoremap <buffer> <silent> <Tab> :call acrylic#find_ref("")<CR>
nnoremap <buffer> <silent> <S-Tab> :call acrylic#find_ref("b")<CR>

" TODO: get indent size from header
