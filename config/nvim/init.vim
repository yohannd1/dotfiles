" vim: fdm=marker sw=2 sts=2 foldenable
"
" Setup {{{

lua _G.dummy = {}

" Cancel if this is not being sourced by NeoVim
if !has("nvim")
  echoerr "You are not using NeoVim; this configuration doesn't work properly with Vim."
  finish
endif

" Get config root
let g:config_root = resolve(expand("<sfile>:p:h"))

" bootstrap module system
lua assert(loadfile(vim.g.config_root .. "/lua/prepare.lua"))()

" }}}
" Plugins {{{

lua require("cfg.plugins").init({ plugins = "all", root_path = vim.g.config_root .. "/plugged" })

" }}}
" Functions {{{

function! MyFoldText() " {{{
  let l:foldmarker = split(&foldmarker, ',')
  let l:tab_char = strpart(' ', shiftwidth())
  let l:line_contents = substitute(getline(v:foldstart), '\t', l:tab_char, 'g')
  let l:line_contents = substitute(l:line_contents, ' *'.l:foldmarker[0].'\d* *', '', 'g')

  let l:numbers_width = &foldcolumn + &number * &numberwidth
  let l:window_width = winwidth(0) - numbers_width - 1
  let l:folded_lines_number = v:foldend - v:foldstart

  let l:line_contents = strpart(l:line_contents, 0, l:window_width - 2 - len(l:folded_lines_number))
  let l:void_size = l:window_width - len(l:line_contents) - len(folded_lines_number)
  let l:void_char = '·'

  return l:line_contents . repeat(l:void_char, l:void_size) . l:folded_lines_number . 'l   '
endfunction " }}}
function! PagerMode(...) " {{{
  if len(a:000) >= 1
    let &ft = a:1
  endif
  setlocal ts=8 nomod nolist noma timeoutlen=0 nocursorline norelativenumber
  setlocal noshowcmd
  nnoremap <buffer> <silent> d <C-d>
  nnoremap <buffer> <silent> u <C-u>
  nnoremap <buffer> <silent> f <C-f>
  nnoremap <buffer> <silent> b <C-b>
  nnoremap <buffer> <silent> q :q<CR>
  nnoremap <buffer> <silent> j <C-e>
  nnoremap <buffer> <silent> k <C-y>
  normal M
endfunction " }}}
function! FormatBuffer() " {{{
  if !exists("b:format_command")
    echo "No format command found (should be at b:format_command)."
    return
  endif

  let l:current_buffer_lines = getline(1, "$")
  let l:current_buffer = join(l:current_buffer_lines, "\n")

  let l:infile = tempname()
  call writefile(l:current_buffer_lines, l:infile)

  let l:output = systemlist(b:format_command . "<" . l:infile)

  if v:shell_error != 0
    let g:last_format_err = l:output

    echo "Formatting failed - see buffer for more info"

    if bufname("*Format Errors*") == ""
      split
      wincmd j
      enew
      file *Format Errors*
      setlocal buftype=nofile
      setlocal bufhidden=delete
      setlocal noswapfile
      setlocal nobuflisted
    else
      let l:bufwindow = bufwinid("*Format Errors*")

      if l:bufwindow != -1
        call win_gotoid(l:bufwindow)
      else
        split
        wincmd j
        buffer *Format Errors*
      endif
    endif

    setlocal modifiable
    normal ggdG
    call append("$", "Formatting failed:")
    for line in l:output
      call append("$", "  " . line)
    endfor
    call append("$", "Note - use :ShowFormatErr to show the error again.")
    normal ggdd
    setlocal nomodifiable
  else
    let l:bufwindow = bufwinid("*Format Errors*")

    if l:bufwindow != -1
      call nvim_win_close(l:bufwindow, v:false)
    endif

    if l:output == l:current_buffer_lines
      echo "Buffer already formatted"
    else
      let window_top = line("w0")
      let cline = line(".")
      let ccol = col(".")
      normal ggdG
      call setline(1, l:output)

      exec "normal " . window_top . "Gzt"
      exec "normal " . cline . "G" . ccol "|"
    endif
  endif

  call delete(l:infile)
endfunction " }}}
function! ShowFormatErr() " {{{
  if !exists("g:last_format_err")
    echo "No format errors."
    return
  endif

  for line in g:last_format_err
    echo line
  endfor
endfunction " }}}
function! GetCurrentChar() " {{{
  " TODO: refactor into a char utilities file
  return strcharpart(getline('.')[col('.') - 1:], 0, 1)
endfunction! " }}}
function! GetLineToEnd() " {{{
  return getline('.')[col('.') - 1:]
endfunction! " }}}
function! GetCharAt(line, col) " {{{
  return strcharpart(getline(a:line)[a:col - 1:], 0, 1)
endfunction! " }}}
function! GetURL(string) " {{{
  return matchstr(a:string, '\v(https?|www\.)://[a-zA-Z0-9/\-\.%_?#=&+~:()]+')
endfunction " }}}
function! GetFile(string) " {{{
  return matchstr(a:string, '\v[a-zA-Z0-9_\-\./]+')
endfunction " }}}
function! OpenSelected() " {{{
  let current_word = expand("<cWORD>")

  let url = GetURL(current_word)
  if len(url) != 0
    let browser = $BROWSER

    echo "Found URL: " .. url
    if browser == ""
      echoerr "Could not find a suitable browser to open (set it via $BROWSER)"
    endif

    if confirm("Open using a browser?", "&Yes\n&No") == 1
      call jobstart([browser, url])
    endif

    return
  endif

  let file = GetFile(current_word)
  if len(file) != 0
    echo "Found file: " .. file

    if confirm("Open in a new buffer", "&Yes\n&No") == 1
      exec 'edit ' .. file
    endif

    return
  endif

  echo "Could not find a suitable file or url in the current WORD"
endfunction " }}}
function! WikiGenTitle() " {{{
  return strftime("%Y%m%d%H%M-") .. trim(system("hexdump -n 3 -e '4/4 \"%08X\" 1 \"\\n\"' /dev/random | cut -c 3-"))
endfunction " }}}
function! SourceIf(...) " {{{
  for path in a:000
    if filereadable(path)
      exec "source ".path
    endif
  endfor
endfunction
" }}}

" }}}
" Commands {{{

command! -nargs=* PagerMode call PagerMode(<f-args>)
command! -nargs=0 FormatBuffer call FormatBuffer()
command! -nargs=0 ShowFormatErr call ShowFormatErr()

" }}}
" Settings {{{

colorscheme base16

filetype plugin indent on
set foldtext=MyFoldText()

" TO-DO Highlighting (and more)
" Partially stolen from https://github.com/sakshamgupta05/vim-todo-highlight
if v:true
  let g:annotations_to_highlight = ['TODO', 'FIXME', 'XXX', 'NOTE']

  " creates annotation group and highlight it according to the config
  for name in g:annotations_to_highlight
    let group_name = 'annotation_' . tolower(name)

    " make group for annotation where its pattern matches and is inside comment
    " TODO: make work with strings too
    execute 'augroup ' . group_name
    autocmd!
    execute 'autocmd Syntax * syntax match ' . group_name .
          \ ' /\v\_.<' . name . '>:?/hs=s+1 contained containedin=.*Comment.*'
    execute 'augroup end'

    " highlight the group according to the config
    execute 'hi link ' . group_name . ' Todo'
  endfor
endif

" }}}
" Experimental Stuff {{{

command! Fgitmerge /\v^(\<{4,}|\={4,}|\>{4,})
command! FoldNone set nofoldenable
command! FoldBracket set foldenable foldmethod=marker foldmarker={,}

" }}}
" Lua Tunnel {{{

lua require("cfg.initrc")

" }}}
