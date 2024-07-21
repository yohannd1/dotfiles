" vim: fdm=marker sw=2 sts=2 foldenable
"
" Cancel if this is not being sourced by NeoVim
if !has("nvim")
  echoerr "You are not using NeoVim; this configuration doesn't work properly with Vim."
  finish
endif

" initialize lua 'dummy' table
lua _G.dummy = {}

" Get config root
let g:config_root = resolve(expand("<sfile>:p:h"))

" bootstrap module system
lua assert(loadfile(vim.g.config_root .. "/lua/prepare.lua"))()

" load plugin config
lua require("cfg.plugins").init({ plugins = "all", root_path = vim.g.config_root .. "/plugged" })

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

command! -nargs=0 FormatBuffer call FormatBuffer()
command! -nargs=0 ShowFormatErr call ShowFormatErr()
command! Fgitmerge /\v^(\<{4,}|\={4,}|\>{4,})

" load the rest of the config
lua require("cfg.general")
lua require("cfg.statusline")
lua require("cfg.keybindings")
lua require("cfg.filetypes")
