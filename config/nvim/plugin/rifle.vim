if exists("g:loaded_rifle") || g:is_win
  finish
elseif !executable("rifle-run")
  echo "(rifle) could not find `rifle-run` in PATH."
  finish
endif

let s:save_cpo = &cpo " Save user coptions
set cpo&vim " Reset them to their defaults

augroup plug_rifle
  au!
  au BufNewFile,BufRead,BufWrite,BufEnter * call s:RifleInit()
augroup end

function! s:RifleInit()
  let s:has_display = ($DISPLAY != "") || ($WAYLAND_DISPLAY != "")

  if !exists("b:rifle_mode")
    if exists("g:rifle_mode")
      let b:rifle_mode = g:rifle_mode
    else
      let b:rifle_mode = s:has_display ? "popup" : "buffer"
    endif
  endif
endfunction

function! g:Rifle(command)
  if !exists("b:rifle_ft")
    let b:rifle_ft = &filetype
  endif

  let l:command = ["rifle-run", a:command, b:rifle_ft, expand("%:p")]
  if b:rifle_mode == "popup"
    call jobstart(["termup", "runread"] + l:command)
  elseif b:rifle_mode == "buffer"
    split
    wincmd j
    enew
    call termopen(l:command)
    normal i
  elseif b:rifle_mode == "silent"
    call jobstart(l:command)
    echo "(rifle) started background job for" l:command
  else
    echo "(rifle) invalid mode: " . b:rifle_mode
    return
  endif
endfunction

command! -nargs=1 Rifle call g:Rifle(eval(<f-args>))

let &cpo = s:save_cpo " Restore user coptions
unlet s:save_cpo " Remove temp var

let g:loaded_rifle = 1

" vim: fdm=marker shiftwidth=2 softtabstop=2
