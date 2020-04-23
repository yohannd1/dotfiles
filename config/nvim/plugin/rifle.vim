if exists("g:loaded_rifle")
  finish
endif

let s:is_windows = exists("g:is_windows")
  \ ? g:is_windows
  \ : isdirectory('C:\')
let s:has_display = $DISPLAY != ""

let s:save_cpo = &cpo " Save user coptions
set cpo&vim " Reset them to their defaults

augroup plug_rifle
  au!
  au BufNewFile,BufRead,BufWrite,BufEnter * call s:RifleInit()
augroup end

function! s:RifleInit()
  let b:rifle_use_termup = exists("b:rifle_use_termup")
    \ ? b:rifle_use_termup
    \ : (!s:is_windows && s:has_display && executable("termup")
    \ && executable("rifle-run"))
endfunction

function! g:Rifle(command)
  if !exists("b:rifle")
    echo "[Rifle] b:rifle dictionary not found."
    return
  endif

  if !has_key(b:rifle, a:command)
    echo printf('[Rifle] key "%s" not found in b:rifle', a:command)
    return
  endif

  " NOTE: Any changes to l:rifle_cmd should not include quote
  " characters, since termopen doesn't accept shell operators if it is
  " an argument list. I might be able to patch it someday, but for now
  " it's not gonna work.
  let l:rifle_cmd = b:rifle[a:command]
  let l:rifle_cmd = substitute(l:rifle_cmd, '%f', expand("%"), "g")

  " Get temp dir and/or file
  if l:rifle_cmd =~ '\v(\%o|\%t)'
    let l:temp = s:GenTemp()
    " if len(l:temp) == 0
    "   echo "[Rifle] could not generate output file."
    "   return
    " endif

    let l:rifle_cmd = substitute(l:rifle_cmd, '%o', l:temp[0]."/".l:temp[1], "g")
    let l:rifle_cmd = substitute(l:rifle_cmd, '%t', l:temp[0], "g")
  endif

  if b:rifle_use_termup
    call jobstart(["rifle-run", l:rifle_cmd])
  else
    split
    wincmd j " TODO: make more customizable directions (here and on termup with bspc and other wm commands)
    enew
    call termopen(l:rifle_cmd) " TODO: turn this into a list
    normal i
  endif
endfunction

function! s:GenTemp()
  let l:temp_dir = expand("~/.cache")
  let l:temp_name = "tmp-output"
  return [l:temp_dir, l:temp_name]
endfunction

command! -nargs=1 Rifle call g:Rifle(eval(<f-args>))

let &cpo = s:save_cpo " Restore user coptions
unlet s:save_cpo " Remove temp var

let g:loaded_rifle = 1

" vim: fdm=marker shiftwidth=2 softtabstop=2
