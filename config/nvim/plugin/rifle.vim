if exists("g:loaded_rifle")
  finish
endif

augroup plug_rifle
  au!
  au BufNewFile,BufRead,BufWrite,BufEnter * let b:filename = expand("%")
  au BufNewFile,BufRead,BufWrite,BufEnter * let g:_rifle_use_termup =
    \ exists("b:rifle_use_termup")
    \ ? (b:rifle_use_termup != 0)
    \ : (!s:is_windows && executable("termup") && executable("rifle-run") && $DISPLAY != "")
augroup end

let s:is_windows = (exists("g:is_windows") && g:is_windows != 0) || isdirectory('C:\')
let s:save_cpo = &cpo " Save user coptions
set cpo&vim " Reset them to their defaults

function! g:LaunchRifle(key)
  if !exists("b:rifle")
    echo "[Rifle] b:rifle dictionary not found."
    return
  endif
  exec "lua require'rifle'".'.rifle("'.a:key.'",'.g:_rifle_use_termup.','.executable("bspc").')'
endfunction

command! -nargs=1 Rifle call g:LaunchRifle(eval(<f-args>))

let &cpo = s:save_cpo " Restore user coptions
unlet s:save_cpo " Remove temp var

let g:loaded_rifle = 1

" vim: fdm=marker shiftwidth=2 softtabstop=2
