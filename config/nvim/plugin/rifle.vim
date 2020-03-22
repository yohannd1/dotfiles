if exists("g:loaded_rifle")
    finish
endif

let s:is_windows = (exists("g:is_windows") && g:is_windows == 1) || isdirectory('C:\')
let s:save_cpo = &cpo " Save user coptions
set cpo&vim " Reset them to their defaults

augroup plug_rifle
    au!
    au BufNewFile,BufRead,BufWrite,BufEnter * let b:filename = expand("%")
augroup end

function! g:LaunchRifle(key)
    if !exists("b:rifle")
        echo "[Rifle] b:rifle dictionary not found."
        return
    endif
    exec "lua require'rifle'.rifle(\"".a:key."\")"
endfunction

command! Rifle call g:LaunchRifle(eval(<f-args>))

let &cpo = s:save_cpo " Restore user coptions
unlet s:save_cpo " Remove temp var

let g:loaded_rifle = 1
