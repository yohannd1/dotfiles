" Thanks to jdhao (Github)
" https://gist.github.com/jdhao/d592ba03a8862628f31cba5144ea04c2

if exists("g:GuiLoaded")
  GuiTabline 0
  GuiPopupmenu 0
  GuiLinespace 2
  silent! GuiFont! Fixedsys:h11

  inoremap <silent> <C-S-v> <C-r>+
endif

" vim: foldmethod=marker foldmarker={{{,}}} shiftwidth=2 softtabstop=2
