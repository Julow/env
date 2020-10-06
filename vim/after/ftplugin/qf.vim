" Settings for the quickfix window

augroup QuickfixWindow
  autocmd! * <buffer>
  " Automatically resize when it is focused/unfocused
  autocmd BufEnter <buffer> resize 11
  autocmd BufLeave <buffer> resize 1
augroup END

" Improve status line
setlocal noruler
setlocal statusline=%{exists('w:quickfix_title')?w:quickfix_title:''}%= " Set by some commands
setlocal statusline+=%=%t " Right part
setlocal statusline+=\ %l/%L%(\ (%{S_selection()})%) " Cursor position and selection

setlocal number

" 'q' to close the window
nnoremap q <C-w>q