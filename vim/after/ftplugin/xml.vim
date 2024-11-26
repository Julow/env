setlocal formatprg=xmllint\ --format\ --recover\ -
setlocal foldmethod=syntax

" Closing a tag, using the omni completion provided by the builtin plugin
inoremap <C-/> </<C-x><C-o>
imap <C-S-/> <C-/>
