" Disable ocaml-vim mappins
let no_plugin_maps = 1

" Disable ocp-indent setting the comments option
let no_ocaml_comments = 1

setlocal commentstring=(*\ %s\ *)

setlocal expandtab
setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2

" Formatting

setlocal fo-=c fo-=r fo-=o

" Alter the path so that ignore files stop working
setlocal formatprg=ocamlformat\ --no-disable\ --no-version-check\ --enable-outside-detected-project\ --ignore-invalid-option\ --parse-docstrings\ --name\ %:h:S/_fmt/%:t:S\ -
" This is used instead of &formatprg when formatting an entire buffer
" &formatprg disables the 'disable' option and the version check
let b:full_formatprg = "ocamlformat --enable-outside-detected-project --name % -"

" Merlin

let g:merlin_split_method = "never"

" C-c: Clear merlin highlights
autocmd User ClearHighlight call merlin#StopHighlight()

if !exists('*MerlinLocateMli')
  function! MerlinLocateMli()
    let old_pref = g:merlin_locate_preference
    let g:merlin_locate_preference = 'mli'
    call merlin#Locate()
    let g:merlin_locate_preference = old_pref
  endfunction
endif

nmap <buffer> gD :call MerlinLocateMli()<return>

nmap <buffer> <Leader>e :MerlinErrorCheck<return>

" -

let g:ripple_repls = {
    \ "ocaml": ["dune exec -- ocaml", "", ";;", 0],
    \ }
