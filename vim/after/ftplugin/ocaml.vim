let no_plugin_maps = 1

setlocal commentstring=(*\ %s\ *)

setlocal expandtab
setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2

setlocal fo-=c fo-=r fo-=o

setlocal formatprg=ocamlformat\ --enable-outside-detected-project\ --no-version-check\ --name\ %\ -

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

let g:ripple_repls = {
    \ "ocaml": ["dune exec -- ocaml", "", ";;", 0],
    \ }
