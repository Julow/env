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

nmap <buffer> <Leader>e :MerlinErrorCheck<return>
nmap <buffer> gD :MerlinILocate<return>

let g:ripple_repls = {
    \ "ocaml": ["dune exec -- ocaml", "", ";;", 0],
    \ }
