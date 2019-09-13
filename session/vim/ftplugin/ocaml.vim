let no_plugin_maps = 1

setlocal commentstring=(*\ %s\ *)

nmap K :call ocpindex#print()<return>

set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2

let g:formatdef_ocamlformat = '"ocamlformat --enable-outside-detected-project --name % -"'
let g:formatters_ocaml = ['ocamlformat']
