# Vim config

cat > "$HOME/.vimrc" <<"EOF"
set encoding=utf8
set ignorecase
set hlsearch
set showmatch

set wildmenu

set mouse=a

syntax enable

filetype plugin on

set backspace=eol,start,indent

" Indent
set smarttab
set shiftwidth=4
set tabstop=4
set ai
set si
set noexpandtab

set wrap

" Fold
set foldenable
set foldmethod=indent
set foldlevelstart=99
nnoremap <space> za

" Double escape to clear highlight
" A custom event is used to allow extensibility
nnoremap <esc><esc> :doautocmd User ClearHighlight<return>
autocmd User ClearHighlight call feedkeys(":nohlsearch\<return>")

" Remap # to highlight the current word and allowing to search forward
nnoremap # *N

" Use system clipboard
set clipboard=unnamedplus

" Set window title
set t_ts=]2;
set t_fs=\\
autocmd BufEnter * let &titlestring = expand("%:t")
set title

" OCaml
let s:no_ocaml_maps=1
execute "set rtp+=" . $OPAM_SWITCH_PREFIX . "/share/merlin/vim"
autocmd User ClearHighlight :MerlinClearEnclosing

" Per project vimrc
set exrc
set secure
EOF
