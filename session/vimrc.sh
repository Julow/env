# Vim config

cat > "$HOME/.vimrc" <<"EOF"
set encoding=utf8
set ignorecase
set hlsearch
set showmatch

set wildmenu

set mouse=a

syntax enable

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
nnoremap <esc><esc> :noh<return>

" Remap # to highlight the current word and allowing to search forward
nnoremap # *N
EOF
