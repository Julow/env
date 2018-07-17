# i guard
# Stop initrc execution if not in interactive mode

echo '[[ ! $- = *i* ]] && return'
