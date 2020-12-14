setlocal indentexpr=FutharkIndent()

function! FutharkIndent()
  let line = getline(v:lnum)
  let prevNum = prevnonblank(v:lnum)
  let prev = getline(prevNum)

  " Indent extra if matching these patterns
  if prev =~ "=$" || prev =~ "->$" || prev =~ "do$"
    return indent(prevNum) + &shiftwidth

  " Else keep same level of indentation
  else
    return indent(prevNum)

  endif
endfunction
