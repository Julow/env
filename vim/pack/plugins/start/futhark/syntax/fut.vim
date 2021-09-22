syntax case match

syn match Number "\v<([+-]?(0x[0-9a-fA-F]+|[0-9]+)([ui](8|16|32|64))?)>"

syn match float "\v(([0-9]+\.[0-9]+|[0-9]+f(32|64))(f(32|64))?)"
syn match float "\v([eE][\+\-]?[0-9]+)"

syn keyword conditional if then else
syn keyword Statement loop with entry for while do in local type val
syn keyword keyword concat zip unzip unsafe
syn keyword FutharkBinding let entry nextgroup=FutIdentifier skipwhite skipempty
syn keyword PreProc module open import nextgroup=FutIdentifier skipwhite skipempty
syn keyword FutharkCase match case

syn keyword function map map1 map2 map3 map4 map5 stream_map stream_map_per
syn keyword function reduce reduce_comm scan filter partition
syn keyword function stream_red stream_red_per stream_seq iota
syn keyword function replicate scatter drop
syn keyword function rotate split flatten unflatten
syn keyword function curry uncurry
syn keyword function id const

syn keyword boolean true false

syn match FutIdentifier "[a-zA-Z_][a-zA-Z0-9_']*" skipwhite contained contains=NONE

syn keyword type i8 i16 i32 i64 u8 u16 u32 u64 int real bool char f16 f32 f64

syn keyword typedef type

syn match constant /'.'/

syn match FutharkAssigment "\v\="
syn match FutharkOperator  "\(+\|-\|*\|/\|>\|<\|%\|!\|&\||\|\^\)"
syn match FutharkOperator  "\(++\|==\|!=\|>->\|<-<\||>\|<|\)" containedin=FutharkLambdaOperator

" Literally the same, just with parenthesis
syn match FutharkLambdaOperator "(\ *\(++\|==\|!=\|>->\|<-<\||>\|<|\)\ *)"
syn match FutharkLambdaOperator "(\ *\(+\|-\|*\|/\|>\|<\|%\|!\|&\||\|^\)\ *)"
syn match FutharkLambdaOperator "(\ *\(\.[1-9][0-9]*\)\ *)"
syn match FutharkLambdaOperator /).[1-9][0-9]*/ms=s+1

syn region string start=/"/ skip=/\\"/ end=/"/ keepend excludenl

syn match comment "--.*$"

hi def link FutIdentifier Function

hi def link FutharkBinding Statement
hi def link number constant
hi def link FutharkOperator operator
hi def link FutharkLambdaOperator operator
hi def link FutharkCase Keyword
