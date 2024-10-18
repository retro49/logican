if exists("b:current_syntax")
    finish
endif

syn match lgIdentifier  display "\v<_*[a-z][A-Za-z0-9_]*>"
syn match lgFunction    display "\zs\(\w*\)*\s*\ze("
syn match lgNumber      display "\v<[0-9](_*[0-9])*([iu](8|16|32|64|128)|([Uu][Ll]?|[Ll]))?>"
syn match lgFloat       display "\v<0[Xx][0-9A-Fa-f](_*[0-9A-Fa-f])*\.[0-9A-Fa-f](_*[0-9A-Fa-f])*([Pp][+-]?[0-9]+)?(f(8|16|32|64|128))?>"
syn match lgString      display "\v\"(\\.|[^\\\"])*\""
syn match lgString      display "\v'(''|.)*'"
syn region lgComment    display start="\v//"    end="\v$"
syn keyword lgBuiltinType true false

hi def link lgIdentifier        Identififer
hi def link lgFunction          Function
hi def link lgNumber            Number
hi def link lgFloat             Float
hi def link lgString            String
hi def link lgComment           Comment
hi def link lgBuiltinType       Type

syn keyword lgKeyword
    \ function
    \ constant
    \ statement
    \ proof
    \ print
    \ theorem
    \ is
    \ let

hi def link lgKeyword      Keyword
let b:current_syntax = 'lg'
