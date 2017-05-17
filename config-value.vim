" Config-value syntax file
" Language:     config-value
" Author:       Eric Mertens

if exists("b:current_syntax")
  finish
endif

" Reserved symbols
syn match cvDelimiter  "*\|:\|\[\|\]\|,\|{\|}\|="

" Strings and constants -- copied from haskell.vim
syn match   cvStringGap         contained "\\[\n\ \t]*\\"
syn match   cvSpecialChar       contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|\^[A-Z^_\[\\\]]\)"
syn match   cvSpecialChar       contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   cvSpecialCharError  contained "\\&\|'''\+"
syn region  cvString            start=+"+  skip=+\\\\\|\\"+  end=+"\|\n+ contains=cvStringGap,cvSpecialChar
syn match   cvNumber            "-\=\([0-9]\+\|0[xX][0-9a-fA-F]\+\|0[oO][0-7]\+\|0[bB][0-1]\+\)\>"
syn match   cvFloat             "-\=[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
syn match   cvFloat             "-\=[0-9]\+[eE][-+]\=[0-9]\+\>"

syn match   cvAtom              "\<[a-zA-Z][a-zA-Z0-9\._\-]*\>"

syn match   cvLineComment      "--.*$"
syn region  cvBlockComment     start="{-" end="-}" contains=cvString,cvBlockComment

hi def link cvAtom                        Identifier
hi def link cvDelimiter                   Delimiter

hi def link cvSpecialCharError            Error
hi def link cvSpecialChar                 SpecialChar
hi def link cvStringGap                   SpecialChar
hi def link cvString                      String
hi def link cvNumber                      Number
hi def link cvFloat                       Float

hi def link cvBlockComment                cvComment
hi def link cvLineComment                 cvComment
hi def link cvComment                     Comment

let b:current_syntax = "config-value"
