" Vim syntax file
" Language:         Simple Programming Language
" Maintainer:       Tim Steenvoorden <steenvoo@science.ru.nl>
" Latest Revision:  2013-04-14


" Initialize Syntaxfile: {{{1
" ======================

if exists('b:current_syntax')
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

" Syntax Definitions: {{{1
" ===================

syn keyword splConditional   if else
syn keyword splRepeat        while
syn keyword splStatement     return

syn keyword splType          Void Int Bool
syn keyword splFunction      print isEmpty head tail fst snd main

syn keyword splConstant      True False
syn match   splNumber        '\<\d\+\>'

syn match  splComment        '//.*$'
syn region splComment         start='/\*' end='\*/'

" Highlight Definitions: {{{1
" ======================

hi def link splConditional   Conditional
hi def link splRepeat        Repeat
hi def link splStatement     Statement

hi def link splType          Type
hi def link splFunction      Function
hi def link splConstant      Constant

hi def link splComment       Comment

" Finalize Syntaxfile: {{{1
" ====================

let b:current_syntax = 'spl'

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: nowrap fdm=marker spell spl=en
