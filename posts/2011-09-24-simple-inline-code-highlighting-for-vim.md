---
title: Simple Inline Code Highlighting with Vim
description: Syntax highlighting for inline code in Markdown
tags: vim, programming, gitit
---

I use the excellent gitit wiki for keeping notes. gitit uses markdown as its
default markup format with some extensions, such as the possibility to have
inline code snippets highlighted. These snippets start with `~~~{.someLanguage}`
and end with `~~~`, e.g:

    ~~~{.haskell}
    quicksort [] = []
    quicksort (x:xs) = quicksort (filter (<x) xs) ++ [x] ++ quicksort (filter (>=x) xs)
    ~~~

Since I use the Firefox add-on Pentadactyl (a fork of Vimperator), I can use Vim
to edit my wiki articles. I really wished that I could get the correct
highlighting for the in-line code snippets in Markdown and I found Vim tip #857.
I used this to create the following filetype plugin for markdown files that will
find code snippets for the languages I use and make vim colourize those snippets
with the correct syntax highlighting.

~~~{.vim}
" from http://vim.wikia.com/wiki/VimTip857
function! TextEnableCodeSnip(filetype, start, end, textSnipHl)
  let ft=toupper(a:filetype)
  let group='textGroup'.ft
  if exists('b:current_syntax')
    let s:current_syntax=b:current_syntax
    " Remove current syntax definition, as some syntax files (e.g. cpp.vim)
    " do nothing if b:current_syntax is defined.
    unlet b:current_syntax
  endif
  execute 'syntax include @'.group.' syntax/'.a:filetype.'.vim'
  try
    execute 'syntax include @'.group.' after/syntax/'.a:filetype.'.vim'
  catch
  endtry
  if exists('s:current_syntax')
    let b:current_syntax=s:current_syntax
  else
    unlet b:current_syntax
  endif
  execute 'syntax region textSnip'.ft.'
        \ matchgroup='.a:textSnipHl.'
        \ start="'.a:start.'" end="'.a:end.'"
        \ contains=@'.group
endfunction

for l in ["cpp", "haskell", "python", "scala", "dot", "xml"]
  call TextEnableCodeSnip(l, '^\s*\~\{3,}{\s*\.'.l.'\s*}\s*$', '^\s*\~\{3,}\s*$', 'specialComment')
endfor

" hightlight latex maths
call TextEnableCodeSnip("plaintex", '\$\+', '\$\+', 'specialComment')
~~~
