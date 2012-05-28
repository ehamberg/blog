---
title: Vim tip: Highlight Function Names in C Code
description: Neat Vim trick
tags: c, programming, Vim
---

When looking at function signatures such as

    EXPORT _foo_bar_handle_restricted _foo_common_alloc_restricted( … )

it's often hard to quickly see what the function name is among all the spaces
and underscores. To help with this, many editors support highlighting function
names. Vim can also do this, e.g. by using a tags file, but for large code bases
this can be quite slow. The following syntax definitions use a simple heuristic
to highlight function names in C code by making their names boldface. In my
experience it works quite well.

~~~{.vim}
" Highlight Function names.  From
" http://stackoverflow.com/questions/736701/class-function-names-highlighting-in-vim/773392#773392

syn match    cCustomParen    "(" contains=cParen contains=cCppParen
syn match    cCustomFunc     "\w\+\s*(" contains=cCustomParen

hi def link cCustomFunc  Function

hi Function term=bold gui=bold
view rawhighlight_functions.vimThis Gist brought to you by GitHub.
~~~

To use this, put it in `~/.vim/after/syntax/c/highlight_functions.vim`. (All
files in `~/.vim/after/syntax/[filetype]/` will be sourced when vim opens a file
of type `[filetype]`.)
