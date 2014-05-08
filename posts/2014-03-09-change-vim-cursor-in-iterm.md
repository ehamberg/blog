---
title: Change Cursor in Vim in iTerm2
description: In iTerm2 it's possible to change Vim's cursor depending on the mode you're currently in
tags: vim, OS X
...

A neat tricks I stumbled over in [iTerm2's
documentation](http://www.iterm2.com/#/section/documentation) is that one can
change the cursor shape from the block (default) to a vertical bar. This is
quite neat to do for Vim's insert mode where the cursor conceptually is
*between* characters. Adding the following to `~/.vimrc` [^1] will change the
cursor to a vertical bar in insert mode and keep it as a block in the other
modes. This should also work in Konsole, as far as I know, so if you use
Konsole you should extend/replace the `$TERM_PROGRAM` check to look for Konsole.

```vim
" Change cursor shape between insert and normal mode in iTerm2.app
if $TERM_PROGRAM =~ "iTerm"
    let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
    let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
endif
```

A screenshot of how this looks:

![iTerm2 cursor in Vim's insert and normal mode](/images/vim_cursors.png)

[^1]: Or `~/.vim/vimrc` which in Vim 7.4 was finally added as a possible
`vimrc` location, making it possible to gather all vim-related files in
`~/.vim`.
