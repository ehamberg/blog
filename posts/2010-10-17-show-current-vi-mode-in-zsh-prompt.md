---
title: Show current vi mode in your zsh prompt
description: Don't lose track of which vi mode zsh is in
tags: vim, zsh, unix
---

Having a brain damaged by prolonged use of vim I even use a vi input mode for my
shell. I recently switched from bash to zsh and among other niceties zsh can
show which vi mode I'm currently in and update this in real time as I switch. I
found out how to do this from [Aaron Toponce's
blog](http://pthree.org/2009/03/28/add-vim-editing-mode-to-your-zsh-prompt/).

To have a VIMODE variable which is updated as you switch modes, add this to your `~/.zshrc`:

~~~{.bash}
# set VIMODE according to the current mode (default “[i]”)
VIMODE='[i]'
function zle-keymap-select {
 VIMODE="${${KEYMAP/vicmd/[n]}/(main|viins)/[i]}"
 zle reset-prompt
}

zle -N zle-keymap-selec</pre>
You can then add $VIMODE to your prompt:
# Set the prompt to “[user]@[host[ [vi mode] $ ”
PROMPT="%n@%m ${VIMODE} \\$ "
~~~
