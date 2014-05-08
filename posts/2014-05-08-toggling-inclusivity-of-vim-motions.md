---
title: Toggling inclusivity of Vim motions
description: One weird trick to double the amount of motions in Vim
tags: vim
...

I don't often see a fundamental Vim feature that's new to me[^1], but today
I learned about `o_v` from [a comment on
/r/vim](http://www.reddit.com/r/vim/comments/24wbuz/command_to_delete_one_full_word_backwards/chbbkfk).
From [`motion.txt`](http://vimdoc.sourceforge.net/htmldoc/motion.html):

    v     When used after an operator, before the motion command: Force the
          operator to work characterwise, also when the motion is linewise.  If
          the motion was linewise, it will become exclusive.
          If the motion already was characterwise, toggle inclusive/exclusive.
          This can be used to make an exclusive motion inclusive and an
          inclusive motion exclusive.

So what does this mean? Well, vim motions are either *inclusive* or *exclusive*.
This dictates whether the character/line motion moves to should be affected by
the preceding operator or not. The `b` motion (back `[count]` words) is
exclusive, so if we have the text `foo̲bar` (the cursor is on the second ‘o’)
`db` would result in `o̲bar` since the operator `d` is applied to the range
`[foo)`. If we would like it to include the second ‘o’ as well, we can use our
new friend `v` to create the command `dvb`, which turns `foo̲bar` into `b̲ar`
Antother -- perhaps more useful example -- is if you want to delete from the
current column in a line, to the position just below the cursor (in the
following line), you could use `dvj` to turn the linewise motion `j` into a
characterwise motion.

I honestly don't know if this will be useful, but I'm excited to discover such
a fundamental feature nonetheless.

[^1]: I have used Vim for many, many years, and even implemented a [Vim mode for
the text editor Kate](http://kate-editor.org/kate-vi-mode/).
