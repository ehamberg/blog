---
title: Visual block mode for Kate's Vi Mode
description: Finally, Kate's Vi mode has visual block mode support
tags: kate, vim, programming
---

One of the most missed features of Kate's Vi input mode is Vim's *visual block
mode*. Visual block mode is entered by pressing `ctrl+v` and allows a
rectangular block of text to be selected and manipulated. Also, text can be
prepended or appended to the block, which is useful for, e.g. commenting out a
range of lines.

Well, good news, everyone! There is now experimental support for visual block
mode in Kate's Vi input mode. Most text manipulation commands should already
support visual block mode, and prepending/appending text (`shift+i` and
`shift+a`, respectively) works, and you can select to end-of-line with `$`, as
in Vim. I also made it possible to re-select the last visual selection with
`gv`, and the marks `'<` and `'>` are set to the start and end position of the
last visual selection. (Further down the road it will hopefully be possible to
use marks in *ex* commands, too.)

![A block selected by visual block mode](/images/visblock.png)

There are probably still some rough edges, but visual block mode should already
be usable. If you want to help test it, [build Kate from
git](http://kate-editor.org/get-it/) and try it out.
