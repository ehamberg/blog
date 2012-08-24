---
title: Kate's Vi Input Mode – What will KDE 4.4 bring?
description: What is new in the Vi input mode for Kate in KDE 4.4?
tags: kate, kde, programming
---

<div style="border: 1px solid black; margin: 0pt auto -1px; padding: 1em; background-color: #eeeeee; width: 50%; text-align: center;">Please see [http://hamberg.no/erlend/kate-vi-mode/](http://hamberg.no/erlend/kate-vi-mode/) for an updated overview of the Kate VI mode project.</div>

Dear "Katevim" users.  Kate's Vi Mode is steadily improving and I want to take a
moment to tell what's on the horizon for KDE 4.4. There has been quite a few
bugs fixed since 4.3, but some major new features have also been introduced:

### The Comma Text Object

This is something that I have been missing in Vim. The comma text object makes it easy to modify parameter lists in C-like languages and other comma separated lists. It is basically the area between two commas or between a comma and a bracket. In the line below, the three ranges this text object would cover is highlighted in red.

![Comma text object ranges. If the cursor is over, say, "arg2", pressing `ci,`  ("change inner comma") would delete "double arg2" and place the cursor between the two commas in insert mode. A very convenient way to change a function's parameter](/images/comma_to.png)

This has actually been in Kate's Vi Mode for a while now, but since it has not been announced until now, I guess I am the only one using it. :-)

### Limited Normal Mode Mapping Support

It is now possible to add mappings in normal mode (**:nnoremap** in Vim). There
are still some flaws, such as the fact that one cannot map a keypress to
"**:somecommand<enter>**", but hopefully people who are using
dvorak/colemake can make use of the vi mode now.

![It is now possible to map keypresses in normal mode](/images/mapping.png)

### Command Line Mode Commands

Yes, really! Kate's Vi Mode finally support some of the most command commandline
mode commands from Vim. What can be done from the Kate editor part is limited by
the fact that it can't control its hosting application, but Kate -- the
application -- has now gotten support for the following commands:

- `q`, `qa` 
-  `w`, `wq`, `wa`, `wqa` 
- `x`, `xa` 
- `bn`, `bp` 
- `new`, `vnew` 
- `edit`

![Kate showing help text for a command line mode command](/images/wqa_help.png)
