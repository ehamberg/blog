---
title: GSoC 2008: Vi input mode for the Kate kpart
description: I will create a vi input mode for Kate for Google Summer of Code
tags: gsoc, kate, programming
---

I'm one of the lucky students doing a Google Summer of Code project for KDE, and
I would like to introduce my project.  I'm making a vi-like, modal editing mode
for the Kate kpart. I'm doing this because many people -- me included -- find
using a vi-like editor much more efficient and pleasant than using a non-modal
editor.  Especially for editing code. This will of course be an optional mode,
and if you don't want to use it, you will never notice it. This should be
obvious, but I want to mention it because I have already gotten a reaction from
a Kate user who were frightened to death when he heard the word “vi” and feared
that I will destroy his beloved Kate. Fear not! :-)

My reference vi implementation is Vim, which is superior to the original vi in
many ways. Especially [text
objects](http://vimdoc.sourceforge.net/htmldoc/usr_04.html#04.8) is a brilliant
addition and will be supported by Kate's vi input mode.

As part of this I will also improve Kate's command line and its commands, which
will also be beneficial for Kate user's who won't use the vi input mode.

By building on Kate's proven foundation it hasn't been too hard to get things up
and running pretty fast. I'm still working on the foundation for the vi commands
handling, but it's already usable (to some extent) for me, since I know its
limitations and how to not make it crash. ;-)

I have been eating my own dog code for a week now, which has been frustrating
some times, but getting better every day. Especially support for marks
(”anchors” in a file one can easily set and jump to) made my life easier.

Working with the Kate source code has generally been a very pleasant experience, and the code lines changed in the Kate core to tie in the vi input mode can almost be counted on one hand.

What needs to be done next is to make it possible to use `ctrl`/`alt`/`meta`
commands with my command framework. There are lots of improvements and clean-up
that needs be done with the implemented commands/motions/text-objects code as
well, but this will have to wait until my current framework has proven its
ability to cope with all scenarios.

Visual mode is also to-do, but I should be able to re-use most of the
motions/text-objects code from the normal mode code.

The really exciting part will be when I have to figure out how to handle Vim's
`ctrl+key` commands. I have a sneaking suspicion that commands like `ctrl+n`,
`ctrl+p`, `ctrl+w`, etc. will clash with other actions in the programs using the
Kate kpart ;-)
My thoughts on this for now is that these actions shouldn't override the host
program's action by default, but users who don't mind the vi input mode eating
their keypresses should be able to use these Vim commands.

Oh, and I almost forgot:

**Frequently asked question:**

Wow, will I be able to use a vi-like editor in Kdevelop?!
:   Yes, that should be possible, and is one of my most motivating goals for doing this. :-)

**Asked questions:**

Will you support vim scripts and loading of my vimrc file?
:   Vim scripts: probably not. This would be a huge effort, and Kate's scripting engine is very capable and is being improved continually. I won't stop anyone from doing it, though. ;-) I plan to use vim-compatible names for commands and options for the command line mode, as most of them are quite good names anyway, at least in their long form. Maybe it will be possible to load a vimrc and use the supported options in the future…

Will you implement 100% of Vim's functionality?
:   Probably not. Certainly not for my GSoC project. Vi is 32 years old…  The aim is not replace Vim, the aim is to have an editor kpart for us who like to use a modal, vi-like editor. Simple as that.

Will it be easy to extend this beyond the GSoC project?
:   Yes, it will be easy to add new commands in the future.
