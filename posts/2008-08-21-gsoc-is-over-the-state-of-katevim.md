---
title: GSoC is over: The state of Kate's Vi input mode
description: State of Kate's Vi input mode now that GSoC is over
tags: gsoc, kate, programming
---

The Google summer of code programme's coding part officially ended this Monday.
That won't be the last you will hear from me, though. Kate's vi input mode has
turned out to be quite nice, and there are lots of improvements I want to do in
the coming months. The biggest news are a visual mode + visual line wise mode.
Visual mode is one of those things that really make vim stand out from all of
the vi clones, and having support for it makes Kate's vi input mode it much more
usable for me.

There are also lots of new commands and support for
setting and jumping to marks. These are not saved between sessions yet, though.
Overall, it's already quite usable, even if it still has its small quirks and
bugs here and there. This will definitely better quite soon as I use it quite
extensively myself, and fix bugs as I find them.

A really nice feature is that one can quickly toggle the vi input mode on and
off by using the view menu or a short cut (default is `ctrl+meta+v`).

The big thing left to do is to enchance Kate's built-in command line. I want to
add support for the most command vi commands and make it support ranges, setting
of options, etc. The current command line implementation isn't really suited for
this, so it probably needs to be modified quite a bit.  Hopefully it will
eventually be possible to send commands to the app hosting the katepart too.
Asking it to quit, close a buffer, etc. Right now the only command supported is
**:w**. ;-)

I am very satified with what I accomplished during GSoC. I had to change some of
my plans underway, mostly due to actually using my work, and seeing which
features I missed the most. GSoC was a nice start, but there are still lots of
things I want to do. :-)
