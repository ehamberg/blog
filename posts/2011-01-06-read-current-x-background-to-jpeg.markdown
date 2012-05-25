---
title: Read current X background image to a JPEG file
description: Ever wanted to dump your current background to a file? No?
tags: programming, linux
---

Hack of the day:

Problem
:   I wanted to be able to fade to a new background image. But how could I get the current background image so that the two images can blended together?
Solution
:   I dived into Xlib for the first time since 2003 and came up with a small C program that reads the current background image and writes it to a JPEG file.

I now have a script that downloads a new background image every hour and nicely
fades to this new background by getting the current background and gradually
blending the new image over this. :-)

Code:

<script src="https://gist.github.com/767824.js"> </script>
