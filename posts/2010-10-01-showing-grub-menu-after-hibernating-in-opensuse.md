---
title: Showing grub menu after hibernating in openSUSE
description: How to show grub menu after when restarting after hibernating openSUSE
tags: linux, opensuse, grub
---

When hibernating in openSUSE and starting the computer again the grub boot menu
is not shown and you are instead taken directly to openSUSE. This is most often
what you want, but if you have a dual boot system like me and want to be able to
switch between Linux and Windows without ending the session in either end, this
is annoying.

Luckily there is a way to show the grub menu after hibernating: If you edit the
file `/usr/lib/pm-utils/sleep.d/99Zgrub` and comment out the last two `if`
blocks, grub will be shown as usual^[This solution is taken from
[this thread](http://forums.opensuse.org/english/get-help-here/install-boot-login/435012-suspension-hibernation-grub.html) in the openSUSE forums.].

P.S: make absolutely sure that your Linux file systems are not mounted in
Windows and vice versa. This can lead to data loss!
