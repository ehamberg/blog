---
title: Autosquashing in git rebase
description: Get git to prepare interactive rebasing for you
tags: git
...

A pretty nifty git trick, if you often interactively rebase your feature
branches, is to set the `rebase.autosquash` option (i.e. `git config --global
rebase.autosquash true`). By setting this option, or passing `--autosquash` to
`git rebase -i`, you can have commits pre-marked as *fixup* or *squash* commits
by putting “fixup!” or “squash!” in their commit message, followed by a prefix
of the commit message of the commit you want it applied to.

So, given the following history:

    01b7812 - (HEAD,feature) fixup! added
    a0f1e7b - modified a
    b88bf82 - added b, c and d
    af0e25c - (master) initial commit

running `git rebase -i master` will present you with the following list:

    pick b88bf82 added b, c and d
    fixup 01b7812 fixup! added
    pick a0f1e7b modified a

(Note that the "fixup" commit was moved to the right place.)
