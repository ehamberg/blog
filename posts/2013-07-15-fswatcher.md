---
title: fswatcher – a platform-agnostic inotifywait
description: A simple program for running a command on file changes
tags: haskell, programming
---

`fswatcher` is a small utility I have written to have a program that runs a
given command when a file or a directory is modified and that will work on both
OS X and Linux. (It should also work on Windows, but I haven't tested.)

I have just uploaded it to Hackage, so it should be possible to install with
`cabal install fswatcher`.

It is really simple and really just a thin wrapper on top of the
[fsnotify](http://hackage.haskell.org/package/fsnotify) library. It takes two
arguments: a file/directory to watch, and a command to run on changes:

    $ fswatcher report.md pandoc report.md -o report.pdf
    Started to watch report.md [→ /private/tmp/report.md]
    Running pandoc report.md -o report.pdf...
    Process completed successfully
    Running pandoc report.md -o report.pdf...
    Process completed successfully
    ^C
    Stopping.

It's quite simple and uses a separate thread to run the provided command
whenever an `MVar` "trigger" is filled. This `MVar` contains nothing or a unit
(`()`), and the thread listening for events uses `tryPutMVar` to avoid
re-running long-running commands for every change. Since `tryPutMVar` will
succeed only once per command invocation, the command will only be re-run once
if several changes happened while it was running.

The code is short and simple and [available on
github](https://github.com/ehamberg/fswatcher/).
