---
title: zsh: Temporary file with output from command
description: A neat zsh process substitution trick
tags: erlang, unix
...

I have often used process substitution in zsh (and bash):

    $ diff <(ls dir1) <(ls dir2)

This will create named pipes with the output of the commands and substitutes
`<(...)` with the name of the pipe:

    $ file <(echo lol)
    /dev/fd/11: fifo (named pipe)

A neat trick I picked up in `man zshexpn` is that it's also possible to have
command substitution return an ordinary file by using `=(...)`:

    $ file =(echo lol)
    /tmp/zshHQJRHb: ASCII text

To quote `man zshexpn`:

> If =(...) is used instead of <(...), then the file passed as an argument will
> be the name  of  a  temporary  file containing  the output of the list
> process.  This may be used instead of the < form for a program that expects to
> lseek (see lseek(2)) on the input file.

This is nice to know when using programs that need to seek in their input files,
e.g. `unzip`:

    $ unzip =(curl -s http://foo.org/foo.zip)
