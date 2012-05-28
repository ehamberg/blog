---
title: C Trigraph Trap
description: How “??/” can lead to disaster
tags: c, programming
---

~~~{.cpp}
// gcc --std=c99 trigraph.c -o test && ./test

#include <stdio.h>

int main(int argc, const char *argv[])
{
    int x = 1;

    // what is going on here??/
    x = 0;

    printf("x = %i\n", x);

    return 0;
}
~~~

What will the above C code print?

Let's take a look:

    $ gcc –std=c99 trigraph.c -o test
    $ ./test
    x = 1

Wait -- `x` was just set to be zero! How can it still be one‽

We get a hint if we don't specify the language standard:

    $ gcc trigraph.c -o test
    trigraph.c:7:29: warning: trigraph ??/ ignored, use -trigraphs to enable
    $ ./test
    x = 0

So it looks like trigraphs have something to do with this weird behaviour. A
trigraph is a way to write sequences of characters that represent other
characters – usually special characters that were not always on all keyboards.
If we take a look at the [Wikipedia article under
“C”](http://en.wikipedia.org/wiki/Digraphs_and_trigraphs#C), we see the
following translation rule:

    ??/ → \

... which means that the comment line ends with `\` and thus continues to the
next line, commenting out `x = 0`.

Source: comment by SteveB in [this](http://www.elpauer.org/?p=971) discussion
thread.

