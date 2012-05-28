---
title: HsUnixCompat.hs on Debian
description: Solution to the “Missing header file: HsUnixCompat.h” problem
tags: ghc, haskell, debian
---

Just so that a others (hopefully) will be spared the annoying and time-consuming work of tracking down the source of the following error message:

    * Missing header file: HsUnixCompat.h

On my Debian system (Debian 5.0.3 -- "Lenny") the missing package was `libbsd-dev`:

    $ cabal unpack unix-compat
    Unpacking unix-compat-0.1.2.1...
    $ cd unix-compat-0.1.2.1/
    $ runhaskell Setup.lhs configure -v3
    […]

    /usr/bin/gcc returned ExitFailure 1 with error message:
    In file included from include/HsUnixCompat.h:1,
    from /tmp/18515.c:1:
    /usr/lib/ghc-6.10.4/unix-2.3.2.0/include/HsUnix.h:79:21: error: libutil.h: No such file or directory
    $ apt-file search libutil.h
    libbsd-dev: /usr/include/libutil.h
