---
title: Translating a Fibonacci Sequence Generator to Python
description: A non-Python Programmer writes Python
tags: python, programming
...

The following `fib` function in Haskell efficiently generates values from the
Fibonacci series:

```haskell
fib = 0:1:zipWith (+) fib (tail fib)
```

It works by zipping (i.e. combining elements from) the list
`0:1:(0+1):(1+1):(1+2):...` and the tail of the same list (i.e.
`1:(0+1):(1+1):(1+2):...`). The (almost) magic thing is that the sums in the
list are sums of elements from the same list, just one and two elements behind,
respectively.

Anyway... Today, for some reason, I was trying to write this function in Python
on a whiteboard, and I'm quite sure I screwed up the python syntax (well, I
know I did -- I completely forgot the `yield` keyword used in Python
generators to name one thing).

So, to brush up on my Python skills, I had the Python interpreter yell at me
until I got it right. The following generator generates an infinite list of
Fibonacci numbers and `fib` allows you to ask for a specific number.

```python
from itertools import islice, chain, imap

def fibgen():
    def tail(iterable): return islice(iterable, 1, None)
    for n in chain([0,1], imap(lambda a,b: a+b, fibgen(), tail(fibgen()))):
        yield n

def fib(n):
    return list(islice(fibgen(), n, n+1))
```

This actually turned out to be quite different from the Haskell version. Since
we can only access the `next` element of a generator, it doesn't make sense to
have two references to previous elements of the same list we're generating (it's
not even a list, after all). So, let have the generator have two versions of
itself, drop one element from one of them and then have the (outer) generator
yield the sum of the next items from its two inner generators, ad infinitum.

However, these two inner generators will have their own generators, and so on,
so this will be dog slow.

(Yes, yes, the way it should *actually* be done is to just keep track of the two
previous *values*.)

```python
def fibgen():
    n1, n2 = 0, 1
    while True:
       yield n1
       n1, n2 = n2, n1 + n2
```
