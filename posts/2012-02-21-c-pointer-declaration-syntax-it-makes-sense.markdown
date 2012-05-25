---
title: C/C++ Pointer Declaration Syntax – It makes sense!
description: C pointer declaration syntax makes sense – kind of
tags: c, programming
---

I never really liked the way pointers are declared in C/C++:

~~~{.cpp}
int *a, *b, *c; // a, b and c are pointers to int
~~~

The reason is that I am used to reading variable declarations as `MyType myVar1,
myVar2, myVar3;` and I always read `int*` as the type “integer pointer”. I
therefore wanted the following

~~~{.cpp}
int* a, b, c; // a is a pointer to int, b and c are ints
~~~

to mean that `a`, `b` and `c` all were of type `int*`, i.e. pointers to `int`. I
therefore found it slightly annoying to repeat the asterisk for every variable.
This also meant that the symbol `*` had two slightly different meanings to me:
(1) It declares a pointer or (2) it dereferences a pointer. I usually don't
declare a whole lot of pointers in one line, but still, this is a (minor)
annoyance I have briefly discussed with few fellow programmers over the years.
Today I started reading *C Traps and Pitfalls* by Andrew Koenig and after
reading one sentence, in chapter two, the pointer declaration syntax suddenly
makes – at least *some* – sense:

    […] Analogously,
    
    float *pf;
    
    means that *pf is a float and therefore that pf is a pointer to a float.

Of course! If we instead of looking at it as a variable `a` of type `int*`, read
it as `*a` – i.e. “`a` dereferenced” – it makes sense. That is indeed an `int`,
and that also means that `*` always means “dereference”.

## Discussion

- [r/programming](http://www.reddit.com/r/programming/comments/pz3n0/cc_pointer_declaration_syntax_it_makes_sense/)
- [hacker news](http://news.ycombinator.com/item?id=3615750])
