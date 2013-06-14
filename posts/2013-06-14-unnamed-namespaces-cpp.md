---
title: Unnamed Namespaces in C++
description: One less use of the “static” keyword
tags: c++, programming
...

I have touch upon the many uses of the keyword static in C when I discovered yet
another usage of the `static` keyword in C99 in my post about [static array
indices in parameter declarations](/posts/2013-02-18-static-array-indices.html).
That usage is not legal in C++[^1], and C++ has actually removed the need to use
`static` for one use case: to specify internal linkage.

From the [C++11 standard N3242
(draft)](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2011/n3242.pdf)
§3.5.4:

> An unnamed namespace or a namespace declared directly or indirectly within an
> unnamed namespace has internal linkage. All other namespaces have external
> linkage.

In other words, this code

```cpp
namespace {
    int foo() { return 0xf00; }
}
```

is equivalent to

```cpp
static int foo() { return 0xf00; }
```

If the C++ Standards Committee ever actually removes this usage [^2], it would
be a rare example of actually decreasing the overloading of `static`.

[^1]: To quote `clang++`: *error: static array size is a C99 feature, not
permitted in C++*
[^2]: Don't hold your breath.
