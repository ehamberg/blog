---
title: A nice, little known C feature: Static array indices in parameter declarations
description: Explanation of a little known C feature – and yet another use of the “static” keyword!
tags: c, programming
...

The people who created C sure loved keeping the number of keywords low, and
today I'm going to show you yet another place you can use the `static` keyword
in C99.

You might have seen function parameter declaration for array parameters that
include the size:

```cpp
void foo(int myArray[10]);
```

The function will still receive a naked `int *`, but the `[10]` part can serve
as documentation for the people reading the code, saying that the function
expects an array of 10 `int`s.

But, you can actually also use the keyword `static` between the brackets [^1]:

```cpp
void bar(int myArray[static 10]);
```

This tells the compiler that it should assume that the array passed to `foo` has
*at least* 10 elements. (Note that this rules out a `NULL` pointer!)

Doing this serves two purposes:

- The compiler could use this information when optimizing the code [^2]
- The compiler can warn callers when it sees them calling the function with
  anything but an array of 10 or more `int`s.

So, let's see what actually happens when compiling a program with the above
definition of `bar` when passing the following three arguments to it [^3]:

- Passing `NULL`:

```cpp
bar(NULL);
```

    warning: null passed to a callee which requires a non-null argument [-Wnonnull]
        bar(NULL);
        ^   ~~~~

- Passing a smaller array:

```cpp
int a[9];
bar(a);
```

    warning: array argument is too small; contains 9 elements, callee requires at least 10 [-Warray-bounds]
        bar(a);
        ^   ~

- Passing a larger array

```cpp
int b[11];
bar(b);
```

    [no output]

This is great for those cases where you actually know the size of the array a
function should be passed since it both serves as documentation for people
reading the code, and lets the compiler help you catch mistakes.

[^1]: You can actually also use the `const` keyword between the brackets. This
will make the pointer to `myArray` a const pointer.
[^2]: Not sure if any compilers actually do that, however, since it would be
risky to assume that programmers actually heed the warning
[^3]: Compiler: `Apple clang version 4.1 (tags/Apple/clang-421.11.66) (based on LLVM 3.1svn)`

