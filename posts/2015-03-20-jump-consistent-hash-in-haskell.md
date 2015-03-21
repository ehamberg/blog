---
title: Jump Consistent Hash in Haskell and C++
description: How hidden behaviour in C++ can be an important part of an algorithm
tags: programming, c++, haskell
...

A paper by John Lamping and Eric Veach^[Lamping, J., & Veach, E. (2014).
A Fast, Minimal Memory, Consistent Hash Algorithm. arXiv Preprint
arXiv:1406.2294, (1), 1–12. Retrieved from <http://arxiv.org/abs/1406.2294>]
describing Jump consistent hash, a *fast, minimal memory, consistent hash
algorithm that can be expressed in about 5 lines of code*. The accompanying C++
code is indeed short and sweet:

```cpp
int32_t JumpConsistentHash(uint64_t key, int32_t num_buckets)
{
    int64_t b = 1, j = 0;
    while (j < num_buckets) {
        b = j;
        key = key * 2862933555777941757ULL + 1;
        j = (b + 1) * (double(1LL << 31)/double((key >> 33) + 1));
    }
    return b;
}
```

However, there are a lot of implicit type conversions in the above code. This
becomes apparent if you implement the algorithm in a language without implicit
conversions. The following implementation in Haskell is quite close to a 1:1
translation of the C++ code (but using recursion instead of a `while` loop),
and as you can see, there are five different type conversions happening (the
conversions are named in the code):

```haskell
module JumpConsistentHash (jch) where

import Data.Bits (shiftL, shiftR)
import Data.Int (Int64, Int32)
import Data.Word (Word64)

jch :: Word64 -> Int32 -> Int32
jch key numBuckets = jch' numBuckets key 1 0

jch' :: Int32 -> Word64 -> Int64 -> Int64 -> Int32
jch' n k b j
  | j >= i32ToI64 n = i64ToI32 b
  | otherwise =
      let k' = k * 2862933555777941757 + 1
          z  = i64ToD (1 `shiftL` 31)/ui64ToD ((k' `shiftR` 33) + 1)
          j' = dToI64 ((i64ToD j + 1) * z)
       in jch' n k' j j'

i32ToI64 :: Int32 -> Int64
i32ToI64 = fromIntegral

i64ToI32 :: Int64 -> Int32
i64ToI32 = fromIntegral

i64ToD :: Int64 -> Double
i64ToD = fromIntegral

ui64ToD :: Word64 -> Double
ui64ToD = fromIntegral

dToI64 :: Double -> Int64
dToI64 = floor
```

So, in the (seemingly) simple algorithm, we have the following conversions
taking place:

- The `int32` to `int64` conversion happens when comparing `j` and `b` and is
  done through sign extension.
- The `int64` to `int32` conversion is the last step of the algorithm and is
  done when returning `b`, which was a 64-bit integer in the loop, to a 32-bit
  integer. **This behaviour is actually implementation-defined(!)**^[*The value
  is unchanged if it can be represented in the destination type (and bit-field
  width); otherwise, the value is implementation-defined.*] ^[C++11 Standard
  §4.7.3], so the algorithm relies on the compiler doing the sensible thing
  here. In practice, this is probably not a problem.
- The `int64` to `double` and `uint64` to `double` conversions happen when
  doing floating-point calculations with integer values and are
  “floating-integral conversions”. Loss of precision occurs if the integral
  value cannot be represented exactly as a value of the floating type. If the
  value being converted is outside the range of values that can be represented,
  the behavior is undefined. ^[C++11 Standard §4.9.2]
- The `double` to `int64` conversion truncates, i.e. discards the fractional
  part, of the `double`. ^[C++11 Standard §4.9.1]

This was, at least for me, an interesting case of how often “hidden” behaviour
can be a crucial part of code.
