---
title: Playing With SBV
description: Solving a puzzle with a SAT solver
tags: haskell
...

John D. Cook [posted a puzzle](http://www.johndcook.com/blog/2015/07/06/multiple-choice/) on his blog which is so short I'll reproduce it here:

    A certain question has the following possible answers.
    
        a. All of the below
        b. None of the below
        c. All of the above
        d. One of the above
        e. None of the above
        f. None of the above
    
    Which answer is correct?

Now, this is probably where one should find paper and a pencil, write out the implications,
simplify, hopefully come up with a solution and then feel clever. But not wanting to feel clever
today -- or perhaps for fear of not being clever -- I decided to try and model the problem in
[Z3](http://research.microsoft.com/en-us/um/redmond/projects/z3/), Microsoft's open source theorem
prover. I installed Z3 and then installed the [`sbv`](http://hackage.haskell.org/package/sbv)
library which is a frontend for several SMT solvers, but defaults to using Z3. A [nice introduction
to `sbv`](https://ocharles.org.uk/blog/guest-posts/2013-12-09-24-days-of-hackage-sbv.html) can be
found as part of the “24 days of Hackage” series.

Okay, so let's encode the formulas. `bAll` is a generalized `all` and `(<+>)` is exclusive or.

```haskell
import Data.SBV

p a b c d e f = bAll id [pa, pb, pc, pd, pe, pf]
  where pa = a <=> bAll id [b,c,d,e,f]   -- “All of the below”
        pb = b <=> bAll bnot [c,d,e,f]   -- “None of the below”
        pc = c <=> bAll id [a,b]         -- “All of the above”
        pd = d <=> a <+> b <+> c         -- “One of the above”
        pe = e <=> bAll bnot [a,b,c,d]   -- “None of the above”
        pf = f <=> bAll bnot [a,b,c,d,e] -- “None of the above”
```

Okay, so we have our formulas, and we specify that we want all of them to hold as the body of `p`.
Now let's see if this is satisfiable.

    λ> allSat p
    Solution #1:
      s0 = False
      s1 = False
      s2 = False
      s3 = False
      s4 = True
      s5 = False
    This is the only solution.
    (0.01 secs, 4,361,872 bytes)

Boom! It seems that *e* is the solution (if my encoding is correct :).
