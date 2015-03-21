---
title: Mutable variables are a side channel
description: Mutability can hide communication between parts of a program
tags: programming
...

Interesting point on data abstraction in
[*TAPL*](http://www.cis.upenn.edu/~bcpierce/tapl/), §24.2:

> [Mechanisms] for modularity and abstraction are almost completely orthogonal
> to the statefulness or statelessness of the abstractions being defined.
> Working with purely function programs sometimes makes the typing problem more
> interesting [because] in imperative programming, mutable variables provide
> a “side channel” allowing direct communication between distant parts of
> a program. In purely functional programs, all information that passes between
> different parts of the program must go via the arguments and results of
> functions, where it is “visible” to the type system.
