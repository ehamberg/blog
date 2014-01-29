---
title: Classes are a Dumping Ground for Language Features
description: When classes are the only structuring mechanism, they tend to accumulate features
tags: programming
...

I am currently reading Benjamin C. Pierce's [Types and Programming
Languages](http://www.cis.upenn.edu/~bcpierce/tapl/) (2002), and in chapter\
18 -- on imperative objects and classes -- he offers an off-hand comment about
why the class features in most OOP languages are so complicated.

As someone who have spent countless hours writing C++ (and to some extent,
Java), this immediately jumped out at me:

> The class mechanisms in real-world object-oriented languages tend to be
> complex and loaded with features---`self`, `super`, visibility annotations,
> static fields and methods, inner classes, friend classes, annotations such as
> `final` and `Serializable`, etc., etc.

> The main reason for all this complexity is that, in most of these languages,
> classes are the *only* large-scale structuring mechanism. Indeed, there is
> just one widely used language---OCaml---that provides both classes and a
> sophisticated module system. So classes in most languages tend to become
> dumping ground for all language features that have anything to do with
> large-scale program structure.
