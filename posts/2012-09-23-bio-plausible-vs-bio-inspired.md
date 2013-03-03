---
title: Bio-Plausible vs Bio-Inspired
description: Essay on bio-plausibility vs bio-inspiration in science
tags: ai
...

> Note: This essay was written in 2010 as part of a course on advanced
> bio-inspired methods at the Norwegian University of Science and Technology
> (NTNU).

## Introduction

The field of bio-inspired artificial intelligence approaches the problem of
creating intelligent systems from a bottom-up perspective inspired by systems
and processes seen in nature. Through the study of biology we have learnt that
many complex natural systems are made up of relatively simple parts, or are the
results of relatively simple processes. Through his theory of natural
selection, Charles Darwin showed how complicated organisms can result from
random variation coupled with non-random selection -- showing us how increasing
complexity can arise through a simple process. Other areas that have been
studied, such as swarm behaviour in social insects and the workings of the
nervous system, have also given us insights into systems where interaction
between simple parts lead to complex results.

In the light of Darwin's theory of evolution through natural selection it is
not surprising that even the most complex systems show these properties.
Evolution has no foresight and a complex end product must either have entailed
an advantage at every step, or be the result of interaction between existing
parts.

Applications of biology principles in computer science and engineering include
evolutionary algorithms (EAs), artificial neural networks (ANNs), cellular
automata, and swarm simulation. Especially EAs and ANNs have been successfully
employed in many areas. Examples include circuit board design, process control,
and optimization problems.

Scientists that want to study these biological processes also use these
bio-inspired systems as models they can learn from. These researchers are
naturally interested in making sure their models are true to nature --
i.e. that the models are biologically plausible. But what about computer
scientists and engineers: Should biological realism also be of importance to
those of us who use these techniques to solve computational problems? Can we
really expect to outsmart nature's design by deviating from biology?

## What is meant by bio-plausible?

We can look at bio-inspired methods as the superset and bio-plausible as a
smaller subset. The criterion for inclusion in the smaller group of
bio-plausible techniques is necessarily a fuzzy one. This is because the amount
an approach may deviate from biology while still being plausible is a
subjective matter, but also because our knowledge about biology is incomplete.

I have chosen to define the bio-plausibility of a technique as the extent to
which it adheres to the biological reality here on Earth as we understand it. I
am going to argue that we generally should not focus on bio-plausibility if we
are are implementing bio-inspired techniques on computers except in a limited
sense I will discuss near the end.

I will explore the topic by looking at example systems that use bio-inspired
techniques.

## Open-endedness

In his 1993 paper on artificial life [^2] Thomas S. Ray argues that we impose
limits on ourselves and our bio-inspired technology because we have only seen
the one instance of life we ourselves are apart of. The examples he use are
artificial life (AL) simulations and what he call AL instantiations.

In talking AL instantiation, Ray writes about how an open-ended process of
evolution in a computational medium will -- if we don't impose any non-inherent
limits -- adjust to the medium it is developed in. Ray argues that we should
view computer systems as alternate worlds with different rules governing their
“physical reality”. This idea is in many ways reminiscent of the search for
“alien biology”, i.e. life in other parts of the universe. While the laws of
physics are the same everywhere (as far as we know); life that has emerged
independently from the life on earth will most likely be very different -- even
if a similar process of evolution probably shaped it.

## Touching reality

The problem with a completely open-ended approach is that we often have an
explicit goal we want to achieve with our use of these techniques. If we are
optimizing a process control unit for use in a power plant we most certainly
want it to follow our rules of physics!

A good example of a technique which is first and foremost inspired by a
biological phenomenon is the CLONALG algorithm presented by de Castro and von
Zuben in [[^1]]. This algorithm draws inspiration from immunology, especially the
way the adaptive part of the vertebrate immune system learns to react to novel
threats by proliferating antibodies that have successfully matched an antigen --
in a process called clonal selection. CLONALG uses this concept by maintaining a
pool of pattern matchers and cloning the ones that perform well. The algorithm
has been successfully applied to optimization and pattern recognition tasks --
the latter application being highlighted by de Castro and von Zuben in their
paper.

de Castro and von Zuben emphasize that their algorithm does not accurately
model a biological immune system, but merely draws inspiration from its
high-level concepts. One factor they explicitly mention is left out cell--cell
interaction.

CLONALG is thus clearly a bio-*inspired* algorithm. The questions it then: Would
the algorithm perform better if more effort were put into making it more
bio-plausible? The authors does not explicitly discuss this, but I believe the
answer is no. When implementing an algorithm such as CLONALG for a specific
usage area, the programmer will need to make various decisions in order to make
the problem tractable. One will not be able to match the number of “cells”
found in the immune system nor the infinite parallelism resulting from these
cells' independence. However, this is not necessarily a weakness; by
familiarity with the problem, the programmer will be able to take the best
“shortcuts”. Will the program often see identical objects? “Cheat” by storing
the exact signatures. Will it rarely see the same item twice? Check if reducing
the importance of “memory” helps.

Evolutions has tuned nature's “algorithms” to their specific usage areas; if
one wants to do something different we should in many cases expect it to be
possible to get better results for different uses of the techniques.

## Robustness and Adaptivity

A beneficial property of bio-inspired approaches is that they are robust and
able to adapt to a changing environment. A system's robustness is defined by
Sipper in [[^3]] as its ability to function in the face of faults, while
adaptivity stems from the system's ability to learn, evolve or self-organize,
according to Sipper.

Tuning a technique to a specific area is often done at the expense of
robustness and adaptivity. This is also relevant for the discussion about
bio-plausibility vs bio-inspiration since one can argue that bio-plausible
techniques are closer to what has been shaped by evolution for millions of
years and are in that regard “proven” to work.

While this argument holds for problem areas where we are faced with noisy data
and the need to change over time, most areas does not have a high degree of
both of these factors. We are able to “outsmart” nature, but only because we
pick our own battles, and -- as Ray could have said: because a computer program
does not occupy the same universe as biological organisms.

## Science vs Technology

I have argued for a pragmatic, problem-solving -- perhaps even
engineering-centred -- approach where we in my opinion should not focus on
whether our algorithms and techniques are bio-plausible or not. However,
bio-plausibility can be very useful in an indirect -- but very important --
sense.

In talking about CLONALG I mentioned that many areas of the immune system are
not well understood and it is not unlikely that learning more about immunology
will give us new insights and inspire new technology. In studies of the immune
system and other biological processes bio-plausible models can be of great help
in understanding these systems. First as simplified models and perhaps later as
more complete simulations of the process being studied.

Artificial development, as described in [[^4]], is an example of a field
which has not yet had its breakthrough -- at least in terms of being used to
solve problems in practice. I believe that bio-plausibility is also a good
“default” for exploring fields that are new or that are not yet well
understood.

Insights into new areas will shed light on new inventions in nature and -- just
as today -- people will be inspired to create technology exploiting these
ideas, continuing the interplay between science and technology.

[^1]: L.N. de Castro and F.J. Von Zuben. Learning and optimization using the clonal selection principle. *IEEE Transactions on Evolutionary Computation*, 6(3):239–251, June 2002.
[^2]: Thomas S. Ray. An Evolutionary Approach to Synthetic Biology: Zen and the Art of Creating Life. *Artificial Life*, 1(1):195–226, 1994.
[^3]: Moshe Sipper. The emergence of cellular computing. *Computer*, 32(7):18–26, 2002.
[^4]: Gunnar Tufte. From Evo to EvoDevo: Mapping and Adaptation in Artificial Development. In *Evolutionary Computation*. 2009.
