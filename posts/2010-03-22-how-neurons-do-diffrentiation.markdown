---
title: How Neurons do Differentiation
description: How temporal inhibition can be used to create "neuro calculators"
tags: biology, neural networks, ai
---

**Today I learned...**

...how neural networks (think brains) can do differentiation by using temporal
inhibition -- i.e. by using a delayed signal. In the figure below, the node
**α** will send a signal to two nodes. One of them -- **β** -- will pass on an
inhibitory signal of the same strength as its input signal, but with a delay.
Thus, when **β**'s signal gets sent to the final node, **α** will at the same
time be sending its “next” output signal to the final node.

!["Differentiation" by using delayed inhibition. Solid lines indicate excitatory signals and the dotted line an inhibitory signal.](/images/tdifferentiation.png)

Therefore, the final node will receive two signals: the current output of **α** and the inverted previous output of **α**. If the final node sums these together its output will therefore be **α**'s current value minus its old value -- i.e. positive if **α**'s output signal is increasing and negative if it is decreasing. Simple and beautiful!
