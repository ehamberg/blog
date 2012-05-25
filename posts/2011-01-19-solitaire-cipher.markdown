---
title: Solitaire Cipher
description: Implementation of the Solitaire cipher from "Cryptonomicon"
tags: crypto, haskell, programming
---

I finally read [Neal Stephenson's book
*Cryptonomicon*](http://http://en.wikipedia.org/wiki/Cryptonomicon) this summer
which -- besides being a great read -- introduced an interesting cipher called
*Pontifex*. This cipher is based on the [Solitaire
cipher](http://www.schneier.com/solitaire.html) by Bruce Schneier and the idea
is that one simply needs a deck of card in order to communicate securely if a
list of keys has been exchanged. When seeing that yesterday's challenge on
Programming Praxis was to implement this cipher I simply *had* to do it, and
here's the resulting implementation, in Haskell, of the Solitaire cipher:

<script src="https://gist.github.com/785960.js"> </script>
