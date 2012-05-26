---
title: Code Reading as a Team Activity
description: Could code reading help keep programmers honest?
tags: programming
---

I am currently reading ["Coders at Work"](http://www.codersatwork.com/) by Peter
Seibel, a really interesting read so far. The book is full of good tips and
experiences from actual coders who learnt by doing. Peter Seibel's questions are
really good -- no doubt because he himself is a programmer -- and his interview
subject have different and interesting stories to tell.

!["The proof is trivial! Just biject it to a compact poset whose elements are thrice-differentiable DAGs"](/images/monkey_tutor.png)

One of the things I found really interesting was how [Douglas
Crockford](http://en.wikipedia.org/wiki/Douglas_Crockford) recommended *code
reading* as a way to avoid having confused programmers in a project that aren't
aware of their own situation before it's too late. According to Crockford it
also helps by making sure that the really good programmers mentor other people
on their team. The procedure is very simple:

> **Seibel**: Can you talk a bit about how you conduct a code reading?
>
> **Crockford**: At each meeting, someone's responsible for reading their code,
> and they'll walk us through everything, and the rest of us will observe.  It's
> a really good chance for the rest of the team to understand how their stuff is
> going to have to fit with that stuff.
>
> We get everybody around the table; everybody gets a stack of paper. We also
> blow it up on the screen. And we all read through it together. And we're all
> commenting on the code as we go along. People say, "I don't understand this
> comment", or, "This comment doesn't seem to describe the code." That kind of
> stuff can be so valuable because as a programmer you stop reading your own
> comments and you're not aware that you're misdirecting the reader. Having the
> people you work with helping to keep you code clean is a huge service—you find
> defects that you never would have found on your own.
>
> I think an hour of code reading is worth two weeks of QA. It's just a really
> effective way of removing errors. If you have someone who is strong reading,
> then the novices around them are going to learn a lot that they wouldn't be
> learning otherwise, and if you have a novice reading, he's going to get a lot
> of really good advice.
>
> [...]
>
> For one thing it makes it easier to track the project, because we can actually
> see what progress people are making. And we can see much sooner if they're
> going off the rails or not.

I simply loved this idea and would love to test it in practice. I know that I
personally would find it highly motivating, both with respect to the quality of
my own code and for the project as a whole by so explicitly seeing what the
others are doing.

This is probably not going to work for all projects and teams, but for a
relatively small team (working at the same location) aiming to produce code of a
high quality I really believe this approach can work.

If anyone has experience with this, or similar approaches, I would love to hear
your thoughts.
