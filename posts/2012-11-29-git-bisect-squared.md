---
title: git bisect squared
description: Using git to bisect the bad commit found with “git bisect”
tags: git
...

`git bisect` is a really powerful tool to find the first commit that introduced
a regression. By supplying a know good point (e.g. “two weeks ago”) and a known
bad commit (usually `HEAD`), it will do a binary search by asking you to test
the midpoint or by running a script you provide.

Today was one of the days I got to use `git bisect`. We started seeing an error
being written to the console -- nothing serious, but we knew it wasn't there
last week. After testing six commits the culprit was found. I ran `git show` to
look at the commit expecting to be able to quickly see the mistake. However, the
commit message for this commit was "Miscellaneous", and then a list of
(unrelated) changes that were done based on feedback for a code review [^1].

Right. So this was a hodgepodge of mostly unrelated changes and it was not
apparent what could cause the debug message we saw. I started to wish it was
possible to bisect the changes of a single commit.

Then, suddenly, an epiphany! By using `git add -p` to add one set of changes at
a time, and then using `git stash --keep-index` I could easily run the test
after adding one and one change [^2]. The bug -- which was quite subtle -- was
quickly found. (Of course, if the regression was caused by changes that
interacted, it would not have been this easy, but this was not one of these
days. :)

[^1]: I do *not* approve of this practice. :-)
[^2]: The entire workflow is 1) `git add -p`, 2) `git stash --keep-index`, 3) run the
test, 4) `git stash pop`, 5) `goto 1`
