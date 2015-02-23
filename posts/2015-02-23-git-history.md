---
title: On Git History – or, “The Case for Merge Commits”
description: Merge commits add extra information, and extra information is most often a good thing
tags: git
...

(I keep having to argue the case for merge commits in git, so this is a brief
summary of my position I can point people to.)

> tl;dr: **Never remove information!** Always merge branches with `git merge
--no-ff` to get a merge commit, and keep the "Conflicts" lines added by git.
Only fast forward when pulling in remote changes to a branch with no local
commits.

I don't think this is very controversial anymore, but I'll start by presenting
the case for merge commits anyway.

## A new feature is born

So, you want to implement a new feature and you branch off `master` at the
point *b*, write the code, and end up with commits *x*, *y* and *z* (possibly
after first having cleaned up your feature branch). Meanwhile, a new commit,
*c*, has been pushed to the master branch:

    [master]  a-b-c
                \
    [feature]    x-y-z

You now have two main options: You can *rebase* your feature branch on top of
the current `master`:

    [master]  a-b-c-x-y-z

or *merge* your feature branch into `master` with a merge commit *m*:

    [master]  a-b-c----m
                \     /
    [feature]    x-y-z

The first option -- to *rebase* and get a linear history -- is for some reason
very appealing to some people that consider this to be a “cleaner” history, but
in reality you're just *losing* history. I would even consider this to be
a slight lie, since the environment where you tested commits *x*, *y* and *z*
is not the same environment where the commits ended up, with no traces of
a merge having taken place.

> Now, ideally, *real* tests should uncover bugs anyway, but you probably did
informal testing all the time when writing the code ("Does this work? Ah, of
course not, that argument needs to be in milliseconds!"), and if anything
changed on the master branch in the meantime, assumptions you made while
developing may no longer hold. In those cases, when digging through the git
history, seeing a merge commit is very valuable information and will probably
lead to closer scrutiny if it seems at all relevant to the issue under
investigation.

When git creates a merge commit it will also by default append a list of files
that had conflicts to the commit message:^[Actually, since git 2.3.0, these
lines are commented out by default, and you have to explicitly un-comment them
to keep them.]

    Conflicts:
        src/foo-service.c
        src/bar-client.c

*Do not remove these lines!* This is also a piece of useful information for our
when poring over the history to find out wtf is wrong. Even if your merge
conflict was trivial, there is always a non-zero chance of introducing a bug
when resolving a conflict, and seeing those lines in the merge commit message
could be valuable information. They are basically a hint saying “Still confused?
Maybe you should be extra careful when reviewing the changes in these files”.

## An alternate history

Okay, fine, you say; merge commits make sense when you actually *merge* two or
more commits, but what if the history was completely linear? Without commit *c*
our history would be:

    [master]  a-b
                \
    [feature]    x-y-z

If you now merge your feature branch into `master`, git will by default do
a *fast-forward*, simply changing `master` to point to *z*:

    $ git merge feature
    Updating 4fe28cd..68a8d69
    Fast-forward

We then still have the same git history as above with no new commits:

    [master,feature]  a-b-x-y-z

In this case it's not as easy as above to argue for introducing a merge commit
(as mentioned, git's default behaviour is even to just fast-forward), but
except for cases where the feature branch ends up just having one commit, you
*should* still create a merge commit, by using `git merge --no-ff feature`,
giving us this history:

    [master]  a-b------m
                \     /
    [feature]    x-y-z

This introduction of a merge commit allows you to write a summary of the
changes in the branch you're merging, and allows people reading the history in
the future to choose to view the merge as just one commit, or -- if they choose
to -- to dive into the commits that compromise the feature that was merged.
Again, this is an example of having more information available.^[As a bonus, it
also makes it easy to revert the introduction of a feature without the need to
carefully study the history around the point where it was introduced.] Which is
almost always a good thing.

## Howto

Ok, so how do you get git to behave like this? Well, there is a config option
`merge.ff`^[`$ git config --global merge.ff false`] that allows you to specify
that merges should never be a fast-forward, but this is probably *not* what you
want since this means that every time you `pull` new commits from a remote, you
will introduce a merge commit.^[Remember that `pull` = `fetch + merge`]
Instead, you should *stop and think* for a second when merging a branch. If you
want to introduce a merge commit (you probably do), you should pass the
`--no-ff` flag to `merge`.

## Postscript

In reality, a long lived feature branch will often look more like the
following, having merged in the `master` branch at various points to keep up
with changes:

    [master]  a-b-c-d-e-f-g-h----m
                \     \     \   /
    [feature]    u-v-w-m-x-y-m-z
