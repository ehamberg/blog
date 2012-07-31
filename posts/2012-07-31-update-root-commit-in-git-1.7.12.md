---
title: git 1.7.12 Will Make it Easy to Update First Commit
description: “git rebase -i --root” allows changing the repo's first commit
tags: git
...

Have you ever had the need to change the very first commit in a git repository?
If you have you will know that it is far from trivial. There are
[several](http://stackoverflow.com/questions/2119480/changing-the-message-of-the-first-commit-git)
[ways](http://stackoverflow.com/questions/2246208/change-first-commit-of-project-with-git)
you can do it, but it's not trivial to remember and I usually just google one of
these recipes when I need to do this.

Another trick employed by some is to start new repositories with an empty commit
so that they can use interactive rebasing to rewrite down to (and including) the
first *real* commit:

    $ git commit --allow-empty -m 'Initial empty commit'

While this isn't a very big problem -- nor one that happen very often -- it is
still a bit annoying that it is so difficult to do this. However, ccording to
the changelog for the [rc1 release of git
1.7.12](http://git-blame.blogspot.co.uk/2012/07/git-1712-rc1.html), these tricks
will finally be made obsolete with the new `--root` switch:

    $ git --rebase -i --root

This will allow you to correct that annoying typo in your "new amazing porject"
before you push it to github for the world to see.
