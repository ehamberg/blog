---
title: git tip: avoid committing your password
description: Avoid committing your password by accident
tags: git
...

If you're working on a project where you have config files with passwords in
them, for example web apps with database passwords it is a bit risky to rely on
always being vigilant and avoiding staging that hunk when building a commit.

Instead, you can tell git to assume that a file is unchanged, which greatly
lowers the risk of pushing your password to github:

    $ git update-index --assume-unchanged config/secretpasswords.conf

If you often change that file, you would have to call `update-index` with
`--no-assume-unchanged` before (carefully) selecting hunks for a commit, but
since config files rarely change, there are no real downside to doing this.

This is how it will look:

    $ git diff
    diff --git i/config/postgresql.yml w/config/postgresql.yml
    index 3a0df29..0d9ae25 100644
    --- i/config/postgresql.yml
    +++ w/config/postgresql.yml
    @@ -1,6 +1,6 @@
     Default: &defaults
       user: rssqueue
    -  password: rssqueue
    +  password: sup3r53cr17
       host: localhost
       port: 5432
       database: rssqueue
    $ git update-index --assume-unchanged config/postgresql.yml
    $ git diff
    $ git st
    $
