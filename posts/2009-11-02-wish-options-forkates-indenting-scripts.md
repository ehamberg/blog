---
title: Wish: Options for Kate's Indenting Scripts
description: A wish for easier manipulation of indentation options in Kate
tags: kate, kde
---

At the start of [Kate's indenting script for C/C++](http://websvn.kde.org/trunk/KDE/kdelibs/kate/script/data/cstyle.js?view=markup), the following options are available:

~~~{.javascript}
// BEGIN USER CONFIGURATION
var cfgIndentCase = true;         // indent 'case' and 'default' in a switch?
var cfgIndentNamespace = true;    // indent after 'namespace'?
var cfgAutoInsertStar = false;    // auto insert '*' in C-comments
var cfgSnapSlash = false;         // snap '/' to '*/' in C-comments
var cfgAutoInsertSlashes = false; // auto insert '//' after C++-comments
// END USER CONFIGURATION
~~~

To set these options one would first have to actually find the indenting scripts
under `/usr/share`, then copy it to one's home directory to be able to modify
it, and then modify the javascript source. This could -- of course -- be done in a
much better way. In an ideal world options like these should be available in
Kate's settings as check boxes. It should also be possible to have non-boolean
options, like choosing a value from a list of possible values for a setting.

I would love to see something like the following:

    registerSetting("Indent 'case' and 'default' in switch statements", "cfgIndentCase",  "boolean");

produce the following tickbox:

	☐ Indent 'case' and 'default' in switch statements

in Kate's settings.

I don't have the time myself, but I don't think it would be very hard, and I'm
sure that many people would be a bit happier. Me and the other Kate developers
would be glad to assist. :-)
