---
title: Google Calendar Plasmoid
description: A quick python hack
tags: kde, plasma, programming, python
---

In case anyone is interested:

I made a simple Plasmoid for viewing coming events from Google Calendar. It's
simply a webview showing the iPhone version of Google Calendar.  It's written in
Python and took 10 minutes to write (27 lines of code) by following [the
excellent tutorial on
techbase](http://techbase.kde.org/Development/Tutorials/Plasma/Python/GettingStarted).  

![Google Calendar Plasmoid](/images/gcal_plasmoid.png)

You can download it here:
[http://hamberg.no/erlend/files/gcal.zip](http://hamberg.no/erlend/files/gcal.zip).
If you improve it or make another Google Calendar plasmoid: let me know! :)

Oh, btw. To install it run `plasmapkg -i gcal.zip`.

P.S: openSuse users: I had to install `python-dev` to actually run a python plasmoid. Don't know why.
