---
title: Google Calendar Plasmoid, Take Two
description: An improved version of my Google Calendar plasmoid
tags: kde, plasma, programming, python
---

After the positive feedback on my [quick 27 line python hack](http://hamberg.no/erlend/2009/05/05/google-calendar-plasmoid/), I decided to make a slightly improved version. This fixes the two major problems with the old one: you can now use your Google Apps for your Domain email and you can use kwallet to remember your login.

![](/images/gcal2.png)

![](/images/gcal2_settings.png)

It's not a simple, 27 line python script any more, but now it's much more usable. (unless there are some stupid bugs I have overlooked. :))

As suggested I uploaded it to [KDE
Look](http://www.kde-look.org/content/show.php?content=104182), so you can
install it from the "Add widgets..." dialogue or download it and install it with
**plasmapkg -i gcal-1.1.plasmoid**.

> (It looks like installing it from GHNS still downloads the old version. This
> will hopefully change. For now, please download and install manually.)

The login is just a quick hack, btw. This pretty much sums it up:

~~~{.pythong}
src = src.replace("id="Email"",
    "id="Email" value=""+self.settings['username']+""")
src = src.replace("id="Passwd"",
    "id="Passwd" value=""+self.settings['password']+""")
src = src.replace("id="gaia_loginform"",
    "id="gaia_loginform" name="gaia_loginform"")
src = src.replace("",
src = src.replace("</body>",
    "<script>document.gaia_loginform.submit()</script></body>")
~~~

I also tried the ninja trick from [Ariya's
blog](http://ariya.blogspot.com/2009/04/transparent-qwebview-and-qwebpage.html)
to get a transparent webview, but Google's 1995-style gif logo pretty much
destroyed the appearance, so I dropped that. :)

![](/images/gcal2_transparent.png)
