# Projects

Here are some of the projects I work on. A more comprehensive list is available
on my [github accout](https://github.com/ehamberg/).

## *How Much?*: A smart, offline currency converter

A smart currency converter for iPhone/Apple watch that knows where you are and
will convert the currency of the country you're vising to your “home currency”
without you having to configure anything. I have written a [blog
post](https://hamberg.no/erlend/posts/2015-10-22-geocoding.html) about the
details of offline, reverse geocoding done by the app.

![How Much? screenshot](images/howmuch.png)

<div style="text-align: center; margin-top: 1em;">
[![](images/appstore.png)](https://geo.itunes.apple.com/us/app/how-much-offline-currency/id999568678?mt=8)
</div>

- [Link](https://gethowmuch.com/)
- [*Lobsters* discussion](https://lobste.rs/s/notli7/how_much_the_smart_offline_currency_converter_for_ios_and_watchos)

## 9m.no

9m.no is a URL^[Technically:
[IRI](https://en.wikipedia.org/wiki/Internationalized_resource_identifier)]
shortener that uses Unicode symbols to get really, short URLs. E.g.
<http://9m.no/ብ易> → <http://reddit.com>. It was a quick hack and meant to be
a (half) joke, but people really seem to like it.

- [Link](http://9m.no)
- [Code](https://github.com/ehamberg/9m)
- [*Hacker News* discussion](https://news.ycombinator.com/item?id=7783239)

## ~~RSSQueue.com~~

~~RSSQueue is a service that lets you create and curate RSS/podcast feeds that can
be synced to tablets or phones. This lets you create a “watch it later” list
with content that is automatically downloaded, even to iOS devices where one
usually cannot simply download e.g. a video file for later watching.~~

- [~~Link~~](http://rssqueue.com)
- [Code](https://github.com/ehamberg/rssqueue)

## Circus Escape

A simple iPhone platform game where the aim is to get as far as possible on a
unicycle while avoiding obstacles.

![Circus Escape screenshot](images/circusescape.png)

<div style="text-align: center; margin-top: 1em;">
[![](images/appstore.png)](http://itunes.apple.com/us/app/circus-escape/id554468356)
</div>

## vim-cute-python

A python syntax file for Vim that uses the new "conceal" feature in Vim 7.3 to
display unicode characters for some Python operators and built-in functions,
turning the following:

~~~{.python}
    map (lambda x: x, [1,2,3])

    def foo(e, a):
        if e in [1,2,3] and not a:
            return math.sqrt(math.pi)
        else:
            return sum([1,2,3])
~~~

into

~~~{.python}
    map (λ x: x, [1,2,3])

    def foo(e, a):
        if e ∈ [1,2,3] ∧ ¬a:
            return √(π)
        else:
            return ∑([1,2,3])
~~~

- [Code](https://github.com/ehamberg/vim-cute-python)

## SimpleEA

A simple framework for writing evolutionary algorithms in Haskell.

- [Code](https://github.com/ehamberg/simpleea)
- [Documentation](http://hackage.haskell.org/package/SimpleEA)

## Guide: GTD in 15 minutes

A pragmatic guide to the "Getting Things Done" framework. Written in Markdown.

- [GTD in 15 Minutes](http://hamberg.no/gtd)
- [Code](https://github.com/ehamberg/gtdguide)

## Vi input mode for the Kate editor

A vim-like, modal input mode for the Kate editor part used in the [Kate
editor](http://kate-editor.org/), [Kdevelop](http://kdevelop.org), the [Kile
LaTeX editor](http://kile.sourceforge.net/) and various other programs.

This started as a [Google Summer of Code](http://code.google.com/soc/) project
in 2008 and I have maintained it since. I mentored Svyatoslav Kuzmich that
worked on the project as a GSoC student in 2011 and Vegard Øye who worked on it
in 2012.

- [The Kate editor](http://kate-editor.org/)
- [Various blog posts about my work on Kate](/tags/kate.html)

## E-book version of the *Typeclassopedia*

A [Pandoc Markdown](http://johnmacfarlane.net/pandoc/) version of Brent Yorgey's
[Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia), allowing
one to create an EPUB version. Having a “real” e-book means that you can
comfortably read it on an e-book reader and highlight text and take notes while
reading.

The EPUB file can be downloaded from Github:

<https://github.com/ehamberg/typeclassopedia-md/releases>

The Markdown source is also available in that repo and you can of course
use Pandoc to convert the Markdown file to all the other output formats
Pandoc supports.

By using a program like [Calibre](http://calibre-ebook.com), the EPUB file can
be converted to other e-book formats such as the Kindle format.
