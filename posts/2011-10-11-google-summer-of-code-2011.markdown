---
title: Google Summer of Code 2011: A summary
description: Experiences from being a GSoC mentor
tags: gsoc, kate, vim
---

Google summer of a code is an annual programme where Google pays students to
work on open source projects in the summer months. The students have one or more
mentors from the project who help them get started and function as a contact
point for the developer community.

In 2008 I did a Google Summer of Code project for the [Kate text
editor](http://kate-editor.org) where I wrote a [Vi Input
Mode](http://kate-editor.org/kate-vi-mode/) -- making it possible to use Kate in
a modal, Vi[m]-like manner. The project was successful and I continued to
maintain this code and added more features over the years. This summer I took
the step of signing up as a mentor myself and put out a project proposal for
further improving the Vi Mode in Kate and adding a good unit test framework for
the code. To my surprise – and delight! – many students were interested in the
project and we ended up getting quite a few applications from students wanting
to work on “my” project. In the end the Russian computer science student
 Святослав Кузьмич (Svyatoslav Kuzmich) from Moscow Institute of Physics and
Technology was chosen.

![The Kate Editor](/images/advanced.png)

Over the summer he wrote a test framework for the Vi Mode and wrote test for the
functionality already present. Having an easy way of adding tests for the Vi
mode had been on my wish list for a long time and it was really nice to see it
implemented. It did not take long for the new test system to show its value:
Svyatoslav found some corner case bugs that had probably been in the code for a
long time and fixed them. A short overview of the new test framework can be
found at the Kate blog: [Kate Vi Mode Test
Suite](http://kate-editor.org/2011/08/07/vitest/).

Svyatoslav also made many more visible improvements to the Vi Mode such as jump
lists (making it possible to jump back/forward to where you were in the text),
making it possible to control sub windows with the keyboard, many new command
line mode commands and much more. An overview can be found in his [blog post at
kate-editor.org](http://kate-editor.org/2011/07/26/vimode-gsoc2011/).

For me personally it was an interesting experience to act as a mentor for
another programmer. My function also changed as the project progressed. In the
beginning I tried to give Svyatoslav an overview of the current code and answer
some questions as they came up. The programming work itself was rarely a theme –
Svyatoslav was already a really good coder and generally just needed to be
pointed in the right direction or – even more commonly – just get a confirming
nod that his suggested solution sounded good. Since I had a few years to think
about many of the features he wanted to implement I also had a few “that
*does* sound like a good idea, but...” often learned the hard way. :-)

In the end the project turned out very well and the users will get many new
features -- some of which have been wished for for years.

As a wrap-up of Google Summer of Code 2011 I am going to San Francisco next week
to take part in the GSoC 2011 mentor summit where mentors from the organizations
taking part in the Summer of Code meet to exchange experiences and discuss
future directions for the programme. I am really looking forward to that! :-)
