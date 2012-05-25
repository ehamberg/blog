---
title: Photo-blogging before “smartphones”
description: How photo blogging was done in ye olden days
tags: programming, perl, blogging
---

Rummaging through an old Unix account I
found the following Perl script, simply named “test.pl”. What could it be? It
hasn’t been touched for over five years:

    -rw-r--r-- 1 hamberg fidi_s 870 2005-09-17 04:50 test.pl

Reading the code it becomes apparent that it processes text and extracts base 64
encoded data…

~~~{.perl}
#!/usr/bin/perl

use MIME::Base64;
use Encode 'from_to';

open(MBOX, $ARGV[0]);
@mailbox = <MBOX>;

$b = join('', @mailbox);
@a = split(/\nFrom /, $b);

foreach $mail (@a) {
    if ($mail =~ m/Subject: .*Foto/) {

        $mail =~ m/Content-Type: TEXT\/PLAIN;.*BASE64\n\n(.*)----_.*Content-Disposition: attachment;\n.*?filename="(.*?)"\n.*?\n\n(.*)----_/s; 

        $c = join('', $3);

        $file = "/tmp/$2";
        open(FILE, ">$file");
        print FILE decode_base64($c);
        close(FILE);

        $text = decode_base64($1);
        print "$text\n";

        print length($text);

        print "\n";
    }
}
close(MBOX);
~~~

… which made me remember that I used this as an email filter to extract images
from MMS messages sent to myself from my mobile phone. Whenever I sent a
formatted email to myself it would extract the image and text and make a blog
post. I remember that was quite cool back then. :-)
