---
title:  Make all mac apps available from the command line
description: A zsh script for making all the apps in /Applications available from the command line
tags: zsh, osx
...

After having written `open -a /Applications/Pixelmator.app logo.png` ten times too many, I decided that it would be great to always have all my installed Mac application bundles available on the command line so that I'll have commands like `pixelmator`, `blender`, `marked`, etc. available.

The following zsh snippet will do just that: For every `.app` bundle in `/Applicatons` and `$HOME/Applications` it will create a shell function that launches the app with the given files as arguments.

```bash
# make all apps in /Applications available from the command line
for a in {$HOME,}/Applications/*.app(N) ; do
    eval "\${\${a:t:l:r}//[ -]/}() {\
        if (( \$# == 0 )); then\
            open ${(qq)a};\
        else\
            open -a ${(qq)a} \$@;\
        fi\
    }"
done
```

The `${${a:t:r:l}//[ -]/}` part will turn a string like `/Applications/Xcode51-Beta5.app` into `xcode51beta5` by removing the directory part (`:t`), removing the suffix (`:r`), lowercasing it (`:l`) and then finally removing dashes (`//[ -]//`). `${(qq)a}` will quote the app path.

The generated functions will look like this:

    $ which pixelmator
    pixelmator () {
    	if (( $# == 0 ))
    	then
    		open '/Applications/Pixelmator.app'
    	else
    		open -a '/Applications/Pixelmator.app' $@
    	fi
    }

(If someone creates a bash version, I'd be happy to add that too.)
