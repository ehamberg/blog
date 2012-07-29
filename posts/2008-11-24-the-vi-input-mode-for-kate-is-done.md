---
title: The Vi input mode for Kate is “done”
description: The original goals set for Kate's Vi input mode are now completed
tags: gsoc, kate, programming
---

<div style="border: 1px solid black; margin: 0pt auto -1px; padding: 1em; background-color: #eeeeee; width: 50%; text-align: center;">Please see [http://kate-editor.org/kate-vi-mode/](http://kate-editor.org/kate-vi-mode/) for an updated overview of the Kate VI mode project.</div>

I just marked the Vi input mode ("V.I.M."? :) ) for the Kate kpart as done in
the feature plan for [KDE
4.2](http://techbase.kde.org/Schedules/KDE4/4.2_Feature_Plan). It feels a bit
weird to mark it as done, though, as there are tonnes of things I want to
implement after KDE 4.2.

As you may know this started as a [Google Summer of Code
project](http://code.google.com/soc/2008/kde/appinfo.html?csaid=370F903C96FF67D1)
and I'm quite happy with the way things have turned out. Since we are in feature
freeze I would like to present the features that will be in the Vi input mode
for the Kate part in KDE 4.2.

![Kate Vi Input Mode in action -- visual mode selection](/images/vimode1.png)

The Vi input mode can be used as the default editing mode by selecting it in
`Settings` → `Configure Kate...` → `Editing` → `Vi Input Mode` and/or toggled
any time by selecting ~~`View`~~`Edit` → `Vi Input Mode`. Nifty, eh?

![Vi Input Mode toggle](/images/vimode2.png)

Thanks to [Dmitry Suzdalev](http://dimsuz.wordpress.com/) there is also an
(optional) vi mode status bar. This shows commands while they are being entered,
output from commands like `ga` (the character code of the character under the
cursor) and of coruse the current mode.

![Kate Vi Input mode status bar -- "a4d has been pressed](/images/vimode3.png)

Quite a few of the vi commands collide with "regular" Kate shortcuts, like
**ctrl+b**/**ctrl+f** for scrolling up/down one page. If you want to use these
as vi commands, you can choose to let Vi commands override Kate shortcuts in the
Vi input mode settings. As you might know, **ctrl+r** is used for "redo" in vim.
To make it possible to redo for people not wanting to override Kate's shortcuts
**shift+u** is also used for undo. (This key press is used for "undoing all
latest changes on one line" in Vim.)

I have also added support for ranges to Kate's command line commands. So you can
do **1,10kill-line** ("kill-line" is a Kate command, not a vi(m) command). This
also works for the substitute command which is already a part of Kate:
**s/foo/bar/g**. This is something whose potential hopefully will be fully
exploited in the future when more commands are added.

For now, the only "ex commands" supported are **:w** and **:hardcopy**. Part of
the reason not more commands are supported is because I need to figure out how
to send requests from the editor part to the hosting application requesting to
e.g. quit or close a buffer. I also want to be able to add several forms of
commands like **:d[elete]**, **:w[rite]** and so on. (But only the long form
should be shown in the autocompletion to not clutter the dropdown with confusing
one-letter commands.)

But, enough of what's not supported! Here is the list of the commands that
actually made it into the Vi Input Mode before the feature freeze:

### Supported normal/visual mode commands

Keypress(es)             Action
------------------       ---------------------------------
  `a`                    `commandEnterInsertModeAppend`
  `A`                    `commandEnterInsertModeAppendEOL`
  `i`                    `commandEnterInsertMode`
  `v`                    `commandEnterVisualMode`
  `V`                    `commandEnterVisualLineMode`
  `o`                    `commandOpenNewLineUnder`
  `O`                    `commandOpenNewLineOver`
  `J`                    `commandJoinLines`
  `c`                    `commandChange`
  `C`                    `commandChangeToEOL`
  `cc`                   `commandChangeLine`
  `s`                    `commandSubstituteChar`
  `S`                    `commandSubstituteLine`
  `dd`                   `commandDeleteLine`
  `d`                    `commandDelete`
  `D`                    `commandDeleteToEOL`
  `x`                    `commandDeleteChar`
  `X`                    `commandDeleteCharBackward`
  `gu`                   `commandMakeLowercase`
  `guu`                  `commandMakeLowercaseLine`
  `gU`                   `commandMakeUppercase`
  `gUU`                  `commandMakeUppercaseLine`
  `y`                    `commandYank`
  `yy`                   `commandYankLine`
  `Y`                    `commandYankToEOL`
  `p`                    `commandPaste`
  `P`                    `commandPasteBefore`
  `r`                    `commandReplaceCharacter`
  `:`                    `commandSwitchToCmdLine`
  `/`                    `commandSearch`
  `u`                    `commandUndo)`
  `<c-r>`                `commandRedo`
  `U`                    `commandRedo`
  `m.`                   `commandSetMark`
  `n`                    `commandFindNext`
  `N`                    `commandFindPrev`
  `>>`                   `commandIndentLine`
  `<<`                   `commandUnindentLine`
  `>`                    `commandIndentLines`
  `<`                    `commandUnindentLines`
  `<c-f>`                `commandScrollPageDown`
  `<c-b>`                `commandScrollPageUp`
  `ga`                   `commandPrintCharacterCode`
  `.`                    `commandRepeatLastChange`
  `==`                   `commandAlignLine`
  `=`                    `commandAlignLines`
  `~`                    `commandChangeCase`

### Supported motions

Keypress(es)             Action
------------------       ---------------------------------
  `h`                    `motionLeft`
  `<left>`               `motionLeft`
  `<backspace>`          `motionLeft`
  `j`                    `motionDown`
  `<down>`               `motionDown`
  `k`                    `motionUp`
  `<up>`                 `motionUp`
  `l`                    `motionRight`
  `<right>`              `motionRight`
  `<space>`              `motionRight`
  `$`                    `motionToEOL`
  `<end>`                `motionToEOL`
  `0`                    `motionToColumn0`
  `<home>`               `motionToColumn0`
  `^`                    `motionToFirstCharacterOfLine`
  `f`                    `motionFindChar`
  `F`                    `motionFindCharBackward`
  `t`                    `motionToChar`
  `T`                    `motionToCharBackward`
  `gg`                   `motionToLineFirst`
  `G`                    `motionToLineLast`
  `w`                    `motionWordForward`
  `W`                    `motionWORDForward`
  `b`                    `motionWordBackward`
  `B`                    `motionWORDBackward`
  `e`                    `motionToEndOfWord`
  `E`                    `motionToEndOfWORD`
  `ge`                   `motionToEndOfPrevWord`
  `gE`                   `motionToEndOfPrevWORD`
  `|`                    `motionToScreenColumn`
  `%`                    `motionToMatchingItem`
  `                      `motionToMark`
  `'`                    `motionToMarkLine`
  `[[`                   `motionToPreviousBraceBlockStart`
  `]]`                   `motionToNextBraceBlockStart`
  `[]`                   `motionToPreviousBraceBlockEnd`
  `][`                   `motionToNextBraceBlockEnd`

### Supported text objects

Keypress(es)             Action
------------------       ---------------------------------
  `iw`                   `textObjectInnerWord`
  `aw`                   `textObjectAWord`
  `i"`                   `textObjectInnerQuoteDouble`
  `a"`                   `textObjectAQuoteDouble`
  `i'`                   `textObjectInnerQuoteSingle`
  `a'`                   `textObjectAQuoteSingle`
  `i(`                   `textObjectInnerParen`
  `a(`                   `textObjectAParen`
  `i[`                   `textObjectInnerBracket`
  `a[`                   `textObjectABracket`

### Supported insert mode commands

Keypress(es)             Action
------------------       ---------------------------------
  `<c-d>`                `commandUnindent`
  `<c-t>`                `commandIndent`
  `<c-e>`                `commandInsertFromBelow`
  `<c-y>`                `commandInsertFromAbove`
  `<c-w>`                `commandDeleteWord`
  `<c-home>`             `commandToFirstCharacterInFile`

### The missing pieces

The aim of Kate's Vi input mode is--as I have said before--is not to replace vim's
functionality, but to have a editor kpart that is comfortable to use for us
Vi(m)-heads. There are however some things I feel is still missing and would
like to implement after KDE 4.2 (some of them mentioned above):

- Proper command mode ("ex commands") support (very few commands are supported as of now)
- Saving of registers and marks between editing sessions
- "Replace mode" (`shift+r` in vim)
- Integrating the Vi input mode marks with Kate's bookmarks system
- Making it possible to use registers in ranges and add support for the the `'<` and `'>` registers

### Please test!

I will be studying for my exams until 19 December so I won't have much time for
coding, but I would still like people to test the Vi Input Mode and report bugs
so I have a better chance of fixing them before the end of January and the 4.2
release.
