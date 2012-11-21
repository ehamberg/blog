---
title: handlerToIO: use forkIO in Yesod handlers
description: The newly added handlerToIO function makes it easy to use forkIO and still be able to run GHandler actions
tags: yesod, haskell
---

Two days ago in this
[patch](https://github.com/yesodweb/yesod/commit/244eb88f3630c16c3db41ac942c1a8927940cfe1),
the
[`handlerToIO`](http://hackage.haskell.org/packages/archive/yesod-core/1.1.1/doc/html/Yesod-Handler.html#g:16)
function was added to Yesod. Version 1.1.1 which was just released contains this
new function.

`handlerToIO` allows you to `GHandler` actions inside `IO`.  A neat use case for
this is to use `forkIO` to fork a thread in a response handler and still be able
to run database actions with `runDB`:

```haskell
postFooR :: Identifier → Handler RepJson
postFooR id = do
    Just (Entity key _) <- runDB $ getBy $ UniqueIdentifier id

    runInnerHandler <- handlerToIO
    liftIO $ forkIO $ runInnerHandler $ do
        result <- liftIO $ actionTakingALongTime
        runDB $ insert $ ResultItem key result
    jsonToRepJson $ String ""
```
