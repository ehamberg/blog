{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll

main :: IO ()
main = hakyllWith conf $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md", "projects.md"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            let ctx = constField "title" title
                      <> constField "posts" list
                      <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx tags <> bodyField "description"
            posts <- fmap (take 10) . recentFirst
                         =<< loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtx tags) (return posts)
                        <> constField "title" "Home"
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


postCtx :: Tags -> Context String
postCtx tags = modificationTimeField "mtime" "%U"
                   <> dateField "date" "%e %b, %Y"
                   <> tagsField "prettytags" tags
                   <> defaultContext

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Erlend Hamberg"
    , feedDescription = "Personal Blog of Erlend Hamberg"
    , feedAuthorName  = "Erlend Hamberg"
    , feedAuthorEmail = ""
    , feedRoot        = "http://hamberg.no/erlend"
    }

conf :: Configuration
conf = defaultConfiguration
           { deployCommand = "rsync -cav _site/* hamberg.no:/srv/www/erlend"
           }
