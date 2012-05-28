{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM_)
import Control.Arrow (arr, (>>>), (&&&), (>>^))
import Data.Monoid (mempty)

import Hakyll

-- | Entry point
--
main :: IO ()
main = hakyllWith config $ do
    -- Copy images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy JavaScript
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Render the /tmp index page
    match "tmp/index.html" $ do
        route idRoute
        compile $ readPageCompiler >>> relativizeUrlsCompiler

    -- Render each and every post
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%e %B, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Post list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "Posts")
        >>> setFieldPageList recentFirst
                "templates/postitem.html" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Erlend Hamberg")
        >>> requireA "tags" (setFieldA "tags" renderTagCloud')
        >>> setFieldPageList recentFirst
                "templates/postitem.html" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render some static pages
    forM_ ["about.md", "resume.md", "projects.md"] $ \p ->
        match p $ do
            route   $ setExtension ".html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    -- Render the 404 page, relativize URLs to /erlend to make sure this will work everywhere
    match "404.md" $ do
        route  $ setExtension ".html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsTo "/erlend"

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

    -- End
    return ()
  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 70 180

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
        >>> pageListCompiler recentFirst "templates/postitem.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync -cav _site/* hamberg.no:www/"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Erlend Hamberg"
    , feedDescription = "Personal blog of Erlend Hamberg"
    , feedAuthorName  = "Erlend Hamberg"
    , feedRoot        = "http://hamberg.no/erlend/"
    }

relativizeUrlsTo :: String -> Compiler (Page String) (Page String)
relativizeUrlsTo base = getRoute &&& id >>^ uncurry relativize
  where
    relativize Nothing  = id
    relativize (Just r) = fmap (relativizeUrls base)
