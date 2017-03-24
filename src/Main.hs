{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad     ((>=>))
import           Data.Monoid       ((<>))
import           Hakyll
import           Hakyll.Web.Hamlet
import           Text.Pandoc

main :: IO ()
main = hakyll $ do
    -- Static files
    match ("images/*" .||. "files/*" .||. "CNAME") $ do
        route   idRoute
        compile copyFileCompiler

    -- Style sheet
    match "static/style.hs" $ do
      route $ setExtension "css"
      compile $ getResourceString
        >>= withItemBody(unixFilter "stack" ["runghc", "--package", "clay", "--stack-yaml", "stack.yaml"])
        >>= return . fmap compressCss
        >>= relativizeUrls

    -- Create tags
    tagsOfPosts <- buildTags "posts/*" (fromCapture "tags/*.html")
    -- Render tags
    tagsRules tagsOfPosts $ \tag identifier -> do
      let title = "Posts tagged " ++ tag
      route   idRoute
      compile $ do
        posts <- recentFirst =<< loadAll identifier
        let tagCtx =
                constField "title" title                        <>
                listField "posts" (postContext tagsOfPosts) (return posts) <>
                defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/tag.hamlet" tagCtx
            >>= loadAndApplyTemplate "templates/flame.hamlet" tagCtx
            >>= relativizeUrls
      version "rss" $ do
          route   $ setExtension "xml"
          compile $ loadAllSnapshots identifier "content"
              >>= fmap (take 10) . recentFirst
              >>= renderRss (feedConfiguration $ title ++ " - ") feedContext

    -- Configure pagenations
    archive <-
      let archivePageName n =  if n == 1
               then fromFilePath "archive.html"
               else fromFilePath $ "archive/" ++ show n ++ ".html"
      in  buildPaginateWith
        (sortRecentFirst >=> return . paginateEvery 5) "posts/*" archivePageName

    paginateRules archive $ \pageNum identifier -> do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll identifier
            let archiveCtx =
                    constField "title" "Archives"                   <>
                    listField "posts" (postContext tagsOfPosts) (return posts) <>
                    paginateContext archive pageNum                 <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.hamlet" archiveCtx
                >>= loadAndApplyTemplate "templates/flame.hamlet" archiveCtx
                >>= relativizeUrls

    -- Render the top pages
    match "top_pages/*.md" $ do
      route $ gsubRoute "top_pages/" (const "") `composeRoutes` setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/flame.hamlet" (postContext tagsOfPosts)
        >>= relativizeUrls

    -- Render the index page
    match "top_pages/index.hamlet" $ do
      route $ gsubRoute "top_pages/" (const "") `composeRoutes` setExtension "html"
      compile $ do
        posts <- fmap (take 4) . recentFirst =<< loadAll "posts/*"
        let indexedContext =
              listField "posts" (postContext tagsOfPosts) (return posts) <>
              field "tags" (\_ -> renderTagList tagsOfPosts) <>
              defaultContext
        hamlCompiler
          >>= applyAsTemplate indexedContext
          >>= loadAndApplyTemplate "templates/flame.hamlet" (postContext tagsOfPosts)
          >>= relativizeUrls

    -- Render the articles
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/article.hamlet" (postContext tagsOfPosts)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/social.hamlet" (postContext tagsOfPosts)
            >>= loadAndApplyTemplate "templates/flame.hamlet" (postContext tagsOfPosts)
            >>= relativizeUrls

    -- Build templates
    match "templates/*.hamlet" $ compile hamlTemplateCompiler

    -- Render RSS feed
    create ["rss.xml"] $ do
      route idRoute
      compile $
        loadAllSnapshots "posts/*" "content"
          >>= fmap (take 10) . recentFirst
          >>= renderRss (feedConfiguration "All posts - ") feedContext

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions {
    writerHTMLMathMethod = MathJax ""
  , writerTableOfContents = True
  , writerTemplate = "$body$"
  , writerStandalone = True
  }

postContext :: Tags -> Context String
postContext tags =
  dateField "date" "%B %e, %Y" `mappend`
  teaserField "teaser" "content" `mappend`
  tagsField "tags" tags          `mappend`
  defaultContext

feedContext :: Context String
feedContext = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = title ++ " | ELIZA.link"
    , feedDescription = "技術録や日記を残します"
    , feedAuthorName  = "eliza0x"
    , feedAuthorEmail = "me@eliza.link"
    , feedRoot        = "https://eliza.link"
    }
