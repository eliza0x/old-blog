{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((>=>))
import Data.Monoid ((<>))
import Hakyll
import Text.Highlighting.Kate.Format.HTML (styleToCss)
import Text.Highlighting.Kate.Styles (tango)

import Flame (flame)
import StyleSheet (styleSheet)

main :: IO ()
main = 
  let flameTemplate = readTemplate flame :: Template
      styleSheetCompiler = makeItem . compressCss $ styleSheet :: Compiler (Item String)
  in  hakyll $ do
    -- Static files
    match ("images/*.jpg" .||. "images/*.png" .||. "images/*.gif" .||.
           "favicon.ico"  .||. "files/*" .||. "CNAME") $ do
        route   idRoute
        compile copyFileCompiler

    -- source code highlighting style sheet
    create ["css/highlight.css"] $ do
      route   idRoute
      compile $ makeItem (compressCss $ styleToCss tango)

    -- Style sheet
    create ["css/style.css"] $ do
      route idRoute
      compile styleSheetCompiler

    -- Create tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Configure pagenations
    archive <- buildPaginateWith
        (sortRecentFirst >=> return . paginateEvery 5)
        "posts/*"
        (\n -> if n == 1
               then fromFilePath "archive.html"
               else fromFilePath $ "archive/" ++ show n ++ ".html")

    paginateRules archive $ \pageNum pattern -> do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let archiveCtx =
                    constField "title" "Archives"                   `mappend`
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    paginateContext archive pageNum                 `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= applyTemplate flameTemplate archiveCtx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do

      let title = "Posts tagged " ++ tag
      route   idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let tagCtx =
                constField "title" title `mappend`
                listField "posts" (postCtx tags) (return posts) `mappend`
                defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html" tagCtx
            >>= applyTemplate flameTemplate tagCtx
            >>= relativizeUrls

      version "rss" $ do
          route   $ setExtension "xml"
          compile $ loadAllSnapshots pattern "content"
              >>= fmap (take 10) . recentFirst
              >>= renderRss (feedConfiguration $ " - " ++ title) feedCtx

    -- Render the top pages
    match ("top_pages/*.md") $ do
      route $ gsubRoute "top_pages/" (const "") `composeRoutes` setExtension "html"
      compile $ pandocCompiler >>= relativizeUrls

    -- Render the index page
    match "top_pages/index.html" $ do
      route $ gsubRoute "top_pages/" (const "") `composeRoutes` setExtension "html"
      compile $ do
        posts <- fmap (take 4) . recentFirst =<< loadAll "posts/*"
        let indexedContext =
                listField "posts" (postCtx tags) (return posts) `mappend`
                defaultContext
        getResourceBody
          >>= applyAsTemplate indexedContext
          >>= applyTemplate flameTemplate indexedContext
          >>= relativizeUrls

    -- Render the articles
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" (postCtx tags)
            >>= applyTemplate flameTemplate (postCtx tags)
            >>= saveSnapshot "content"
            >>= relativizeUrls

    -- Build templates
    match "templates/*" $ compile templateCompiler
    
    -- Render RSS feed
    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        loadAllSnapshots "posts/*" "content"
          >>= fmap (take 10) . recentFirst
          >>= renderRss (feedConfiguration "All posts") feedCtx

    -- Build Atop Feed
    create ["atom.xml"] $ do
        route   idRoute
        compile $ do
            let feedCtx =
                    postCtx tags `mappend`
                    bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom (feedConfiguration "")feedCtx posts

postCtx :: Tags -> Context String
postCtx tags =
  dateField "date" "%B %e, %Y" `mappend`
  teaserField "teaser" "content" `mappend`
  tagsField "tags" tags          `mappend`
  defaultContext

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "eliza.link" ++ title
    , feedDescription = "技術や読書録を残します"
    , feedAuthorName  = "Eliza Calls"
    , feedAuthorEmail = "me@eliza.link"
    , feedRoot        = "https://eliza.link"
    }
