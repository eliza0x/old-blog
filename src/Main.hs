{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((>=>))
import Data.Monoid ((<>))
import Hakyll
import Hakyll.Web.Hamlet
import Text.Highlighting.Kate.Format.HTML (styleToCss)
import Text.Highlighting.Kate.Styles (tango)

import StyleSheet (styleSheet)

main :: IO ()
main = 
  let styleSheetCompiler = makeItem . compressCss $ styleSheet :: Compiler (Item String)
  in  hakyll $ do

    -- Static files
    match ("images/*" .||. "files/*" .||. "CNAME") $ do
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
                    listField "posts" (postCtx tags) (return posts) <>
                    paginateContext archive pageNum                 <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.hamlet" archiveCtx
                >>= loadAndApplyTemplate "templates/flame.hamlet" archiveCtx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag identifier -> do

      let title = "Posts tagged " ++ tag
      route   idRoute
      compile $ do
        posts <- recentFirst =<< loadAll identifier
        let tagCtx =
                constField "title" title                        <>
                listField "posts" (postCtx tags) (return posts) <>
                defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/tag.hamlet" tagCtx
            >>= loadAndApplyTemplate "templates/flame.hamlet" tagCtx
            >>= relativizeUrls
      version "rss" $ do
          route   $ setExtension "xml"
          compile $ loadAllSnapshots identifier "content"
              >>= fmap (take 10) . recentFirst
              >>= renderRss (feedConfiguration $ title ++ " - ") feedCtx

    -- Render the top pages
    match "top_pages/*.md" $ do
      route $ gsubRoute "top_pages/" (const "") `composeRoutes` setExtension "html"
      compile $ pandocCompiler 
        >>= loadAndApplyTemplate "templates/flame.hamlet" (postCtx tags)
        >>= relativizeUrls

    -- Render the index page
    match "top_pages/index.hamlet" $ do
      route $ gsubRoute "top_pages/" (const "") `composeRoutes` setExtension "html"
      compile $ do
        posts <- fmap (take 4) . recentFirst =<< loadAll "posts/*"
        let indexedContext =
                listField "posts" (postCtx tags) (return posts) <>
                defaultContext
        hamlCompiler
          >>= applyAsTemplate indexedContext
          >>= loadAndApplyTemplate "templates/flame.hamlet" (postCtx tags)
          >>= relativizeUrls

    -- Render the articles
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.hamlet" (postCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/social.hamlet" (postCtx tags)
            >>= loadAndApplyTemplate "templates/flame.hamlet" (postCtx tags)
            >>= relativizeUrls

    -- Build templates
    match "templates/*.hamlet" $ compile hamlTemplateCompiler

    -- Render RSS feed
    create ["rss.xml"] $ do
      route idRoute
      compile $
        loadAllSnapshots "posts/*" "content"
          >>= fmap (take 10) . recentFirst
          >>= renderRss (feedConfiguration "All posts - ") feedCtx

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
    { feedTitle       = title ++ "eliza.link"
    , feedDescription = "技術や読書録を残します"
    , feedAuthorName  = "Eliza Calls"
    , feedAuthorEmail = "me@eliza.link"
    , feedRoot        = "https://eliza.link"
    }
