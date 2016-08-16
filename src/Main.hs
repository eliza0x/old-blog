{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell #-}

module Main where

import qualified Hakyll as H
import           Hakyll ((.||.))
import Text.Hamlet (shamletFile, Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = H.hakyll $ etcR >> cssR >> postsR >>
                  indexR >> archiveR >> templatesR

etcR :: H.Rules ()
etcR =
  H.match ("slides/*" .||. "images/*" .||.  "CNAME" .||. ".commit_template") $ do
    H.route   H.idRoute
    H.compile H.copyFileCompiler

cssR :: H.Rules ()
cssR =
  H.match "css/*" $ do
    H.route   H.idRoute
    H.compile H.compressCssCompiler
 
postsR :: H.Rules ()
postsR =
  H.match "posts/*" $ do
    H.route $ H.setExtension "html"
    H.compile $ H.pandocCompiler
        >>= H.loadAndApplyTemplate "templates/post.html"    postCtx
        >>= H.loadAndApplyTemplate "templates/default.html" postCtx
        >>= H.relativizeUrls

indexR :: H.Rules ()
indexR =
  H.match "pages/index.haml" $ do
    H.route $ H.gsubRoute "pages/" (const "") `H.composeRoutes` H.setExtension "html"
    H.compile $ do
      posts <- H.recentFirst =<< H.loadAll "posts/*"
      let indexCtx =
              H.listField "posts" postCtx (return posts) `mappend`
              H.constField "title" "Home"                `mappend`
              H.defaultContext
      indexCompiler
        >>= H.applyAsTemplate indexCtx
        >>= H.loadAndApplyTemplate "templates/default.html" indexCtx
        >>= H.relativizeUrls

indexCompiler :: H.Compiler (H.Item String)
indexCompiler = H.makeItem $ renderHtml $(shamletFile "pages/index.haml")

archiveR :: H.Rules ()
archiveR =
  H.match "pages/archive.haml" $ do
    H.route $ H.gsubRoute "pages/" (const "") `H.composeRoutes` H.setExtension "html"
    H.compile $ do
      posts <- H.recentFirst =<< H.loadAll "posts/*"
      let indexCtx = 
              H.listField  "posts" postCtx (return posts) `mappend`
              H.constField "title" "Archive"                `mappend` 
              H.defaultContext
      archiveCompiler
        >>= H.applyAsTemplate indexCtx
        >>= H.loadAndApplyTemplate "templates/default.html" indexCtx
        >>= H.relativizeUrls

archiveCompiler :: H.Compiler (H.Item String)
archiveCompiler = H.makeItem $ renderHtml $(shamletFile "pages/archive.haml")

templatesR :: H.Rules ()
templatesR =
  H.match "templates/*" $ H.compile H.templateCompiler

postCtx :: H.Context String
postCtx =
  H.dateField "date" "%B %e, %Y" `mappend`
  H.defaultContext


