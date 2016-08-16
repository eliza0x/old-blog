{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell #-}

module Pages.TopPages(
  aboutR
, archiveR
, contactR
, indexR
, productsR ) where

import qualified Hakyll as H
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Pages.Util (postCtx)
import Pages.Template

indexR :: H.Rules ()
indexR =
  let indexCompiler :: H.Compiler (H.Item String)
      indexCompiler = H.makeItem $ renderHtml $(shamletFile "pages/index.haml")
  in  H.match "pages/index.haml" $ do
      H.route $ H.gsubRoute "pages/" (const "") `H.composeRoutes` H.setExtension "html"
      H.compile $ do
        posts <- H.recentFirst =<< H.loadAll "posts/*"
        let indexCtx =
                H.listField "posts" postCtx (return posts) `mappend`
                H.constField "title" "Home"                `mappend`
                H.defaultContext
        indexCompiler
          >>= H.applyAsTemplate indexCtx
          >>= H.applyTemplate flame indexCtx
          >>= H.relativizeUrls

archiveR :: H.Rules ()
archiveR = 
  let archiveCompiler :: H.Compiler (H.Item String)
      archiveCompiler = H.makeItem $ renderHtml $(shamletFile "pages/archive.haml")
  in  H.match "pages/archive.haml" $ do
      H.route $ H.gsubRoute "pages/" (const "") `H.composeRoutes` H.setExtension "html"
      H.compile $ do
        posts <- H.recentFirst =<< H.loadAll "posts/*"
        let indexCtx = 
                H.listField  "posts" postCtx (return posts) `mappend`
                H.constField "title" "Archive"                `mappend` 
                H.defaultContext
        archiveCompiler
          >>= H.applyAsTemplate indexCtx
          >>= H.applyTemplate flame indexCtx
          >>= H.relativizeUrls

aboutR :: H.Rules ()
aboutR =
  let aboutCompiler :: H.Compiler (H.Item String)
      aboutCompiler = H.makeItem $ renderHtml $(shamletFile "pages/about.haml")
  in  H.match "pages/about.haml" $ do
      H.route $ H.gsubRoute "pages/" (const "") `H.composeRoutes` H.setExtension "html"
      H.compile $ aboutCompiler
          >>= H.applyTemplate flame postCtx
          >>= H.relativizeUrls

contactR :: H.Rules ()
contactR =
  let contactCompiler :: H.Compiler (H.Item String)
      contactCompiler = H.makeItem $ renderHtml $(shamletFile "pages/contact.haml")
  in  H.match "pages/contact.haml" $ do
      H.route $ H.gsubRoute "pages/" (const "") `H.composeRoutes` H.setExtension "html"
      H.compile $ contactCompiler
          >>= H.applyTemplate flame postCtx
          >>= H.relativizeUrls

productsR :: H.Rules ()
productsR =
  let productsCompiler :: H.Compiler (H.Item String)
      productsCompiler = H.makeItem $ renderHtml $(shamletFile "pages/products.haml")
  in  H.match "pages/products.haml" $ do
      H.route $ H.gsubRoute "pages/" (const "") `H.composeRoutes` H.setExtension "html"
      H.compile $ productsCompiler
          >>= H.applyTemplate flame postCtx
          >>= H.relativizeUrls
