{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import qualified Hakyll as H
import           Hakyll ((.||.))
import qualified Hakyll.Web.Sass as S

main :: IO ()
main = H.hakyll $ do
  H.match ("images/*" .||.  "CNAME" .||. ".commit_template") $ do
    H.route   H.idRoute
    H.compile H.copyFileCompiler

  H.match "css/*" $ do
    H.route   H.idRoute
    H.compile H.compressCssCompiler

  H.match "posts/*" $ do
    H.route $ H.setExtension "html"
    H.compile $ H.pandocCompiler
        >>= H.loadAndApplyTemplate "templates/post.html"    postCtx
        >>= H.loadAndApplyTemplate "templates/default.html" postCtx
        >>= H.relativizeUrls
  
  H.match "index.html" $ do
    H.route H.idRoute
    H.compile $ do
      posts <- H.recentFirst =<< H.loadAll "posts/*"
      let indexCtx =
              H.listField "posts" postCtx (return posts) `mappend`
              H.constField "title" "Home"                `mappend`
              H.defaultContext
      H.getResourceBody
        >>= H.applyAsTemplate indexCtx
        >>= H.loadAndApplyTemplate "templates/default.html" indexCtx
        >>= H.relativizeUrls
  
  H.match "archive.html" $ do
    H.route H.idRoute
    H.compile $ do
      posts <- H.recentFirst =<< H.loadAll "posts/*"
      let indexCtx = 
              H.listField  "posts" postCtx (return posts) `mappend`
              H.constField "title" "Archive"                `mappend` 
              H.defaultContext
      H.getResourceBody
        >>= H.applyAsTemplate indexCtx
        >>= H.loadAndApplyTemplate "templates/default.html" indexCtx
        >>= H.relativizeUrls
  
  
  H.match "templates/*" $ H.compile H.templateCompiler

postCtx :: H.Context String
postCtx =
  H.dateField "date" "%B %e, %Y" `mappend`
  H.defaultContext
