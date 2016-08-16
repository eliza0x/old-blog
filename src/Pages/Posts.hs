{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell #-}

module Pages.Posts where

import qualified Hakyll as H

import Pages.Util (postCtx)
import Pages.Template

postsR :: H.Rules ()
postsR =
  H.match "posts/*" $ do
    H.route $ H.setExtension "html"
    H.compile $ H.pandocCompiler
        >>= H.applyTemplate post postCtx
        >>= H.applyTemplate flame postCtx
        >>= H.relativizeUrls
