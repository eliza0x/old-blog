{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell #-}

module Blog.Posts where

import qualified Hakyll as H

import Blog.Util (postCtx)

postsR :: H.Rules ()
postsR =
  H.match "posts/*" $ do
    H.route $ H.setExtension "html"
    H.compile $ H.pandocCompiler
        >>= H.loadAndApplyTemplate "templates/post.html"    postCtx
        >>= H.relativizeUrls
