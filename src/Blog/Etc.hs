{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell #-}

module Blog.Etc(etcR) where

import qualified Hakyll as H
import           Hakyll ((.||.))
import Text.Hamlet (shamletFile, Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

etcR :: H.Rules ()
etcR =
  H.match ("slides/*" .||. "images/*" .||.  "CNAME" .||. ".commit_template") $ do
    H.route   H.idRoute
    H.compile H.copyFileCompiler
