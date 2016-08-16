{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell #-}

module Pages.Template.Archive(archive) where

import qualified Hakyll as H
import Text.Hamlet (shamlet, Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

archive :: H.Template
archive = let html = renderHtml template :: String
        in  H.readTemplate html
    
template :: Html
template = [shamlet|
  \$partial("templates/post_list.html")$
|]
