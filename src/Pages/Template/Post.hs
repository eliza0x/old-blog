{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell #-}

module Pages.Template.Post(post) where

import qualified Hakyll as H
import Text.Hamlet (shamlet, Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

post :: H.Template
post = let html = renderHtml template :: String
        in  H.readTemplate html
    
template :: Html
template = [shamlet|
  <h2>$title$

  <p>Posted on $date$
  \$if(author)$
    <p>by $author$
  \$endif$

  \$body$
|]
