{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell #-}

module Pages.Etc(etcR) where

import qualified Hakyll as H
import           Hakyll ((.||.))

etcR :: H.Rules ()
etcR =
  H.match ("slides/*" .||. "images/*" .||.  "CNAME" .||. ".commit_template") $ do
    H.route   H.idRoute
    H.compile H.copyFileCompiler
