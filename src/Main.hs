{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Hakyll as H

import Pages

main :: IO ()
main = H.hakyll $ do
  H.match "templates/post-list.html" $ H.compile H.templateCompiler
  cssR        -- style sheet
  postsR      -- posts/*
  indexR      -- pages/index.haml
  archiveR    -- pages/archive.haml
  aboutR      -- pages/about.haml
  productsR   -- pages/products.haml
  contactR   -- pages/contact.haml
  etcR        -- etc ...


