module Main where

import qualified Hakyll as H

import Blog.Css
import Blog.Etc
import Blog.Pages
import Blog.Posts
import Blog.Templates

main :: IO ()
main = H.hakyll $ do
  cssR        -- style sheet
  postsR      -- posts/*
  indexR      -- pages/index.haml
  archiveR    -- pages/archive.haml
  aboutR      -- pages/about.haml
  productsR   -- pages/products.haml
  contactR   -- pages/contact.haml
  templatesR  -- templates
  etcR        -- etc ...


