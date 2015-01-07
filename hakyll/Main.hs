{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Monoid

import Hakyll

main :: IO ()
main = hakyllWith defaultConfiguration $ do

  -- Copy images
  match "images/*" $ do
    route $ gsubRoute "^images" (const "static")
    compile copyFileCompiler

  -- Compress CSS
  match "css/*" $ do
    route $ gsubRoute "^css" (const "static")
    compile copyFileCompiler

  -- Copy JS
  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler

  -- Posts
  match "posts/*" $ do
    route $ setExtension ".html"
    compile $ pandocCompiler
       >>= loadAndApplyTemplate "templates/post.html" defaultContext
       >>= loadAndApplyTemplate "templates/default.html" defaultContext
       >>= relativizeUrls

  -- Render posts list
  create ["index.html"] $ do
    route idRoute
    compile $ do
      pages <- recentFirst =<< loadAll "posts/*"
      itemTpl <- loadBody "templates/postitem.html"
      list <- applyTemplateList itemTpl defaultContext pages
      let postsCtx =  constField "title" "Blog posts"
                   <> constField "posts" list
                   <> defaultContext
      makeItem list
        >>= loadAndApplyTemplate "templates/posts.html" postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postsCtx
        >>= relativizeUrls

  -- Read templates
  match "templates/*" $
    compile templateCompiler

  -- Render RSS feed
  create ["rss.xml"] $ do
    route idRoute
    compile $ loadAll "posts/*"
      >>= renderRss feedConfiguration defaultContext

-- Feed config
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "Anton Dubovik - a personal blog"
  , feedDescription = "Personal blog of Anton Dubovik"
  , feedAuthorName  = "Anton Dubovik"
  , feedAuthorEmail = "anton.dubovik@gmail.com"
  , feedRoot        = "http://dubovik.info"
  }
