{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Monoid
import System.Exit

import Hakyll

import qualified Deploy as Deploy
import Preprocess

main :: IO ()
main = hakyllWith config $ do

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
    compile $ pandocCompilerWithPreprocess mdPreprocess
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

config :: Configuration
config = defaultConfiguration
  { deploySite = deploy
  }
  where
    deploy :: Configuration -> IO ExitCode
    deploy conf = do
      putStrLn "Deploying..."
      Deploy.deploy $ destinationDirectory conf
      return ExitSuccess

pandocCompilerWithPreprocess
  :: (String -> String)
  -> Compiler (Item String)
pandocCompilerWithPreprocess preproc = do
  item <- getResourceBody
  item' <- withItemBody (return . preproc) item
  let pandoc = readPandocWith defaultHakyllReaderOptions item'
  let html = writePandocWith defaultHakyllWriterOptions pandoc
  return html
