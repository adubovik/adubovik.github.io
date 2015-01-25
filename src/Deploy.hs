module Deploy (main, deploy) where

import Control.Monad
import Data.Functor
import System.Directory
import System.FilePath.Posix

exceptions :: [String]
exceptions =
  [ "src"
  , ".gitignore"
  , ".git"
  , "CNAME"
  ]

delete :: FilePath -> IO ()
delete fileName = do
  isDir  <- doesDirectoryExist fileName
  isFile <- doesFileExist fileName
  when ( isDir) $ removeDirectoryRecursive fileName
  when (isFile) $ removeFile fileName

rename :: FilePath -> FilePath -> IO ()
rename old new = do
  isDir  <- doesDirectoryExist old
  isFile <- doesFileExist old
  when ( isDir) $ renameDirectory old new
  when (isFile) $ renameFile old new

getDirFiles :: FilePath -> IO [String]
getDirFiles folder
  =  filter (not . (`elem` [".",".."]))
 <$> getDirectoryContents folder
  
deleteFiles :: FilePath -> IO ()
deleteFiles folder = do
  files <-  map (folder </>)
         .  filter (not . (`elem` exceptions))
        <$> getDirFiles folder

  when (not . null $ files) $ do
    
    forM_ files $ \file ->
      putStrLn $ "Going to remove: " ++ file

    putStrLn "Are you ok with this? [yY]"
    ans <- getLine
    when (not $ ans `elem` ["y","Y"]) $
      error "Ok. I quit."

    forM_ files $ \file -> do
      putStrLn $ "Deleting: " ++ file
      delete file

moveFiles :: FilePath -> FilePath -> IO ()
moveFiles src dst = do
  files <- getDirFiles src

  forM_ files $ \file -> do
    let srcFile = src </> file
        dstFile = dst </> file
    putStrLn $ srcFile ++ " ->\n  " ++ dstFile
    rename srcFile dstFile

deploy :: FilePath -> IO ()
deploy srcDir = do
  let dstDir = srcDir </> ".." </> ".."
  deleteFiles dstDir
  moveFiles srcDir dstDir
  putStrLn "Done."  
  
main :: IO ()
main = do
  cdir <- getCurrentDirectory
  let srcDir = cdir </> "_site"
  deploy srcDir
