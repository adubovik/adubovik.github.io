{-# language
   ViewPatterns
  #-}

module Preprocess (mdPreprocess) where

import Text.Pandoc.Shared

mdPreprocess :: String -> String
mdPreprocess = hideSections

--
-- Fenced by `+` symbols code blocks will be removed
-- from the string.
--
-- For example this:
--
-- B
-- +++
--
-- > main :: IO ()
-- > main = putStrLn "Hello!"
---
-- ++++
-- A
--
-- will be transformed into this:
--
-- B
-- A
--
-- Logic is quite similar to the handling of tilda-fenced code blocks in Pandoc:
-- http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html#fenced-code-blocks

hideSections :: String -> String
hideSections = unlines . third . foldr folder (Nothing,[],[]) . lines
  where
    sectionSymbol = '+'
    third (_,_,x) = x

    getSize (trim -> line)
      | all (==sectionSymbol) line
      , length line >= 3
      = Just $ length line
      | otherwise
      = Nothing

    folder line (size, dropLines, acc) =
      let size'      = getSize line
          acc'       = line:acc
          dropLines' = line:dropLines
          dropAcc    = drop (length dropLines) acc
      in
      case (size,size') of
        (Just oldSize, Just newSize) ->
          if oldSize <= newSize
          then (Nothing, [], dropAcc)
          else (size, dropLines', acc')
        (Nothing, Just _) ->
          (size', [line], acc')
        (Just _, Nothing) ->
          (size, dropLines', acc')
        (Nothing, Nothing) ->
          (size, acc', acc')
