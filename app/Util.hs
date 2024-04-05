module Util (addToIndex, splitOnDelimiter, getFileContents, kellyColours)
  where

import Data.Text (splitOn, pack, unpack)
import Data.List
import GHC.IO.Handle (hSetEncoding)
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import GHC.IO.Encoding

--adds the value to the index
--if the list is too short expands it adding Nothing
addToIndex :: [Maybe a] -> Maybe a -> Int -> [Maybe a]
addToIndex l v i | length l -1 < i = l ++ (replicate (i - length l) Nothing) ++ [v]
                 | otherwise       = let (h, t) = splitAt i l in h ++ (v : (tail t))

splitOnDelimiter :: String -> String -> [String]
splitOnDelimiter s d = map (Data.Text.unpack) $ splitOn (Data.Text.pack d) $ Data.Text.pack s

getFileContents :: FilePath -> IO String
getFileContents path = do
  file <- openFile path ReadMode
  hSetEncoding file utf8
  hGetContents file

kellyColours :: [String]
kellyColours = ["#F2F3F4", "#222222", "#F3C300", "#875692", "#F38400", "#A1CAF1", "#BE0032", "#C2B280", "#848482", "#008856", "#E68FAC", "#0067A5", "#F99379", "#604E97", "#F6A600", "#B3446C", "#DCD300", "#882D17", "#8DB600", "#654522", "#E25822", "#2B3D26"]

