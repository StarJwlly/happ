{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use !!" #-}
module Diff (getData)   --,getFileContents, getArgumentData, getMapData, toChord, getColumnData, getChordDiff)
  where
import Data.Text (splitOn, pack, unpack)
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Control.Monad.IO.Class
import GHC.IO.Handle (hSetEncoding)
import GHC.IO.Encoding
import qualified Data.Maybe
import Data.List (nub)

import Util

--TODO limpar esse caos

data TimingPoint = TimingPoint {offset :: Int, beatLen :: Double, inherited :: Bool} deriving (Show)
data Note = HitNote | HoldNote | ReleaseNote deriving (Show, Eq)
data HitObject = HitCircle {timeH :: Int, columnH :: Int} | LongNote {timeH :: Int, columnH :: Int, releaseTime :: Int} deriving (Show)
data Chord = Chord {time :: Int, notes :: [Maybe Note]} deriving (Show)
data ArgumentData = ArgumentData {hithit :: [[(Double, Double, Double)]], hitrel :: [[(Double, Double, Double)]], relhit :: [[(Double, Double, Double)]], relrel :: [[(Double, Double, Double)]], hold :: [[Double]], fingers :: ([[Int]],[[Int]])} deriving (Show)


--receive map file path and diff function file path
--returns (artistName, songName, creatorName, diffName, keymode, diffData)
getData :: FilePath -> FilePath -> IO (String, String, String, String, Int, ([Int], [[[Double]]]), ([Int], [[[Double]]]))
getData mapP diffP = do
  mapS <- liftIO $ getFileContents mapP
  diffS <- liftIO $ getFileContents diffP

  let (artistName, songName, creatorName, diffName, keymode, hitObjects) = getMapData mapS
  --putStrLn $ show (artistName, songName, creatorName, diffName, keymode, hitObjects)


  let chords = toChord hitObjects (replicate keymode Nothing) (timeH $ head hitObjects)


  let (chordsL, chordsR) = unzip $ splitChords chords

  --let columnData = getColumnData chords
  --liftIO $ putStrLn $ show columnData
  --liftIO $ putStrLn ""

  let columnDataL = getColumnData chordsL
  let columnDataR = getColumnData chordsR

  let argumentData = getArgumentData diffS
  --let diffData = (map time chords, map (\x -> getChordDiff x columnData  argumentData) chords)
  --liftIO $ putStrLn $ show diffData
  --liftIO $ putStrLn ""
  liftIO $ putStrLn $ show $ fingers argumentData
  liftIO $ putStrLn ""

  let fingerL = fst (fingers argumentData) !! (keymode - 1)
  let fingerR = snd (fingers argumentData) !! (keymode - 1)
  
  
  let diffDataL = (map time chords, map (\x -> getChordDiff x columnDataL argumentData fingerL) chordsL)
  let diffDataR = (map time chords, map (\x -> getChordDiff x columnDataR argumentData fingerR) chordsR)

  --return (artistName, songName, creatorName, diffName, keymode, diffData)
  return (artistName, songName, creatorName, diffName, keymode, diffDataL, diffDataR)


getArgumentData :: String -> ArgumentData
getArgumentData argStr = ArgumentData (getDiffFunctionArgs hh) (getDiffFunctionArgs hr) (getDiffFunctionArgs rh) (getDiffFunctionArgs rr) (map (map (\(x, _, _) -> x)) (getDiffFunctionArgs h)) ff
  where argLines = lines argStr
        hh = tail $ dropWhile ("[hithit]" /=) argLines
        hr = tail $ dropWhile ("[hitrel]" /=) argLines
        rh = tail $ dropWhile ("[relhit]" /=) argLines
        rr = tail $ dropWhile ("[relrel]" /=) argLines
        h = tail $ dropWhile ("[hold]" /=) argLines
        f = tail $ dropWhile ("[fingers]" /=) argLines
        ff = unzip $ map (\x -> let y = splitOnDelimiter x "|" in (map read $ words (y !! 0) :: [Int], map read $ words (y !! 1) :: [Int])) f


getDiffFunctionArgs :: [String] -> [[(Double, Double, Double)]]
getDiffFunctionArgs [] = []
getDiffFunctionArgs (s:ss)   | s == ""   = []
                             | otherwise = curr : getDiffFunctionArgs  ss
  where curr = map (\x -> let temp = map read $ words x :: [Double] in (temp !! 0, temp !! 1, temp !! 2)) $ splitOnDelimiter s "|"






--receives .osu as a string 
--returns artistName, songName, creatorName, diffName, keymode, hitObjects
getMapData :: String -> (String, String, String, String, Int, [HitObject])
getMapData mapStr = (artistName,songName,creatorName,diffName,keyCount,hitObjects)
  where mapLines = lines mapStr
        metadataLine = dropWhile ("[Metadata]" /=) mapLines
        difficultyLine = dropWhile ("[Difficulty]" /=) mapLines
        hitObjectsLine = dropWhile ("[HitObjects]" /=) mapLines
        getVal s = splitOnDelimiter s ":" !! 1
        songName = getVal $ head $ tail $ tail metadataLine
        artistName = getVal $ head $ tail $ tail $ tail $ tail metadataLine
        creatorName = getVal $ head $ tail $ tail $ tail $ tail $ tail metadataLine
        diffName = getVal $ head $ tail $ tail $ tail $ tail $ tail $ tail metadataLine
        keyCount = read (getVal $ head $ tail $ tail difficultyLine) :: Int
        hitObjects = map (toHitObject keyCount) (tail hitObjectsLine)




toHitObject :: Int -> String -> HitObject
toHitObject keyCount str | t >= 128 = LongNote (read (n !! 2)) c (read $ head $ splitOnDelimiter (n !! 5) ":")
                         | otherwise  = HitCircle (read (n !! 2)) c
                           where n = splitOnDelimiter str ","
                                 t = read (n !! 3) :: Integer
                                 c = floor (fromIntegral (read (head n) * keyCount) / 512)




addNote :: [Maybe Note] -> HitObject -> [Maybe Int] ->  ([Maybe Note], [Maybe Int])
addNote notes h releases = (addToIndex notes (Just HitNote) col, addToIndex releases rel col)
  where (col, rel) = case h of
            HitCircle _ c  -> (c, Nothing)
            LongNote _ c r -> (c, Just r)

--receive a list of HitObjects
--        a list of timings of releases for each column (starting value needs to be an array od Nothings with length of keycout)
--        the current time (starting value is hitObjects' head time)
--returns an array with all chords
toChord :: [HitObject] -> [Maybe Int] -> Int -> [Chord]
toChord [] [] timeMs = []
toChord hitObjects releases timeMs = if nextTimeMs /= maxBound
  then currChord : toChord nextHitObjects finalReleases nextTimeMs
  else [currChord]
  where currHitObjects = takeWhile ((==timeMs) . timeH) hitObjects
        nextHitObjects = drop (length currHitObjects) hitObjects
        nextHitObjectsHeadTime = if null nextHitObjects then maxBound else timeH $ head nextHitObjects
        (holdReleases, nextReleases, _) = foldl (\(hr, nr, i) r -> case r of
          Nothing -> (addToIndex hr Nothing i, addToIndex nr Nothing i, i + 1)
          Just t  -> if t == timeMs
            then (addToIndex hr (Just ReleaseNote) i, addToIndex nr Nothing i, i + 1)
            else (addToIndex hr (Just HoldNote) i, nr, i + 1)
          ) ([], [], 0) releases
        (finalNotes, finalReleases) = foldl (\(h, fr) cho -> addNote h cho fr) (holdReleases, nextReleases) currHitObjects
        nextTimeMs = min nextHitObjectsHeadTime (minimum $ map (Data.Maybe.fromMaybe maxBound) finalReleases)
        currChord = Chord timeMs finalNotes




--receives a chord array
--returns an array of tuples with the chords split in half
--  the first (left) chord is reversed
--  in case the column count is odd the second (right) chord will have one more column than the first
splitChords :: [Chord] -> [(Chord, Chord)]
splitChords [] = []
splitChords (chord:s) = (Chord t (reverse cl), Chord t cr) : splitChords s
  where (t, n) = (time chord, notes chord)
        (cl, cr) = splitAt (length n `div` 2) n



--eliminate all Nothings and HoldNotes 
--separates the notes per column
--returns a matrix with the times of hits and releases
getColumnData :: [Chord] -> [[(Int, Note)]]
getColumnData chords = foldl (\arr chord -> let n = notes chord in zipWith (\x y -> x ++ checkNote y chord) arr n) (replicate (length $ notes $ head chords) []) chords
  where checkNote note chord = case note of
          Nothing -> []
          Just HoldNote -> []
          Just HitNote -> [(time chord, HitNote)]
          Just ReleaseNote -> [(time chord, ReleaseNote)]


--receives the time 
--             the column data
--returns an array with the last (time, note) of each column before the time
getLastNotes :: Int ->  [[(Int, Note)]] -> [(Int, Note)]
getLastNotes timeMs = map (ifNull . takeWhile (\x -> fst x < timeMs))
  where ifNull x = if null x then (timeMs, HitNote) else last x


--receives the chord to calculate the difficulty of
--         the column data
--         the argument data
--returns the difficulty per finger relationship
getChordDiff :: Chord -> [[(Int, Note)]] -> ArgumentData -> [Int] -> [[Double]]
getChordDiff chord colData args finger = map (map (`max` 0)) brute
  where brute = fst $ foldl (\(arr, i) x -> case x of
          Nothing -> (arr ++ [noteLessResult], i+1)
          Just n  -> (arr ++ [noteDiff i n]  , i+1)) ([], 0) notesArr
        (timeMs, notesArr) = (time chord, notes chord)
        lastNotes = getLastNotes timeMs colData
        noteLessResult = replicate (length notesArr) 0
        noteDiff i n = fst $ foldl (\(arr, ii) (pastNoteTime, nn) ->
          if timeMs == pastNoteTime then (arr ++ [0], ii + 1)
          else (arr ++ [applyDiffFunc (timeMs - pastNoteTime) (finger !! i,n) (finger !! ii,nn) args], ii + 1)) ([], 0) lastNotes

riceFunc :: Double -> Double -> Double -> Int -> Double
riceFunc a b c x = -x1 * x1 * a + x1 * b + c
  where x1 = fromIntegral x

jackFunc :: Double -> Double -> Double -> Int -> Double
jackFunc a b c x = (-a) * logBase b (c * x1)
  where x1 = fromIntegral x

--receives the distance
--             the col and type of the first note
--             the col and type of the second note
--             the argument data
--returns the calculated diff
applyDiffFunc :: Int -> (Int, Note) -> (Int, Note) -> ArgumentData -> Double
applyDiffFunc dist (col1, note1) (col2, note2) argData = func a1 a2 a3 dist
  where func = if col1 == col2 then jackFunc else riceFunc
        (a1,a2,a3) = case (note1, note2) of
          (HitNote, HitNote)          -> hithit argData !! col1 !! col2
          (HitNote, ReleaseNote)      -> hitrel argData !! col1 !! col2
          (ReleaseNote, HitNote)      -> relhit argData !! col1 !! col2
          (ReleaseNote, ReleaseNote)  -> relrel argData !! col1 !! col2


--receives chord 
--         col 
--         argumentData
--returns the hold diff of the selected column
applyHoldDiffFunc :: Chord -> Int -> ArgumentData -> [Double]
applyHoldDiffFunc chord i args | i >= length (notes chord) = replicate (length (notes chord)) 0
                               | otherwise  = fst $ foldl (\(arr, ii) note ->
                                  if note == Just HoldNote
                                    then (arr ++ [hArgs !! ii], ii+1)
                                    else (arr, ii+1)) ([], 0
                                ) otherCols
  where otherCols = addToIndex (notes chord) Nothing i
        hArgs = hold args !! i



--TODO testar excessoes



