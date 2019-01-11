module Main where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.ByteString as B
import Data.Word
import Data.Ord
import qualified Data.Set as S

main :: IO ()
main = do
  targetBytes <- B.readFile "Target.txt"
  let unused = [45..64]
  print unused
  print $ B.pack unused
  let m = go [B.map (\w -> if w == 0x0a then 46 else w) targetBytes, B.singleton 0x0a] $ drop 2 unused
  print m
  print $ length $ show m
  print $ length m
  mapM_ print $ zip (map B.singleton unused) m

saving :: (B.ByteString, [a]) -> Int
saving (bs, ips) = (B.length bs - 1) * (length ips - 1)

go :: [B.ByteString] -> [Word8] -> [B.ByteString]
go dictEntries [] = dictEntries
go dictEntries (w:ws) =
  let (bs, ips)
        = maximumBy (comparing saving)
        $ M.toList $ M.fromListWith (++)
        [ (bs, [(i, pos)])
        | (i, dictEntry) <- zip [0..] dictEntries
        , len <- [2 .. B.length dictEntry]
        , pos <- [0 .. B.length dictEntry - len]
        , let bs = B.take len $ B.drop pos dictEntry
        ]
      dictEntries' = map (replaceAll bs w) dictEntries ++ [bs]
  in if saving (bs, ips) < 5 then dictEntries else go dictEntries' ws

replaceAll :: B.ByteString -> Word8 -> B.ByteString -> B.ByteString
replaceAll bs w = replaceLoop
  where
    replaceLoop entry
        | B.null bsAfter = entry
        | otherwise      = replaceLoop $ before <> B.singleton w <> B.drop (B.length bs) bsAfter
      where
      (before, bsAfter) = B.breakSubstring bs entry
