module Main where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.ByteString as B
import Data.Word
import Data.Ord

main :: IO ()
main = do
  targetBytes <- B.readFile "Target.txt"
  print $ B.intercalate (B.singleton 124) $ go [targetBytes] [46..64]

saving :: (B.ByteString, [a]) -> Int
saving (bs, ips) = literalLength bs * (length ips - 1)

literalLength :: B.ByteString -> Int
literalLength bs = B.length bs + B.length (B.filter (== 0x0a) bs)

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
  in if saving (bs, ips) < 10 then dictEntries else go dictEntries' ws

replaceAll :: B.ByteString -> Word8 -> B.ByteString -> B.ByteString
replaceAll bs w = replaceLoop
  where
    replaceLoop entry
        | B.null bsAfter = entry
        | otherwise      = replaceLoop $ before <> B.singleton w <> B.drop (B.length bs) bsAfter
      where
      (before, bsAfter) = B.breakSubstring bs entry
